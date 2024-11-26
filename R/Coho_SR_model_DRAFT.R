###########################################################################
###     Hierarchical SR models w/time variant alphas, PR priors        ####
###########################################################################

#setwd("C:/data/centralcoast")
require(rjags)
require(R2jags)
library(here)

SR.dat<-read.table(here("Data/Coho_Brood_MASTER.txt"), header=T)
SR.dat<-subset(SR.dat,SR.dat$year<2017)

SR.dat$logRS1<-log(SR.dat$RS_E)
SR.dat$logRS2<-log(SR.dat$RS_2)
SR.dat$logRS3<-log(SR.dat$RS_3)

n.pops<-max(SR.dat$pop_no)
n.years<-length(seq(1980,2016,1))
spawn<-matrix(nrow=n.years,ncol=n.pops,NA)

rec1<-matrix(nrow=n.years,ncol=n.pops,NA)
rec2<-matrix(nrow=n.years,ncol=n.pops,NA)
rec3<-matrix(nrow=n.years,ncol=n.pops,NA)

logRS1<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS2<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS3<-matrix(nrow=n.years,ncol=n.pops,NA)

for(i in 1:n.pops){
  dat<-subset(SR.dat,SR.dat$pop_no==i)
  spawn[,i]<-dat[,6]
  rec1[,i]<-dat[,16]
  rec2[,i]<-dat[,17]
  rec3[,i]<-dat[,18]
  logRS1[,i]<-dat[,19]
  logRS2[,i]<-dat[,20]
  logRS3[,i]<-dat[,21]
}

yrs<-matrix(nrow=n.years,ncol=n.pops,NA) 

for(i in 1:n.pops){
  yrs[1:length(which(spawn[,i]>0)),i]<-which(spawn[,i]>0)
}

#### making matrixes JAGS to index
spawn.mat<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS1.mat<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS2.mat<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS3.mat<-matrix(nrow=n.years,ncol=n.pops,NA)
logRSB.mat<-matrix(nrow=n.years,ncol=n.pops,NA)


for(i in 1:n.pops){
  spawn.mat[1:length(na.omit(yrs[,i])),i]<-spawn[na.omit(yrs[,i]),i]
}

for(i in 1:n.pops){
  logRS1.mat[1:length(na.omit(yrs[,i])),i]<-logRS1[na.omit(yrs[,i]),i]
  logRS2.mat[1:length(na.omit(yrs[,i])),i]<-logRS2[na.omit(yrs[,i]),i]
  logRS3.mat[1:length(na.omit(yrs[,i])),i]<-logRS3[na.omit(yrs[,i]),i]
}

pops_E<-unique(SR.dat[which(SR.dat$stat_area<5),1])
pops_2<-seq(1,52,1)
pops_2<-pops_2[-c(pops_E)]

for(i in pops_2){
  logRSB.mat[1:length(na.omit(yrs[,i])),i]<-logRS2[na.omit(yrs[,i]),i]

}

for(i in pops_E){
  logRSB.mat[1:length(na.omit(yrs[,i])),i]<-logRS1[na.omit(yrs[,i]),i]
  
}

## Will eventually need to subset out populations where we want to analyze RS2 instead of RS1

yrs.dat<-rep(NA,n.pops)
for (i in 1:n.pops){
  yrs.dat[i]<-length(na.omit(yrs[,i]))
}

## excluding NAs (they seem to give the model problems) 
SR.dat<-na.omit(SR.dat)

#spawn<-read.table("SR_short_S.txt",header=T)
#logRS<-read.table("SR_short_logRS.txt",header=T)

#######################################################################
#########################################################################
##### Model 1 - hierarchical model w/time-variant (recursive) alphas and uninformative priors
###   but hyper prior mu.alpha is constant. Next try fitting a model where the hyper prior is time varying 

## Stopped here March 24, 2021
###  Next step will be to estimate hyperdistribution for each CU, and to fit a model 
####  where we use CWT ER estimates for Areas 2-4 and logRS2 values for Areas 5-10. 



### using mean run size (harvest scenario 1) across the timeseries as our prior on capacity
Smax.p<-rep(NA,52)
Smax.tau<-rep(NA,52)
n.pops<-max(SR.dat$pop_no)

for(i in 1:n.pops){
  Smax.p[i]<-log(mean(na.omit(rec1[,i])))
  Smax.tau[i] <- 0.2*Smax.p[i] #scales var to Smax by multiplying log abundance by 0.2 for our SD
}

sink("model.SR")
cat("
model{

    ## Stock-recruit component

for (i in 1:52){
    for(t in 1:yrs.dat[i]) {
        logRS[t,i] ~ dnorm(mu.y[t,i], tau)
        mu.y[t,i] <- alpha[t,i] - beta[i] * spawners[t,i] ## time & population specific alpha
    }

# hyper-prior on alpha at t=1 for pops 1:52
	alpha[1,i] ~ dnorm(mu.alpha, tau.alpha)

### priors for beta in 1:52 populations
	beta[i] <-  1/Smax[i]
	Smax[i] ~ dlnorm(Smax.p[i], 1/((Smax.tau[i])^2))

## Recursive alpha
    for(t in 2:yrs.dat[i]){
	  alpha[t,i] ~ dnorm(alpha[t-1,i], tau.a1)
	}

  alpha.mu[i]<-mean(alpha[1:yrs.dat[i],i])
  #Smsy[i]<-Smax[i]*(0.5-0.07*exp(alpha.mu[i]))

}

    ## the priors
    mu.alpha ~ dunif(0,10)

    tau.alpha <- pow(sigma.alpha, -2)
    sigma.alpha ~ dunif(0, 25)

    tau.a1 <- pow(sigma.a1, -2)
    sigma.a1 ~ dunif(0, 25)

    tau <- pow(sigma, -2)
    sigma ~ dunif(0, 25)

}" ,fill = TRUE)
sink()

inits <- list("alpha"=1,"mu.alpha"=1,"beta"=0.01,"tau.alpha"=10,"sigma.alpha"=1,"tau"=0.01,"sigma"=1)    

datamcmc2 <- list(
  "logRS"=logRS1.mat,
  "spawners"=spawn.mat,
  "yrs.dat"=yrs.dat,
  "Smax.p"=Smax.p,
  "Smax.tau"=Smax.tau
  #	    "n.pops"=n.pops
)

variables <- c("Smax","alpha","alpha.mu","mu.alpha","beta","tau.alpha","tau.a1")#"sigma.alpha","tau","sigma") # these are the variables to keep track of

temp1 <- jags.model(file="model.SR", data=datamcmc2 ,
                   n.chains = 3, n.adapt=1000, quiet=FALSE)#, inits=inits)
update(temp1, n.iter = 10000)  # burnin
resultSR_E1<-coda.samples(model=temp1, variable.names=variables , n.iter=30000,n.thin=5) 
SR.results_E1<-summary(resultSR_E1)

plot(resultSR_E1[,1464:1515])

Smax_est1<-SR.results_E1$quantiles[1:52,]
pop_alphas1<-SR.results_E1$quantiles[53:1463,]
mean_pop_alphas1<-SR.results_E1$quantiles[1464:1515,]
pop_beta1<-SR.results_E1$quantiles[1516:1567,]

saveRDS(SR.results_E1,file="COSR1_MCMC.rds")



SR.resultsA$quantiles[1500:1568,] #wtf should this be?

#######################################################################
#########################################################################
##### Model 2.1 - hierarchical model w/time-variant (recursive) alphas and uninformative priors
###   but hyper prior mu.alpha is also time varying, estimating overall change in productivity
###   using English et al. ER estimates

### using mean run size (harvest scenario 1) across the timeseries as our prior on capacity
Smax.p<-rep(NA,52)
Smax.tau<-rep(NA,52)
n.pops<-max(SR.dat$pop_no)

for(i in 1:n.pops){
  Smax.p[i]<-log(mean(na.omit(rec1[,i])))
  Smax.tau[i] <- 0.2*Smax.p[i] #scales var to Smax by multiplying log abundance by 0.2 for our SD
}


n.years<-length(spawn.mat[,1])

sink("model.SR")
cat("
    model{
    
    ## Stock-recruit component
    
  for (i in 1:52){
    for(t in 1:yrs.dat[i]) {
      logRS[t,i] ~ dnorm(mu.y[t,i], tau)
      mu.y[t,i] <- alpha[t,i] - beta[i] * spawners[t,i]
      alpha[t,i] ~ dnorm(mu.alpha[t], tau.alpha)
    
    }
    
    alpha.mu[i]<-mean(alpha[1:yrs.dat[i],i])

    # priors for beta 
	  beta[i] <-  1/Smax[i]
	  Smax[i] ~ dlnorm(Smax.p[i], 1/((Smax.tau[i])^2))
    
    }


  ## Time variant mu.alpha
    for(z in 2:n.years){
    mu.alpha[z] ~ dnorm(mu.alpha[z-1], tau.mu)
    }
    
    # fixed prior for mu.alpha_low at time = 1
    mu.alpha[1] ~ dunif(0,10) 
    
    ## the priors
    
    tau.alpha<- pow(sigma.alpha, -2)
    sigma.alpha ~ dunif(0, 25)

    tau.mu <- pow(sigma.mu, -2)
    sigma.mu ~ dunif(0,25)
    
    tau <- pow(sigma, -2)
    sigma ~ dunif(0, 25)
    
    
    }" ,fill = TRUE)
sink()

inits <- list("alpha"=1,"mu.alpha"=1,"beta"=0.01,"tau.alpha"=10,"sigma.alpha"=1,
              "tau"=0.01,"sigma"=1)    

datamcmc2 <- list(
  "logRS"=logRS1.mat,
  "spawners"=spawn.mat,
  "yrs.dat"=yrs.dat,
  "Smax.p"=Smax.p,
  "Smax.tau"=Smax.tau,
  "n.years"=n.years
)


variables <- c("Smax","mu.alpha","alpha","alpha.mu","beta","tau.alpha","tau") # these are the variables to keep track of

temp2 <- jags.model(file="model.SR", data=datamcmc2 ,
                     n.chains = 3, n.adapt=1000, quiet=FALSE)#, inits=inits)
update(temp2, n.iter = 50000)  # burnin
resultSR_E2<-coda.samples(model=temp2, variable.names=variables , n.iter=60000, n.thin=20) 

plot(resultSR_E2[,1516:1526])
SR.results_E2<-summary(resultSR_E2)

saveRDS(resultSR_E2,file="COSR2_MCMC.rds")


SR.results_E2$quantiles

Smax_est2<-SR.results_E2$quantiles[1:52,]
pop_alphas2<-SR.results_E2$quantiles[53:1463,]
mean_alphas2<-SR.results_E2$quantiles[1464:(1464+51),]
pop_betas2<-SR.results_E2$quantiles[1516:1567,]
mu_alpha2<-SR.results_E2$quantiles[1568:(1568+36),] ## time-varying hierachical mean alpha


### plotting trend in NCC coho productivity 
years<-seq(1980,2016,1)
cols<-c("seagreen2","royalblue2","darkviolet","turquoise3","orchid3","chartreuse3")

uci95_E<-mu_alpha2[,5]
lci95_E<-mu_alpha2[,1]

uci75_E<-mu_alpha2[,4]
lci75_E<-mu_alpha2[,2]

x <- c(1980:2016, 2016:1980)
y1_E <- c(lci95_E,rev(uci95_E))
y2_E <- c(lci75_E,rev(uci75_E))


plot(1,1,cex=0,axes=FALSE, xlab="",ylab="log(alpha)",xlim=c(1980,2016),ylim=c(0,6))
polygon(x, y1_E,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2_E,  col = "grey82", lty = 2, lwd = 2, border = NA)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1)

lines(years,mu_alpha2[,3],type="l", lwd=2, lty=1, col=cols[2])

lines(years,mu_alpha2B[,3],type="l", lwd=2, lty=1, col=cols[3])

gc()



#######################################################################
#########################################################################
##### Model 2.1 - hierarchical model w/time-variant (recursive) alphas and uninformative priors
###   but hyper prior mu.alpha is also time varying, estimating overall change in productivity
###   using adapted ER estimates

### using mean run size (harvest scenario 1) across the timeseries as our prior on capacity
Smax.p<-rep(NA,52)
Smax.tau<-rep(NA,52)
n.pops<-max(SR.dat$pop_no)

for(i in 1:n.pops){
  Smax.p[i]<-log(mean(na.omit(rec1[,i])))
  Smax.tau[i] <- 0.2*Smax.p[i] #scales var to Smax by multiplying log abundance by 0.2 for our SD
}

n.years<-length(spawn.mat[,1])

sink("model.SR")
cat("
    model{
    
    ## Stock-recruit component
    
  for (i in 1:52){
    for(t in 1:yrs.dat[i]) {
      logRS[t,i] ~ dnorm(mu.y[t,i], tau)
      mu.y[t,i] <- alpha[t,i] - beta[i] * spawners[t,i]
      alpha[t,i] ~ dnorm(mu.alpha[t], tau.alpha)
    
    }
    
    alpha.mu[i]<-mean(alpha[1:yrs.dat[i],i])

    # priors for beta 
	  beta[i] <-  1/Smax[i]
	  Smax[i] ~ dlnorm(Smax.p[i], 1/((Smax.tau[i])^2))
    
    }


  ## Time variant mu.alpha
    for(z in 2:n.years){
    mu.alpha[z] ~ dnorm(mu.alpha[z-1], tau.mu)
    }
    
    # fixed prior for mu.alpha_low at time = 1
    mu.alpha[1] ~ dunif(0,10) 
    
    ## the priors
    
    tau.alpha<- pow(sigma.alpha, -2)
    sigma.alpha ~ dunif(0, 25)

    tau.mu <- pow(sigma.mu, -2)
    sigma.mu ~ dunif(0,25)
    
    tau <- pow(sigma, -2)
    sigma ~ dunif(0, 25)
    
    
    }" ,fill = TRUE)
sink()

inits <- list("alpha"=1,"mu.alpha"=1,"beta"=0.01,"tau.alpha"=10,"sigma.alpha"=1,
              "tau"=0.01,"sigma"=1)    

datamcmc2 <- list(
  "logRS"=logRSB.mat,
  "spawners"=spawn.mat,
  "yrs.dat"=yrs.dat,
  "Smax.p"=Smax.p,
  "Smax.tau"=Smax.tau,
  "n.years"=n.years
)


variables <- c("Smax","mu.alpha","alpha","alpha.mu","beta","tau.alpha","tau") # these are the variables to keep track of

temp2B <- jags.model(file="model.SR", data=datamcmc2 ,
                    n.chains = 3, n.adapt=1000, quiet=FALSE)#, inits=inits)
update(temp2B, n.iter = 50000)  # burnin
resultSR_B2<-coda.samples(model=temp2B, variable.names=variables , n.iter=60000, n.thin=20) 

plot(resultSR_B2[,100:102])
SR.results_B2<-summary(resultSR_B2)

saveRDS(resultSR_B2,file="COSR2B_MCMC.rds")

Smax_est2B<-SR.results_B2$quantiles[1:52,]
pop_alphas2B<-SR.results_B2$quantiles[53:1463,]
mean_alphas2B<-SR.results_B2$quantiles[1464:(1464+51),]
pop_betas2B<-SR.results_B2$quantiles[1516:1567,]
mu_alpha2B<-SR.results_B2$quantiles[1568:(1568+36),] ## time-varying hierachical mean alpha


#################################################
##### Model 3 - hierarchical model w/time-variant (recursive) alphas and uninformative priors
###   with regional hyper priors mu.alpha which is also time varying, estimating overall change in productivity 
###   for each population group

####   The next step would be to add a covariance matrix between annual productivities

## I grouped rivers and smiths inlet with Area 7 & 8

co_pops<-read.table(here("Data/coho_groups.txt"),header=TRUE)

### lumping Rivers Smith Inlet with Area 7-8
co_pops[which(co_pops$group==7),4]<-6

### using mean run size (harvest scenario 1) across the timeseries as our prior on capacity
Smax.p<-rep(NA,52)
Smax.tau<-rep(NA,52)
n.pops<-max(SR.dat$pop_no)

for(i in 1:n.pops){
  Smax.p[i]<-log(mean(na.omit(rec1[,i])))
  Smax.tau[i] <- 0.2*Smax.p[i] #scales var to Smax by multiplying log abundance by 0.2 for our SD
}


### this is the region for each population
hg<-which(co_pops$group==1)
nass<-which(co_pops$group==2)
skeena<-which(co_pops$group==3)
hec_low<-which(co_pops$group==4)
nc_inner<-which(co_pops$group==5)
cc_south<-which(co_pops$group==6)
#a78<-which(co_pops$group==6)
#a910<-which(co_pops$group==7)

Kgroups <- 6 # number of groups that we want to see correlated
covarGroups <- matrix(0,nrow=Kgroups,ncol=Kgroups)
diag(covarGroups) <- 1

group <- co_pops$group
Npops <- length(group)
sink("model.SR")
cat("
    model{
    
    ## Stock-recruit component
    for(i in 1:Npops){
      beta[i] <- 1/Smax[i]
      Smax[i] ~ dlnorm(Smax.p[i],1/(Smax.tau[i]^2))
        for(t in 1:yrs.dat[i]) {
          logRS[t,i] ~ dnorm(mu.y[t,i], tau)
          mu.y[t,i] <- lalpha[t,i] - beta[i] * spawners[t,i]
          lalpha[t,i] ~ dnorm(mu_lalpha[t,group[i]], tau.alpha[group[i]])
        }
    }
    for(k in 1:Kgroups)
    {
    tau.alpha[k] <- pow(sigma_alpha[k],-2)
    sigma_alpha[k] ~ dunif(0,25)
    }
    
    mu_lalpha[1,1:Kgroups] ~ dmnorm(init_lalpha,tauMVN) # initialize the log-alpha 6x6 matrix
    for(z in 2:n.years)
    {
    mu_lalpha[z,1:Kgroups] ~ dmnorm(mu_lalpha[z-1,1:Kgroups],tauMVN) # make log-alpha recursive, and correlated to each group
    #mu_alpha[z,1:Kgroups] <- exp(mu_lalpha[z,1:Kgroups]) # global median alphas for each of the 6 groups
    }
    tauMVN ~ dwish(covarGroups,Kgroups+1)
    sigmaGroups <- inverse(tauMVN)
    tau <- pow(sigma, -2)
    sigma ~ dunif(0, 25)
    
    }" ,fill = TRUE)
sink()

inits <- list("mu_alpha"=rep(1,Kgroups),"beta"=0.01,"tau.alpha"=10,"sigma.alpha"=1,
              "tau"=0.01,"sigma"=1)    

datamcmc2 <- list(
  "logRS"=logRSB.mat,
  "spawners"=spawn.mat,
  "yrs.dat"=yrs.dat,
  "Smax.p"=Smax.p,
  "Smax.tau"=Smax.tau,
  "n.years"=n.years,
  "Kgroups"=Kgroups,
  "covarGroups"=covarGroups,
  "init_lalpha"=rep(1,Kgroups),
  "group"=group,
  "Npops"=Npops
)


variables <- c("Smax","beta","mu_lalpha","lalpha","sigmaGroups")#,"beta","tau.alpha","sigma.alpha","tau","sigma") # these are the variables to keep track of

temp3B <- jags.model(file="model.SR", data=datamcmc2 ,
                     n.chains = 3, n.adapt=1000, quiet=FALSE)#, inits=inits)
update(temp3B, n.iter = 60000)  # burnin
resultSR_B3<-coda.samples(model=temp3B, variable.names=variables, n.iter=50000, n.thin=5) 

saveRDS(resultSR_B3,file="COSR3B.2_MCMC.rds")

SR.results_B3<-summary(resultSR_B3)

SR.results_B3$quantiles[1516:1690,]

plot(resultSR_B3[,1490:1493])

Smax_est3B<-SR.results_B3$quantiles[1:52,]
pop_alphas3B<-SR.results_B3$quantiles[53:1463,]
pop_betas3B<-SR.results_B3$quantiles[1464:1515,]
mean_alphasCC<-SR.results_B3$quantiles[1516:(1516+36),]
mean_alphasHec<-SR.results_B3$quantiles[1553:(1553+36),]
mean_alphasHG<-SR.results_B3$quantiles[1590:(1590+36),]
mean_alphasNass<-SR.results_B3$quantiles[1627:(1627+36),]
mean_alphasNC<-SR.results_B3$quantiles[1664:(1664+36),]
mean_alphasRivSm<-SR.results_B3$quantiles[1701:(1701+36),]
mean_alphasSkeena<-SR.results_B3$quantiles[1738:(1738+36),]

### plotting trend in NCC coho productivity 
years<-seq(1980,2016,1)
cols<-c("seagreen2","royalblue2","darkviolet","turquoise3","orchid3","chartreuse3")

uci95cc<-mean_alphasCC[,5]
lci95cc<-mean_alphasCC[,1]

uci95hec<-mean_alphasHec[,5]
lci95hec<-mean_alphasHec[,1]

uci95rs<-mean_alphasRivSm[,5]
lci95rs<-mean_alphasRivSm[,1]

uci75cc<-mean_alphasCC[,4]
lci75cc<-mean_alphasCC[,2]

uci75hec<-mean_alphasHec[,4]
lci75hec<-mean_alphasHec[,2]

uci75rs<-mean_alphasRivSm[,4]
lci75rs<-mean_alphasRivSm[,2]

x <- c(1980:2016, 2016:1980)

y1cc <- c(lci95cc,rev(uci95cc))
y2cc <- c(lci75cc,rev(uci75cc))

y1hec <- c(lci95hec,rev(uci95hec))
y2hec <- c(lci75hec,rev(uci75hec))

y1rs <- c(lci95rs,rev(uci95rs))
y2rs <- c(lci75rs,rev(uci75rs))


par(mfrow=c(1,3))

plot(1,1,cex=0,axes=FALSE, xlab="",ylab="log(alpha)",xlim=c(1980,2016),ylim=c(0,6))
polygon(x, y1cc,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2cc,  col = "grey82", lty = 2, lwd = 2, border = NA)

text(2000,5.5,"Area 7 & 8")

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1)

lines(years,mean_alphasCC[,3],type="l", lwd=2, lty=1, col=cols[2])

plot(1,1,cex=0,axes=FALSE, xlab="",ylab="log(alpha)",xlim=c(1980,2016),ylim=c(0,6))
polygon(x, y1hec,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2hec,  col = "grey82", lty = 2, lwd = 2, border = NA)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1)

lines(years,mean_alphasHec[,3],type="l", lwd=2, lty=1, col=cols[3])

text(2000,5.5,"Hecate Lowlands - Area 5/6")

plot(1,1,cex=0,axes=FALSE, xlab="",ylab="log(alpha)",xlim=c(1980,2016),ylim=c(0,6))
polygon(x, y1rs,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2rs,  col = "grey82", lty = 2, lwd = 2, border = NA)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1)

lines(years,mean_alphasRivSm[,3],type="l", lwd=2, lty=1, col=cols[4])
text(2000,5.5,"Rivers/Smiths Inlet")

# I would like to plot the individual trends on here, but will need to figure 
##  out how to write the code to extract the individual population alpha estimates 
### and match them to their year

### rerun these analysis today with Rivers and Smiths Inlet grouped with Areas 7/8

### export these tables when I get back from dinner or tomorrow morning 



