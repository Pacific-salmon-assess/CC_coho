###########################################################################
###     Hierarchical SR models w/time variant alphas, PR priors        ####
###########################################################################

#setwd("C:/data/centralcoast")
require(rjags)
require(R2jags)
library(runjags)
library(here)
library(bayestestR) #for hdi()

thinning <- 200
samples <- 1500
n_chains <- 4

SR.dat<-read.table(here("Data/Coho_Brood_MASTER.txt"), header=T)
SR.dat<-subset(SR.dat,SR.dat$year<2017)

SR.dat$logRS1<-log(SR.dat$RS_E)
SR.dat$logRS2<-log(SR.dat$RS_2)
SR.dat$logRS3<-log(SR.dat$RS_3)

SR.dat$er_est <- SR.dat$er_2
SR.dat$er_est[is.na(SR.dat$er_est)] <- SR.dat$er_E[is.na(SR.dat$er_est)]

n.pops<-max(SR.dat$pop_no)
n.years<-length(seq(1980,2016,1))
spawn<-matrix(nrow=n.years,ncol=n.pops,NA)

rec1<-matrix(nrow=n.years,ncol=n.pops,NA)
rec2<-matrix(nrow=n.years,ncol=n.pops,NA)
rec3<-matrix(nrow=n.years,ncol=n.pops,NA)

logRS1<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS2<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS3<-matrix(nrow=n.years,ncol=n.pops,NA)
logRSo<-matrix(nrow=n.years,ncol=n.pops,NA)
er_est <- matrix(nrow=n.years,ncol=n.pops,NA)

for(i in 1:n.pops){
  dat<-subset(SR.dat,SR.dat$pop_no==i)
  spawn[,i]<-dat[,"escapement"]
  rec1[,i]<-dat[,"rec_E"]
  rec2[,i]<-dat[,"rec_2"]
  rec3[,i]<-dat[,"rec_3"]
  logRS1[,i]<-dat[,"logRS1"]
  logRS2[,i]<-dat[,"logRS2"]
  logRS3[,i]<-dat[,"logRS3"]
  logRSo[,i]<-ifelse(!is.na(dat[,"logRS2"]),dat[,"logRS2"],dat[,"logRS1"])
  er_est[,i]<-dat$er_est
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


#################################################
##### Model 3 - hierarchical model w/time-variant (recursive) alphas and uninformative priors
###   with regional hyper priors mu.alpha which is also time varying, estimating overall change in productivity 
###   for each population group

####   The next step would be to add a covariance matrix between annual productivities

## I grouped rivers and smiths inlet with Area 7 & 8

co_pops<-read.table(here("Data/coho_groups.txt"),header=TRUE)
co_pops$mean_total<-NA

for(i in 1:52){
  dat<-subset(SR.dat,SR.dat$pop_no==i)
  co_pops[i,5]<-mean(na.omit(dat$total_runE))
}

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

Kgroups <- 6 # number of groups that we want to see correlated
covarGroups <- matrix(0,nrow=Kgroups,ncol=Kgroups)
diag(covarGroups) <- 1

group <- co_pops$group
Npops <- length(group)

datamcmc2 <- list(
  "logRS"=logRSo,
  "spawners"=spawn,
  "Smax.p"=Smax.p,
  "Smax.tau"=Smax.tau,
  "n.years"=n.years,
  "Kgroups"=Kgroups,
  "covarGroups"=covarGroups,
  "init_lalpha"=rep(1,Kgroups),
  "group"=group,
  "Npops"=Npops,
  "init_s0"=colMeans(spawn,na.rm=TRUE)
)
sigma_sd <- apply(logRSo,2,sd,na.rm=TRUE)
spawn_sd <- apply(log(spawn),2,sd,na.rm=TRUE)
variables <- c("Smax","beta","mu_alpha","alpha","sigmaGroups")#,"beta","tau.alpha","sigma.alpha","tau","sigma") # these are the variables to keep track of
init_fx <- function(chain_id)
{
  list("Smax"=Smax.p,
       "tauMVN"=covarGroups,
       "sigma_alpha"=rep(5,Kgroups),
       "mu_lalpha"=matrix(0,nrow=n.years,ncol=Kgroups),
       "sigma_s0"=spawn_sd,
       "sigma"=sigma_sd)
}

variables <- c("spawners","beta","mu_lalpha","lalpha","ln_alpha.mu")#,"beta","tau.alpha","sigma.alpha","tau","sigma") # these are the variables to keep track of

temp3B <- jags.model(file=here("JAGS/model_fitting tr1.jags"), data=datamcmc2 ,n.chains = n_chains, n.adapt=50000, quiet=FALSE,inits=init_fx)#, inits=inits)
update(temp3B, n.iter = 75000)  # burnin
resultSR_B3<-coda.samples(model=temp3B, variable.names=variables, n.iter=thinning*samples, thin = thinning) 

mcmc_names <- colnames(resultSR_B3[[1]])
plot(resultSR_B3[,grep("mu_lalpha",mcmc_names)[1:4]])
SR.results_B3<-summary(resultSR_B3)
#converg.test<-gelman.diag(resultSR_B3[,grep("spawners",mcmc_names)]) ##breaks
converg.test<-gelman.diag(resultSR_B3[,grep("mu_lalpha",mcmc_names)])
converg.test<-gelman.diag(resultSR_B3[,grep("lalpha",mcmc_names)])
converg.test<-gelman.diag(resultSR_B3[,grep("ln_alpha.mu",mcmc_names)])


neff_test<-coda::effectiveSize(resultSR_B3[,grep("spawners",mcmc_names)])
neff_test<-coda::effectiveSize(resultSR_B3[,grep("mu_lalpha",mcmc_names)])
neff_test<-coda::effectiveSize(resultSR_B3[,grep("lalpha",mcmc_names)])
neff_test<-coda::effectiveSize(resultSR_B3[,grep("ln_alpha.mu",mcmc_names)])

saveRDS(resultSR_B3,file="Results/COSR3B.tr1_lalpha_MCMC.rds")
resultSR_B3<-readRDS(here("Results/COSR3B.tr1_lalpha_MCMC.rds"))
mcmc_names <- colnames(resultSR_B3[[1]])
SR.results_B3<-summary(resultSR_B3)

##pulling out the parameters
spawners_impute<-SR.results_B3$quantiles[grep("spawners",mcmc_names),]
pop_alpha_mu<-SR.results_B3$quantiles[grep("ln_alpha.mu",mcmc_names),]
pop_betas3B<-SR.results_B3$quantiles[grep("beta",mcmc_names),]
pop_alphas3B<-SR.results_B3$quantiles[grep("\\blalpha",mcmc_names),]
mu_alphaHG<-SR.results_B3$quantiles[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,1]",mcmc_names),] # grab Group 1
mu_alphaNass<-SR.results_B3$quantiles[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,2]",mcmc_names),] # grab Group 2
mu_alphaSkeena<-SR.results_B3$quantiles[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,3]",mcmc_names),] # grab Group 3
mu_alphaHec<-SR.results_B3$quantiles[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,4]",mcmc_names),] # grab Group 4
mu_alphaNC<-SR.results_B3$quantiles[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,5]",mcmc_names),] # grab Group 5
mu_alphaCC<-SR.results_B3$quantiles[grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,6]",mcmc_names),] # grab Group 6

### pulling out the individual population productivity trends

a.pops_med<-matrix(data=NA, nrow=37,ncol=n.pops+1)
a.pops_med[,1]<-seq(1980,2016,1)

a.pops_lci<-matrix(data=NA, nrow=37,ncol=n.pops+1)
a.pops_lci[,1]<-seq(1980,2016,1)

a.pops_uci<-matrix(data=NA, nrow=37,ncol=n.pops+1)
a.pops_uci[,1]<-seq(1980,2016,1)

pop.start<-rep(NA,52)
pop.end<-rep(NA,52)

pop.start[1]<-1
pop.end[1]<-0+n.years

for (i in 2:52){
  pop.start[i]<-pop.end[i-1]+1
  pop.end[i]<-pop.end[i-1]+n.years
}

for (i in 1:n.pops){
  years<-1:n.years
  a.pops_med[years,i+1]<-pop_alphas3B[pop.start[i]:pop.end[i],3]
  a.pops_lci[years,i+1]<-pop_alphas3B[pop.start[i]:pop.end[i],1]
  a.pops_uci[years,i+1]<-pop_alphas3B[pop.start[i]:pop.end[i],5]
}
a.pops_med[,-1][which(is.na(logRSo),arr.ind = TRUE)] <- NA
a.pops_lci[,-1][which(is.na(logRSo),arr.ind = TRUE)] <- NA
a.pops_uci[,-1][which(is.na(logRSo),arr.ind = TRUE)] <- NA

#######################################################################
#### estimating Smsy using lamberts w Scheuerell method
require(gsl)
Smsy<-matrix(data=NA,nrow=samples,ncol=n.pops)

samp.chain<-sample(1:n_chains,samples,replace=TRUE)
samp.MCMC<-sample(1:samples, samples, replace=FALSE)

## just need to adjust the length based on how long the MCMC is
for (j in 1:n.pops){
  samp.chain<-sample(1:n_chains,samples,replace=TRUE)
  samp.MCMC<-sample(1:samples, samples, replace=FALSE)
  
  for (i in 1:samples){
    Smsy[i,j]<-(1 - lambert_W0(exp(1 - (resultSR_B3[[samp.chain[i]]][samp.MCMC[i],grep("ln_alpha.mu",mcmc_names)[j]])))) / (resultSR_B3[[samp.chain[i]]][samp.MCMC[i],grep("beta",mcmc_names)[j]]) #Smsy in 1000 random draws from MCMC
    
  }
}

## this is working

hdi(Smsy[,24])
hist(Smsy[,24])
median(Smsy[,26])
plot(exp(Smax.p),colMeans(Smsy),ylab="S(msy)",xlab="S(max)")
abline(b=1,a=0)
spawn.cur <- sapply(1:Npops,function(x){spawn.mat[yrs.dat[x],x]})
plot(colMeans(Smsy),spawn.cur/colMeans(Smsy),ylab="S(msy)",xlab="S(max)")
abline(b=1,a=0)
co_pops
### this seems to be working to produce reasonable 95% CIs from the MCMC 








############################################################################
####  visualizing the results
#######################################

### this is the region for each population
hg<-which(co_pops$group==1)
nass<-which(co_pops$group==2)
skeena<-which(co_pops$group==3)
hec_low<-which(co_pops$group==4)
nc_inner<-which(co_pops$group==5)
cc_south<-which(co_pops$group==6)

### plotting trend in NCC coho productivity 
years<-seq(1980,2016,1)
cols<-c("seagreen2","royalblue2","darkviolet","turquoise3","orchid3","chartreuse3")

### 95% CI

uci95cc<-mu_alphaCC[,5]
lci95cc<-mu_alphaCC[,1]

uci95hec<-mu_alphaHec[,5]
lci95hec<-mu_alphaHec[,1]

uci95hg<-mu_alphaHG[,5]
lci95hg<-mu_alphaHG[,1]

uci95nc<-mu_alphaNC[,5]
lci95nc<-mu_alphaNC[,1]

uci95sk<-mu_alphaSkeena[,5]
lci95sk<-mu_alphaSkeena[,1]

uci95nass<-mu_alphaNass[,5]
lci95nass<-mu_alphaNass[,1]

### 75% CI
uci75cc<-mu_alphaCC[,4]
lci75cc<-mu_alphaCC[,2]

uci75hec<-mu_alphaHec[,4]
lci75hec<-mu_alphaHec[,2]

uci75hg<-mu_alphaHG[,4]
lci75hg<-mu_alphaHG[,2]

uci75nc<-mu_alphaNC[,4]
lci75nc<-mu_alphaNC[,2]

uci75sk<-mu_alphaSkeena[,4]
lci75sk<-mu_alphaSkeena[,2]

uci75nass<-mu_alphaNass[,4]
lci75nass<-mu_alphaNass[,2]

100*(mean(exp(mu_alphaNass[(nrow(mu_alphaNass)-1):nrow(mu_alphaNass),3]))-mean(exp(mu_alphaNass[,3])))/(mean(exp(mu_alphaNass[,3])))
100*(mean(exp(mu_alphaSkeena[(nrow(mu_alphaSkeena)-1):nrow(mu_alphaSkeena),3]))-mean(exp(mu_alphaSkeena[,3])))/(mean(exp(mu_alphaSkeena[,3])))
100*(mean(exp(mu_alphaNC[(nrow(mu_alphaNC)-1):nrow(mu_alphaNC),3]))-mean(exp(mu_alphaNC[,3])))/(mean(exp(mu_alphaNC[,3])))
100*(mean(exp(mu_alphaHG[(nrow(mu_alphaHG)-1):nrow(mu_alphaHG),3]))-mean(exp(mu_alphaHG[,3])))/(mean(exp(mu_alphaHG[,3])))
100*(mean(exp(mu_alphaHec[(nrow(mu_alphaHec)-1):nrow(mu_alphaHec),3]))-mean(exp(mu_alphaHec[,3])))/(mean(exp(mu_alphaHec[,3])))
100*(mean(exp(mu_alphaCC[(nrow(mu_alphaCC)-1):nrow(mu_alphaCC),3]))-mean(exp(mu_alphaCC[,3])))/(mean(exp(mu_alphaCC[,3])))

x <- c(1980:2016, 2016:1980)

y1cc <- c(lci95cc,rev(uci95cc))
y2cc <- c(lci75cc,rev(uci75cc))

y1hec <- c(lci95hec,rev(uci95hec))
y2hec <- c(lci75hec,rev(uci75hec))

y1hg <- c(lci95hg,rev(uci95hg))
y2hg <- c(lci75hg,rev(uci75hg))

y1nc <- c(lci95nc,rev(uci95nc))
y2nc <- c(lci75nc,rev(uci75nc))

y1sk <- c(lci95sk,rev(uci95sk))
y2sk <- c(lci75sk,rev(uci75sk))

y1na <- c(lci95nass,rev(uci95nass))
y2na <- c(lci75nass,rev(uci75nass))

sizes<-rep(NA,4)
sizes[1]<-log(100)*0.2
sizes[2]<-log(1000)*0.2
sizes[3]<-log(10000)*0.2
sizes[4]<-log(50000)*0.2


y_range <- pmin(10,range(mu_alphaCC,mu_alphaHec,mu_alphaHG,mu_alphaNC,mu_alphaNass,mu_alphaSkeena,a.pops_med[,-1],na.rm=TRUE))
jpeg("Figures/CO_alpha_trend_pops_NCC.jpeg", width = 1800, height=1600, units="px", pointsize=12, res=200)

par(mfrow=c(2,3),mar=c(2,1,0,0),oma=c(3,4,1,1))

# Central Coast - Areas 7-10
plot(1,1,cex=0,axes=FALSE, xlab="",ylab="",xlim=c(1980,2016),ylim=y_range)
polygon(x, y1cc,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2cc,  col = "grey82", lty = 2, lwd = 2, border = NA)

text(2000,-0.5,"Area 7-10",font=2)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1)

for(i in 1:length(cc_south)){
  par(new=TRUE)
  plot(a.pops_med[,1],a.pops_med[,(cc_south[i]+1)],axes=FALSE,ylab="",xlab="",
       xlim=c(1980,2016),ylim=y_range,cex=0.2*log(co_pops[cc_south[i],5]))
  #par(new=TRUE)
  #plot(a.pops_med[,1],a.pops_med[,(cc_south[i]+1)],axes=FALSE,ylab="",xlab="",
  #     xlim=c(1980,2016),ylim=c(0,10),lwd=0.1*log(co_pops[cc_south[i],5]),type='l')
}

legend("topright",c("100", "1000","10000","50000"),pt.cex=sizes,pch=1,title="mean spawners")

lines(years,mu_alphaCC[,3],type="l", lwd=3, lty=1, col=cols[1])

mtext(expression("Survival index - Ricker ln" ~~ alpha ~ ""),side=2,line=2.8, adj=-1.2,font=2,cex=1.2)


# Hecate Lowlands - Areas 5-6
plot(1,1,cex=0,axes=FALSE, xlab="",ylab="",xlim=c(1980,2016),ylim=y_range)
polygon(x, y1hec,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2hec,  col = "grey82", lty = 2, lwd = 2, border = NA)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1,labels=FALSE)

for(i in 1:length(hec_low)){
  par(new=TRUE)
  plot(a.pops_med[,1],a.pops_med[,(hec_low[i]+1)],axes=FALSE,ylab="",xlab="",
       xlim=c(1980,2016),ylim=y_range,cex=0.2*log(co_pops[hec_low[i],5]))
}

lines(years,mu_alphaHec[,3],type="l", lwd=3, lty=1, col=cols[2])

text(2000,-0.5,"Hecate Lowlands - Area 5/6",font=2)

# Area 6 - Inner Waters
plot(1,1,cex=0,axes=FALSE, xlab="",ylab="",xlim=c(1980,2016),ylim=y_range)
polygon(x, y1nc,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2nc,  col = "grey82", lty = 2, lwd = 2, border = NA)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1,labels=FALSE)

for(i in 1:length(nc_inner)){
  par(new=TRUE)
  plot(a.pops_med[,1],a.pops_med[,(nc_inner[i]+1)],axes=FALSE,ylab="",xlab="",
       xlim=c(1980,2016),ylim=y_range,cex=0.2*log(co_pops[nc_inner[i],5]))
}

lines(years,mu_alphaNC[,3],type="l", lwd=3, lty=1, col=cols[3])

text(2000,-0.5,"Area 6 - Inner Waters",font=2)

# Haida Gwaii - Area 2E
plot(1,1,cex=0,axes=FALSE, xlab="",ylab="",xlim=c(1980,2016),ylim=y_range)
polygon(x, y1hg,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2hg,  col = "grey82", lty = 2, lwd = 2, border = NA)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1)

for(i in 1:length(hg)){
  par(new=TRUE)
  plot(a.pops_med[,1],a.pops_med[,(hg[i]+1)],axes=FALSE,ylab="",xlab="",
       xlim=c(1980,2016),ylim=y_range,cex=0.2*log(co_pops[hg[i],5]))
}

lines(years,mu_alphaHG[,3],type="l", lwd=3, lty=1, col=cols[4])
text(2000,-0.5,"Haida Gwaii - Area 2E",font=2)

# Skeena
plot(1,1,cex=0,axes=FALSE, xlab="",ylab="",xlim=c(1980,2016),ylim=y_range)
polygon(x, y1sk,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2sk,  col = "grey82", lty = 2, lwd = 2, border = NA)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1,labels=FALSE)

for(i in 1:length(skeena)){
  par(new=TRUE)
  plot(a.pops_med[,1],a.pops_med[,(skeena[i]+1)],axes=FALSE,ylab="",xlab="",
       xlim=c(1980,2016),ylim=y_range,cex=0.2*log(co_pops[skeena[i],5]))
}

lines(years,mu_alphaSkeena[,3],type="l", lwd=3, lty=1, col=cols[5])
text(2000,-0.5,"Skeena - Area 4",font=2)

# Nass
plot(1,1,cex=0,axes=FALSE, xlab="",ylab="",xlim=c(1980,2016),ylim=y_range)
polygon(x, y1na,  col = "grey90", lty = 2, lwd = 2, border = NA)
polygon(x, y2na,  col = "grey82", lty = 2, lwd = 2, border = NA)

axis(1,labels=TRUE,at=c(1980,1985,1990,1995,2000,2005,2010,2015))
axis(2,las=1,labels=FALSE)

for(i in 1:length(nass)){
  par(new=TRUE)
  plot(a.pops_med[,1],a.pops_med[,(nass[i]+1)],axes=FALSE,ylab="",xlab="",
       xlim=c(1980,2016),ylim=y_range,cex=0.2*log(co_pops[nass[i],5]))
}

lines(years,mu_alphaNass[,3],type="l", lwd=3, lty=1, col=cols[6])
text(2000,-0.5,"Nass - Area 3",font=2)


dev.off()
