SR.dat<-read.table("Data/Coho_Brood_MASTER.txt", header=T)
SR.dat_full <- SR.dat
SR.dat_predict<-subset(SR.dat,SR.dat$year>=2017)
SR.dat<-subset(SR.dat,SR.dat$year<2017)

SR.dat$logRS1<-log(SR.dat$RS_E)
SR.dat$logRS2<-log(SR.dat$RS_2)
SR.dat$logRS3<-log(SR.dat$RS_3)

SR.dat$recruits <-ifelse(!is.na(SR.dat$RS_E),SR.dat$RS_2*SR.dat$escapement,SR.dat$RS_E*SR.dat$escapement)

n.years<-length(seq(1980,2016,1))

co_pops<-read.table("Data/coho_groups.txt",header=TRUE)
co_pops$mean_total<-NA

for(i in 1:52){
  dat<-subset(SR.dat,SR.dat$pop_no==i)
  co_pops[i,5]<-mean(na.omit(dat$total_runE))
}

### lumping Rivers Smith Inlet with Area 7-8
co_pops[which(co_pops$group==7),4]<-6
n.pops<-max(SR.dat$pop_no)
group <- co_pops$group
Npops <- length(group)
Kgroups <- 6 # number of groups that we want to see correlated
covarGroups <- matrix(0,nrow=Kgroups,ncol=Kgroups)
diag(covarGroups) <- 1

resultSR_B3<-readRDS("Results/COSR3B.tr1_lalpha_MCMC.rds")
mcmc_names <- colnames(resultSR_B3[[1]])
samples <- dim(resultSR_B3[[1]])[1]
n_chains <- 4
#######################################################################
#### estimating Smsy using lamberts w Scheuerell method
Smsy<-Sgen<-Umsy<-matrix(data=NA,nrow=samples,ncol=n.pops)

samp.chain<-sample(1:n_chains,samples,replace=TRUE)
samp.MCMC<-sample(1:samples, samples, replace=FALSE)
# Sgen; solve Smsy=Sgen*exp(a*(1- Sgen/S(k))) from Holt et al. 2009 Indicators of Status and Benchmarks for Conservation Units in Canada's Wild Salmon Policy 
## just need to adjust the length based on how long the MCMC is
Sgen_find <- function(Smsy,Sgen,a,b)
{
  (Smsy-(Sgen*exp(a*(1- Sgen/(a/b)))))^2
}
umsy_find <- function(Smsy,Umsy,a,b)
{
  #Umsy <- Umsy_priors$mean[1]
  #Smsy <- Smsy_priors$mean[1]
  #a <- resultSR_B3[[samp.chain[i]]][samp.MCMC[i],grep("ln_alpha.mu",mcmc_names)[1]]
  #b <- resultSR_B3[[samp.chain[i]]][samp.MCMC[i],grep("beta",mcmc_names)[1]]
  #(Smsy-(1-Umsy)*(a/b))^2
  (Smsy-(1-Umsy)*(exp(a)*Smsy*exp(-b*Smsy)))^2
}
for (j in 1:n.pops){
  samp.chain<-sample(1:n_chains,samples,replace=TRUE)
  samp.MCMC<-sample(1:samples, samples, replace=FALSE)
  
  for (i in 1:samples){
    alpha <- resultSR_B3[[samp.chain[i]]][samp.MCMC[i],grep("ln_alpha.mu",mcmc_names)[j]]
    beta <- resultSR_B3[[samp.chain[i]]][samp.MCMC[i],grep("beta",mcmc_names)[j]]
    Smsy[i,j]<-(1 - gsl::lambert_W0(exp(1 - (alpha)))) / (beta) #Smsy in 1000 random draws from MCMC
    Sgen[i,j] <- optimize(Sgen_find,c(0,Smsy[i,j]),tol=0.0001,Smsy=Smsy[i,j],a=alpha,b=beta)$minimum
    Umsy[i,j] <- optimize(umsy_find,c(0,1),tol=0.0001,Smsy=Smsy[i,j],a=alpha,b=beta)$minimum
    # plot(recruits~escapement,data=SR.dat[SR.dat$pop_no==j,],xlim=c(0,max(SR.dat[SR.dat$pop_no==j,"escapement"],na.rm=TRUE)),ylim=c(0,max(SR.dat[SR.dat$pop_no==j,"recruits"],na.rm=TRUE)))
    # curve(exp(alpha)*x*exp(-beta*x),add=TRUE,lwd=2)
    # abline(b=1,a=0)
    # abline(v=Smsy[i,j])
    # abline(v=alpha/beta)
    # legend("topright",paste("K=",round(alpha/beta,2),"\nUmsy=",1-round(Smsy[i,j]/(alpha/beta),2),sep=""))
  }
}

Smsy_t<-Sgen_t<-Umsy_t<-array(data=NA,dim=c(samples,n.pops,n.years))
for(t in 1:n.years)
{
  for (j in 1:n.pops){
    samp.chain<-sample(1:n_chains,samples,replace=TRUE)
    samp.MCMC<-sample(1:samples, samples, replace=FALSE)
    
    for (i in 1:samples){
      alpha <- resultSR_B3[[samp.chain[i]]][samp.MCMC[i],match(paste("lalpha[",t,",",j,"]",sep=""),mcmc_names)]
      beta <- resultSR_B3[[samp.chain[i]]][samp.MCMC[i],grep("beta",mcmc_names)[j]]
      Smsy_t[i,j,t] <- (1 - gsl::lambert_W0(exp(1 - (alpha)))) / (beta) #Smsy in 1000 random draws from MCMC
      Sgen_t[i,j,t] <- optimize(Sgen_find,c(0,Smsy_t[i,j,t]),tol=0.0001,Smsy=Smsy_t[i,j,t],a=alpha,b=beta)$minimum
      Umsy_t[i,j,t] <- optimize(umsy_find,c(0,1),tol=0.0001,Smsy=Smsy_t[i,j,t],a=alpha,b=beta)$minimum
      
      # plot(recruits~escapement,data=SR.dat[SR.dat$pop_no==j,],xlim=c(0,max(SR.dat[SR.dat$pop_no==j,"escapement"],na.rm=TRUE)),ylim=c(0,max(SR.dat[SR.dat$pop_no==j,"recruits"],na.rm=TRUE)))
      # curve(exp(alpha)*x*exp(-beta*x),add=TRUE,lwd=2)
      # abline(b=1,a=0)
      # abline(v=Smsy_t[i,j,t])
      # abline(v=alpha/beta)
      # legend("topright",paste("K=",round(alpha/beta,2),"\nUmsy=",1-round(Smsy_t[i,j,t]/(alpha/beta),2),sep=""))
    }
  }
}

Umsy_reg<-matrix(data=NA,nrow=samples,ncol=6)
for (k in 1:6){
  samp.chain<-sample(1:n_chains,samples,replace=TRUE)
  samp.MCMC<-sample(1:samples, samples, replace=FALSE)
  
  for (i in 1:samples){
    match(paste("mu_lalpha[",1:37,",",k,"]",sep=""),mcmc_names)
    alpha_reg <- mean(resultSR_B3[[samp.chain[i]]][samp.MCMC[i],match(paste("mu_lalpha[",1:37,",",k,"]",sep=""),mcmc_names)])
    Umsy_reg[i,k] <- 1 -gsl::lambert_W0(exp(1-alpha_reg))
  }
}

Umsy_reg_t<-array(data=NA,dim=c(samples,6,n.years))
for(t in 1:n.years)
{
  for (k in 1:6){
    samp.chain<-sample(1:n_chains,samples,replace=TRUE)
    samp.MCMC<-sample(1:samples, samples, replace=FALSE)
    
    for (i in 1:samples){
      alpha_reg <- resultSR_B3[[samp.chain[i]]][samp.MCMC[i],match(paste("mu_lalpha[",t,",",k,"]",sep=""),mcmc_names)]
      Umsy_reg_t[i,k,t] <- max(0,1 -gsl::lambert_W0(exp(1-alpha_reg)))
    }
  }
}

Smsy_priors <- apply(Smsy,2,FUN=function(x){c(mean(x),sd(x))})
Smsy_priors <- data.frame("pop"=1:n.pops,"mean"=Smsy_priors[1,],"tau"=1/(Smsy_priors[2,]^2))
Sgen_priors <- apply(Sgen,2,FUN=function(x){c(mean(x),sd(x))})
Sgen_priors <- data.frame("pop"=1:n.pops,"mean"=Sgen_priors[1,],"tau"=1/(Sgen_priors[2,]^2))
Umsy_priors <- apply(Umsy,2,FUN=function(x){c(mean(x),sd(x))})
Umsy_priors <- data.frame("pop"=1:n.pops,"mean"=Umsy_priors[1,],"tau"=1/(Umsy_priors[2,]^2))
saveRDS(Smsy_priors,"Results/Smsy.rds")
saveRDS(Sgen_priors,"Results/Sgen.rds")
saveRDS(Umsy_priors,"Results/Umsy.rds")
group_names <- c("Central Coast (South)","Hecate Lowlands","Inner Waters","Haida Gwaii","Skeena","Nass")
group_names <- group_names[c(4,6,5,2,3,1)]

Umsy_reg_priors <- apply(Umsy_reg,2,FUN=function(x){c(mean(x),sd(x))})
Umsy_reg_priors <- data.frame("Region"=group_names,"mean"=Umsy_reg_priors[1,],"tau"=1/(Umsy_reg_priors[2,]^2))
saveRDS(Umsy_reg_priors,"Results/Umsy_reg.rds")


Smsy_priors <- as.data.frame(apply(Smsy_t,c(3,2),mean))
Sgen_priors <- as.data.frame(apply(Sgen_t,c(3,2),mean))
Umsy_priors <- as.data.frame(apply(Umsy_t,c(3,2),mean))
Umsy_reg_priors <- as.data.frame(apply(Umsy_reg_t,c(3,2),mean))

colnames(Sgen_priors) <- colnames(Umsy_priors) <- colnames(Smsy_priors) <- co_pops$population
colnames(Umsy_reg_priors) <- group_names

Smsy_priors$Year <- seq(1980,2016,1)
Sgen_priors$Year <- seq(1980,2016,1)
Umsy_priors$Year <- seq(1980,2016,1)
Umsy_reg_priors$Year <- seq(1980,2016,1)

Smsy_tv <- tidyr::pivot_longer(Smsy_priors,cols=1:length(co_pops$population),names_to="Population",values_to = "Value")
Smsy_tv$Metric <- "Smsy"
Sgen_tv <- tidyr::pivot_longer(Sgen_priors,cols=1:length(co_pops$population),names_to="Population",values_to = "Value")
Sgen_tv$Metric <- "Sgen"
Umsy_tv <- tidyr::pivot_longer(Umsy_priors,cols=1:length(co_pops$population),names_to="Population",values_to = "Value")
Umsy_tv$Metric <- "Umsy"

Umsy_reg_tv <- tidyr::pivot_longer(Umsy_reg_priors,cols=1:6,names_to="Region",values_to = "Value")
Umsy_reg_tv$Metric <- "Umsy"

time_varying <- rbind(Smsy_tv,Sgen_tv,Umsy_tv)

saveRDS(time_varying,"Results/time_varying_metrics.rds")
saveRDS(Umsy_reg_tv,"Results/time_varying_regional_UMSY.rds")
