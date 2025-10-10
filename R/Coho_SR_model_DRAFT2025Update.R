# fit models ----

require(rjags)
require(R2jags)
library(runjags)
library(here)
library(bayestestR) #for hdi()

thinning <- 200
samples <- 1500
n_chains <- 4

SR.dat<-read.csv(here("Data/Coho_Brood_MASTER2025Update.csv"), header=T)
SR.dat<-subset(SR.dat,SR.dat$year<2021)

SR.dat$logRS1<-log(SR.dat$RS_E)
SR.dat$logRS2<-log(SR.dat$RS_2)
SR.dat$logRS3<-log(SR.dat$RS_3)
SR.dat$logRS4<-log(SR.dat$RS_4)

SR.dat$er_est <- SR.dat$er_2
SR.dat$er_est[is.na(SR.dat$er_est)] <- SR.dat$er_E[is.na(SR.dat$er_est)]

n.pops<-max(SR.dat$pop_no)
n.years<-length(seq(1980,2020,1))
spawn<-matrix(nrow=n.years,ncol=n.pops,NA)

rec1<-matrix(nrow=n.years,ncol=n.pops,NA)
rec2<-matrix(nrow=n.years,ncol=n.pops,NA)
rec3<-matrix(nrow=n.years,ncol=n.pops,NA)
rec4<-matrix(nrow=n.years,ncol=n.pops,NA)

logRS1<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS2<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS3<-matrix(nrow=n.years,ncol=n.pops,NA)
logRS4<-matrix(nrow=n.years,ncol=n.pops,NA)
logRSo<-matrix(nrow=n.years,ncol=n.pops,NA)
er_est <- matrix(nrow=n.years,ncol=n.pops,NA)

for(i in 1:n.pops){
  dat<-subset(SR.dat,SR.dat$pop_no==i)
  spawn[,i]<-as.numeric(dat[,"escapement"])
  rec1[,i]<-dat[,"rec_E"]
  rec2[,i]<-dat[,"rec_2"]
  rec3[,i]<-dat[,"rec_3"]
  rec4[,i]<-dat[,"rec_4"]
  logRS1[,i]<-dat[,"logRS1"]
  logRS2[,i]<-dat[,"logRS2"]
  logRS3[,i]<-dat[,"logRS3"]
  logRS4[,i]<-dat[,"logRS4"]
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
logRS4.mat<-matrix(nrow=n.years,ncol=n.pops,NA)
logRSB.mat<-matrix(nrow=n.years,ncol=n.pops,NA)


for(i in 1:n.pops){
  spawn.mat[1:length(na.omit(yrs[,i])),i]<-spawn[na.omit(yrs[,i]),i]
}

for(i in 1:n.pops){
  logRS1.mat[1:length(na.omit(yrs[,i])),i]<-logRS1[na.omit(yrs[,i]),i]
  logRS2.mat[1:length(na.omit(yrs[,i])),i]<-logRS2[na.omit(yrs[,i]),i]
  logRS3.mat[1:length(na.omit(yrs[,i])),i]<-logRS3[na.omit(yrs[,i]),i]
  logRS4.mat[1:length(na.omit(yrs[,i])),i]<-logRS4[na.omit(yrs[,i]),i]
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


## I grouped rivers and smiths inlet with Area 7 & 8

co_pops<-read.table(here("Data/coho_groups_2025.txt"),header=TRUE)
co_pops$mean_total<-NA

for(i in 1:58){
  dat<-subset(SR.dat,SR.dat$pop_no==i)
  co_pops[i,5]<-mean(na.omit(dat$total_runE))
}

### lumping Rivers Smith Inlet with Area 7-8
co_pops[which(co_pops$group==7),4]<-6

### using mean run size (harvest scenario 1) across the timeseries as our prior on capacity
Smax.p<-rep(NA,58)
Smax.tau<-rep(NA,58)
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
  "logRS"=logRS4,
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
sigma_sd <- apply(logRS4,2,sd,na.rm=TRUE)
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

saveRDS(resultSR_B3,file="Results/2025COSR3B.tr1_lalpha_MCMC.rds")

mcmc_names <- colnames(resultSR_B3[[1]])
SR.results_B3<-summary(resultSR_B3)
SRquantilesDF <- as.data.frame(SR.results_B3$quantiles)
saveRDS(SRquantilesDF,file="Results/2025COSR3B.tr1_lalpha_MCMCDF.rds")
saveRDS(mcmc_names,file="Results/2025mcmc_names.rds")

# load model fit/posteriors ----

resultSR_B3<-readRDS(here("Results/2025COSR3B.tr1_lalpha_MCMC.rds"))
mcmc_names <- colnames(resultSR_B3[[1]])
SR.results_B3<-summary(resultSR_B3)

# pulling out the parameters
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

# pulling out the individual population productivity trends

a.pops_med<-matrix(data=NA, nrow=41,ncol=n.pops+1)
a.pops_med[,1]<-seq(1980,2020,1)

a.pops_lci<-matrix(data=NA, nrow=41,ncol=n.pops+1)
a.pops_lci[,1]<-seq(1980,2020,1)

a.pops_uci<-matrix(data=NA, nrow=41,ncol=n.pops+1)
a.pops_uci[,1]<-seq(1980,2020,1)

pop.start<-rep(NA,58)
pop.end<-rep(NA,58)

pop.start[1]<-1
pop.end[1]<-0+n.years

for (i in 2:58){
  pop.start[i]<-pop.end[i-1]+1
  pop.end[i]<-pop.end[i-1]+n.years
}

for (i in 1:n.pops){
  years<-1:n.years
  a.pops_med[years,i+1]<-pop_alphas3B[pop.start[i]:pop.end[i],3]
  a.pops_lci[years,i+1]<-pop_alphas3B[pop.start[i]:pop.end[i],1]
  a.pops_uci[years,i+1]<-pop_alphas3B[pop.start[i]:pop.end[i],5]
}
a.pops_med[,-1][which(is.na(logRS4),arr.ind = TRUE)] <- NA
a.pops_lci[,-1][which(is.na(logRS4),arr.ind = TRUE)] <- NA
a.pops_uci[,-1][which(is.na(logRS4),arr.ind = TRUE)] <- NA



#  visualizing the results ----

### this is the region for each population
hg<-which(co_pops$group==1)
nass<-which(co_pops$group==2)
skeena<-which(co_pops$group==3)
hec_low<-which(co_pops$group==4)
nc_inner<-which(co_pops$group==5)
cc_south<-which(co_pops$group==6)

### plotting trend in NCC coho productivity 
years<-seq(1980,2020,1)
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

years <- (nrow(mu_alphaSkeena)-1):nrow(mu_alphaSkeena)
mypost <- as.matrix(resultSR_B3)

x <- c(1980:2020, 2020:1980)

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
years<-seq(1980,2020,1)

### this is the region for each population
hg<-which(co_pops$group==1)
nass<-which(co_pops$group==2)
skeena<-which(co_pops$group==3)
hec_low<-which(co_pops$group==4)
nc_inner<-which(co_pops$group==5)
cc_south<-which(co_pops$group==6)

library(tidyverse)
str(a.pops_med)
str(SR.dat)
a.pops_df <- as.data.frame(a.pops_med)
colnames(a.pops_df) <- c("year",co_pops$population)
a.pops_long <- pivot_longer(a.pops_df,cols=-1,values_to = "ln_a",names_to="population")

df_coho <- left_join(SR.dat,a.pops_long,by=c("year","population"))
df_coho$mean_escapement <- co_pops$mean_total[match(df_coho$population,co_pops$population)]
df_coho$region_no <- co_pops$group[match(df_coho$population,co_pops$population)]
df_coho$Region[df_coho$region_no==1] <- "Haida Gwaii"
df_coho$Region[df_coho$region_no==2] <- "Nass"
df_coho$Region[df_coho$region_no==3] <- "Skeena"
df_coho$Region[df_coho$region_no==4] <- "Hecate Lowlands"
df_coho$Region[df_coho$region_no==5] <- "Inner Waters"
df_coho$Region[df_coho$region_no==6] <- "Central Coast (South)"
df_coho$Year <- df_coho$year

regional_prod <- rbind(cbind("Year"=years,"Region"="Nass",as.data.frame(mu_alphaNass)),
                       cbind("Year"=years,"Region"="Skeena",as.data.frame(mu_alphaSkeena)),
                       cbind("Year"=years,"Region"="Inner Waters",as.data.frame(mu_alphaNC)),
                       cbind("Year"=years,"Region"="Haida Gwaii",as.data.frame(mu_alphaHG)),
                       cbind("Year"=years,"Region"="Hecate Lowlands",as.data.frame(mu_alphaHec)),
                       cbind("Year"=years,"Region"="Central Coast (South)",as.data.frame(mu_alphaCC)))
row.names(regional_prod) <- NULL
regional_prod$Region <- factor(regional_prod$Region,levels=c("Haida Gwaii","Nass","Skeena","Hecate Lowlands","Inner Waters","Central Coast (South)"))
df_coho$Region <- factor(df_coho$Region,levels=c("Haida Gwaii","Nass","Skeena","Hecate Lowlands","Inner Waters","Central Coast (South)"))
margins <- c(0.5,0.5,0.5,1.1)
margins <- c(0.1,1,0.1,0.1)

ggplot(data=regional_prod,aes(x=Year,y=`50%`,group=Region)) + 
  geom_ribbon(data=regional_prod,aes(x=Year,ymax=`97.5%`,ymin=`2.5%`,group=Region),alpha=0.5) +
  geom_point(data=df_coho,aes(x=Year,y=ln_a,size=mean_escapement),pch=21,col="black",fill="white") +
  geom_line(data=regional_prod,aes(x=Year,y=`50%`,colour=Region),lwd=1) +
  facet_wrap(~Region,ncol=3)+
  theme_minimal() +
  scale_size_area(name="Mean spawners",limits=(c(100,50000)),
                  breaks=(c(100,1000,10000,50000)),
                  labels = c("100","1,000","10,000","50,000"))+
  scale_colour_brewer(name="Region",palette = "Paired",direction=1) +
  theme(legend.position = "top",strip.text.x = element_text(hjust = -0),plot.margin=unit(margins,"line")) +
  xlab("Year") + ylab("Intrinsic recruitment productivity (ln \U03b1)")+
  theme(text = element_text(size=10),axis.text = element_text(size=10),legend.title = element_text(size = 9),legend.text = element_text(size=7)) +
  theme(legend.box.just="center",legend.box="horizontal",legend.justification = "center",legend.key.size=unit(0.5, "lines"),legend.margin = margin(c(0,1,0,0),unit="lines"))

ggsave("Figures/CO_alpha_trend_pops_NCC_2024_update.jpeg", width = 9, height=6,units="in", dpi=600)


# estimate decline in productivities ----
prod_decline <- function(x)
{
  x_perc <- 100*((rowMeans(x[,years])-rowMeans(x))/(rowMeans(x)))
  return(c(mean(x_perc),quantile(x_perc,probs=c(0.025,0.5,0.975))))
}
hg <- prod_decline(exp(mypost[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,1]",mcmc_names)]))
nass <- prod_decline(exp(mypost[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,2]",mcmc_names)]))
skeena <- prod_decline(exp(mypost[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,3]",mcmc_names)]))
hec <- prod_decline(exp(mypost[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,4]",mcmc_names)]))
inner <- prod_decline(exp(mypost[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,5]",mcmc_names)]))
cc <- prod_decline(exp(mypost[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,6]",mcmc_names)]))

100*(mean(exp(mu_alphaNass[(nrow(mu_alphaNass)-1):nrow(mu_alphaNass),3]))-mean(exp(mu_alphaNass[,3])))/(mean(exp(mu_alphaNass[,3])))
100*(mean(exp(mu_alphaSkeena[(nrow(mu_alphaSkeena)-1):nrow(mu_alphaSkeena),3]))-mean(exp(mu_alphaSkeena[,3])))/(mean(exp(mu_alphaSkeena[,3])))
100*(mean(exp(mu_alphaNC[(nrow(mu_alphaNC)-1):nrow(mu_alphaNC),3]))-mean(exp(mu_alphaNC[,3])))/(mean(exp(mu_alphaNC[,3])))
100*(mean(exp(mu_alphaHG[(nrow(mu_alphaHG)-1):nrow(mu_alphaHG),3]))-mean(exp(mu_alphaHG[,3])))/(mean(exp(mu_alphaHG[,3])))
100*(mean(exp(mu_alphaHec[(nrow(mu_alphaHec)-1):nrow(mu_alphaHec),3]))-mean(exp(mu_alphaHec[,3])))/(mean(exp(mu_alphaHec[,3])))
100*(mean(exp(mu_alphaCC[(nrow(mu_alphaCC)-1):nrow(mu_alphaCC),3]))-mean(exp(mu_alphaCC[,3])))/(mean(exp(mu_alphaCC[,3])))

# save regional productivties ----

resultSR_B3<-readRDS(here("Results/2025COSR3B.tr1_lalpha_MCMC.rds"))
mcmc_names <- colnames(resultSR_B3[[1]])
resultSR_B3 <- as.matrix(resultSR_B3)
group_names <- c("Central Coast (South)","Hecate Lowlands","Inner Waters","Haida Gwaii","Skeena","Nass")
group_names <- group_names[c(4,6,5,2,3,1)]
region_names <- c("Area 7-10","Area 5-6","Area 6 - Inner","Area 2E","Area 4","Area 3")
region_names <- region_names[c(4,6,5,2,3,1)]
mu_alphaHG<-colMeans(resultSR_B3[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,1]",mcmc_names)]) # grab Group 1
mu_alphaNass<-colMeans(resultSR_B3[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,2]",mcmc_names)]) # grab Group 2
mu_alphaSkeena<-colMeans(resultSR_B3[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,3]",mcmc_names)]) # grab Group 3
mu_alphaHec<-colMeans(resultSR_B3[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,4]",mcmc_names)]) # grab Group 4
mu_alphaNC<-colMeans(resultSR_B3[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,5]",mcmc_names)]) # grab Group 5
mu_alphaCC<-colMeans(resultSR_B3[,grepl("\\bmu_lalpha",mcmc_names) & grepl("\\b,6]",mcmc_names)]) # grab Group 6

log_alpha <- data.frame("year"=seq(1980,2020,1),"ln_alpha"=c(mu_alphaHG,mu_alphaNass,mu_alphaSkeena,mu_alphaHec,mu_alphaNC,mu_alphaCC),"region"=rep(group_names,each=length(mu_alphaCC)),"pfma"=rep(region_names,each=length(mu_alphaCC)))
head(log_alpha)
library(ggplot2)
ggplot(log_alpha,aes(x=year,y=ln_alpha,fill=region)) +
  geom_point() +
  geom_line() +
  facet_wrap(~region) +
  scale_fill_brewer()

write.csv(log_alpha,"Data/Coho_alpha_trends_2024update.csv",row.names=FALSE)














