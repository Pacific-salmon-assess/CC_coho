library(tidyverse)
library(dplyr)
SR.dat<-read.table("Data/Coho_Brood_MASTER.txt", header=T)
SR.dat_full <- SR.dat
SR.dat_predict<-subset(SR.dat,SR.dat$year>=2017)
SR.dat<-subset(SR.dat,SR.dat$year<2017)

SR.dat$logRS1<-log(SR.dat$RS_E)
SR.dat$logRS2<-log(SR.dat$RS_2)
SR.dat$logRS3<-log(SR.dat$RS_3)
SR.dat$Ut <- ifelse(!is.na(SR.dat$er_2),SR.dat$er_2,SR.dat$er_E)

time_varying <- readRDS("Results/time_varying_metrics.rds")
time_varying <- tidyr::pivot_wider(time_varying,names_from="Metric",values_from="Value")

Smsy_priors <- readRDS("Results/Smsy.rds")
Sgen_priors <- readRDS("Results/Sgen.rds")
Umsy_priors <- readRDS("Results/Umsy.rds")

df_full <-merge(SR.dat,time_varying,by.x=c("year","population"),by.y=c("Year","Population"))

df_full <- tidyr::pivot_longer(df_full,cols=c("Smsy","Sgen","Umsy"),names_to="Metric",values_to = "Value")
df_full$Model <- "Time-varying"
SR.dat$Smsy <- Smsy_priors$mean[match(SR.dat$pop_no,Smsy_priors$pop)]
SR.dat$Sgen <- Sgen_priors$mean[match(SR.dat$pop_no,Sgen_priors$pop)]
SR.dat$Umsy <- Umsy_priors$mean[match(SR.dat$pop_no,Umsy_priors$pop)]

df_2 <- tidyr::pivot_longer(SR.dat,cols=c("Smsy","Sgen","Umsy"),names_to="Metric",values_to = "Value")
df_2$Model <- "Equilibrium"
df_full <- bind_rows(df_full,df_2)


co_pops<-read.table("Data/coho_groups.txt",header=TRUE)
co_pops$mean_total<-NA

for(i in co_pops$pop_no){
  dat<-subset(SR.dat,SR.dat$pop_no==i)
  co_pops[i,5]<-mean(na.omit(dat$total_runE))
}

### lumping Rivers Smith Inlet with Area 7-8
co_pops[which(co_pops$group==7),4]<-6
group_names <- c("Central Coast (South)","Hecate Lowlands","Inner Waters","Haida Gwaii","Skeena","Nass")
group_names <- group_names[c(4,6,5,2,3,1)]
df_full$group <- co_pops$group[match(df_full$population,co_pops$population)]
df_full$region <- group_names[df_full$group]
df_summ <- df_full %>%
  group_by(year,region,Metric,Model) %>%
  summarise(value=median(Value))
df_summ$region <- factor(df_summ$region,levels=group_names)

ggplot(df_summ[df_summ$Metric%in%c("Sgen","Smsy","Umsy"),],aes(x=year,y=value,colour=Model,fill=Model,shape=Metric))+
  geom_line(data=df_summ[df_summ$Metric%in%c("Sgen","Smsy","Umsy"),],aes(x=year,y=value,colour=Model))+
  geom_point(data=df_summ[df_summ$Model=="Time-varying",],aes(x=year,y=value,fill=Model,shape=Metric))+
  facet_grid(rows=vars(Metric),cols=vars(region),labeller=label_wrap_gen(width=15,multi_line = TRUE),scales='free') +
  theme_minimal() +
  ylab("Posterior median regional average")+xlab("Year")+
  #coord_flip(clip = "off") +
  #guides(fill = guide_legend(override.aes = list(size=2)))+
  scale_fill_brewer(type="qual",palette=2,direction = -1) +
  scale_colour_brewer(type="qual",palette=2,direction = -1) +
  theme(legend.position="top")

df_wide <- tidyr::pivot_wider(df_full,names_from="Metric",values_from="Value")
df_wide$U_relative <- df_wide$Ut/df_wide$Umsy
df_agg <- df_wide %>%
  group_by(year,region,Model) %>%
  summarise(value=mean(U_relative),Ut=mean(Umsy))
df_agg$region <- factor(df_agg$region,levels=group_names)


ggplot(df_agg,aes(x=year,y=value,colour=Model,fill=Model))+
  geom_line()+
  geom_point(data=df_agg[df_agg$Model=="Time-varying",],aes(x=year,y=value,fill=Model))+
  geom_hline(yintercept=1,lty=2,colour='red') +
  facet_grid(rows=vars(region),labeller=label_wrap_gen(width=15,multi_line = TRUE)) +
  theme_minimal() +
  ylab("Posterior mean population status within region")+xlab("Year")+
  #coord_flip(clip = "off") +
  #guides(fill = guide_legend(override.aes = list(size=2)))+
  scale_fill_brewer(type="qual",palette=2,direction = -1) +
  scale_colour_brewer(type="qual",palette=2,direction = -1) +
  theme(legend.position="top")

ggplot(df_agg,aes(x=year,y=Ut,colour=Model,fill=Model))+
  geom_line()+
  geom_point(data=df_agg[df_agg$Model=="Time-varying",],aes(x=year,y=Ut,fill=Model))+
  facet_wrap(~region,ncol=3,labeller=label_wrap_gen(width=15,multi_line = TRUE)) +
  theme_minimal() +
  ylab("Regional average U(MSY)")+xlab("Year")+
  #coord_flip(clip = "off") +
  #guides(fill = guide_legend(override.aes = list(size=2)))+
  scale_fill_brewer(type="qual",palette=2,direction = -1) +
  scale_colour_brewer(type="qual",palette=2,direction = -1) +
  theme(legend.position="top",strip.text.y = element_text(size=6.5),strip.text.x = element_text(size=8),axis.text.x=element_text(size=7,angle=45,hjust=1),axis.text.y=element_text(size=7),legend.text=element_text(size=8),legend.title=element_text(size=8),axis.title=element_text(size=8),legend.key.size = unit(0.9, "line"),panel.spacing.y = unit(0.75, "lines"))
ggsave("Figures/time-varying umsy.jpeg",width=6,height=6,units="in",dpi=800)

Umsy_reg_tv <- readRDS("Results/time_varying_regional_UMSY.rds")
Umsy_reg_tv$Model <- "Time-varying"
Umsy_reg <- readRDS("Results/Umsy_reg.rds")
Umsy_static <- expand.grid(Year=1980:2016,"Region"=unique(Umsy_reg$Region),"Metric"="Umsy",Model="Equilibrium")
Umsy_static$Value <- Umsy_reg$mean[match(Umsy_static$Region,Umsy_reg$Region)]

Umsy_reg_full <- bind_rows(Umsy_reg_tv,Umsy_static)

Umsy_reg_full$Region <- factor(Umsy_reg_full$Region,levels=group_names)

ggplot(Umsy_reg_full,aes(x=Year,y=Value,colour=Model,fill=Model))+
  geom_line()+
  geom_point(data=Umsy_reg_full[Umsy_reg_full$Model=="Time-varying",],aes(x=Year,y=Value,fill=Model))+
  facet_wrap(~Region,ncol=3,labeller=label_wrap_gen(width=15,multi_line = TRUE)) +
  theme_minimal() +
  ylab("Regional average U(MSY)")+xlab("Year")+
  #coord_flip(clip = "off") +
  #guides(fill = guide_legend(override.aes = list(size=2)))+
  scale_fill_brewer(type="qual",palette=2,direction = -1) +
  scale_colour_brewer(type="qual",palette=2,direction = -1) +
  theme(legend.position="top",strip.text.y = element_text(size=6.5),strip.text.x = element_text(size=8),axis.text.x=element_text(size=7,angle=45,hjust=1),axis.text.y=element_text(size=7),legend.text=element_text(size=8),legend.title=element_text(size=8),axis.title=element_text(size=8),legend.key.size = unit(0.9, "line"),panel.spacing.y = unit(0.75, "lines"))
ggsave("Figures/time-varying regional umsy.jpeg",width=6,height=6,units="in",dpi=800)
write.csv(Umsy_reg_full,file="Results/UMSY_regional.csv")
write.csv(df_wide,file="Results/coho time series and MSY reference points.csv")
