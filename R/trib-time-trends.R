library(tidyverse)
library(here)
library(ggsidekick)
library(lme4)


co_pops<-read.table(here("Data/coho_groups_2025.txt"),header=TRUE)
SR.dat<-read.csv(here("Data/Coho_Brood_MASTER2025UpdateDQ.csv"), header=T)
spawn_data <- left_join(SR.dat,co_pops|>select(pop_no,group),by="pop_no")

# hecate lowlands ----
pop_group <- spawn_data |>
  filter(!is.na(total_run4),
         !is.na(escapement),
         group==4) |>
  mutate(survey_quality = case_when(
    data_qual == "2" ~ "low / unknown",
    data_qual == "1" ~ "high"),
    spawners = as.numeric(escapement),
    returns = total_run4)|>
  select(year, group,population, survey_quality,spawners, returns)

pop_group_short<- pop_group|>
  filter(year>2000)

pop_group_short$year_index <- pop_group_short$year-1999

pop_group_short_high <- pop_group_short |>
  filter(survey_quality != "low / unknown",
         population %in% c("east_arm","west_arm"))
  
model<-lm(log(spawners)~year_index,data=pop_group_short_high|>filter(population=="east_arm"))
model_s<-summary(model)
round(as.numeric((exp((model_s$coefficients[2,1])*(3*4))-1)*100))

model<-lm(log(spawners)~year_index,data=pop_group_short_high|>filter(population=="west_arm"))
model_s<-summary(model)
round(as.numeric((exp((model_s$coefficients[2,1])*(3*4))-1)*100))

pop_group_short_high

# central coast south ----
pop_group <- spawn_data |>
  filter(!is.na(total_run4),
         !is.na(escapement),
         group==6) |>
  mutate(survey_quality = case_when(
    data_qual == "2" ~ "low / unknown",
    data_qual == "1" ~ "high"),
    spawners = as.numeric(escapement),
    returns = total_run4)|>
  select(year, group,population, survey_quality,spawners, returns)

pop_group_short<- pop_group|>
  filter(year>2000)

pop_group_short$year_index <- pop_group_short$year-1999

pop_group_short_high <- pop_group_short |>
  filter(survey_quality != "low / unknown",
         population %in% c("cascade","elcho", "martin", "quartcha", "roscoe"))

model<-lm(log(spawners)~year_index,data=pop_group_short_high|>filter(population=="cascade"))
model_s<-summary(model)
round(as.numeric((exp((model_s$coefficients[2,1])*(3*4))-1)*100))

model<-lm(log(spawners)~year_index,data=pop_group_short_high|>filter(population=="elcho"))
model_s<-summary(model)
round(as.numeric((exp((model_s$coefficients[2,1])*(3*4))-1)*100))

model<-lm(log(spawners)~year_index,data=pop_group_short_high|>filter(population=="martin"))
model_s<-summary(model)
round(as.numeric((exp((model_s$coefficients[2,1])*(3*4))-1)*100))

model<-lm(log(spawners)~year_index,data=pop_group_short_high|>filter(population=="quartcha"))
model_s<-summary(model)
round(as.numeric((exp((model_s$coefficients[2,1])*(3*4))-1)*100))

model<-lm(log(spawners)~year_index,data=pop_group_short_high|>filter(population=="roscoe"))
model_s<-summary(model)
round(as.numeric((exp((model_s$coefficients[2,1])*(3*4))-1)*100))


ggplot(pop_group_short_high,aes(x = year, y = spawners)) +
  geom_smooth(method="lm", color="grey") +
  geom_point(size=2, aes(color=survey_quality))+
  xlab("Year") +
  ylab("Spawners") +
  theme_minimal() +
  facet_wrap(~population, scales = "free_y", ncol = 3)+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size = 8))   




# super plot ----

pop_group <- spawn_data |>
  filter(!is.na(total_run4),
         !is.na(escapement),
         group %in% c(4,5,6)) |>
  mutate(survey_quality = case_when(
    data_qual == "2" ~ "low / unknown",
    data_qual == "1" ~ "high"),
    spawners = as.numeric(escapement),
    returns = total_run4)|>
  select(year, group,population, survey_quality,spawners, returns)

pop_group <- spawn_data |>
  filter(
    !is.na(total_run4),
    !is.na(escapement),
    group %in% c(4, 5, 6)
  ) |>
  mutate(
    # initial survey quality categories
    survey_quality = case_when(
      data_qual == 2 ~ "adults present",
      data_qual == 1 ~ "abundance based estimate"
    ),
    spawners = as.numeric(escapement),
    returns = total_run4
  ) |>
  group_by(population) |>
  mutate(
    # determine reviewed status based on abundance estimate
    reviewed_status = if_else(
      any(survey_quality == "abundance based estimate", na.rm = TRUE),
      "reviewed",
      "not reviewed"
    ),
    # override survey_quality if population is not reviewed
    survey_quality = if_else(
      reviewed_status == "not reviewed",
      "unknown",
      survey_quality
    )
  ) |>
  ungroup() |>
  select(
    year, group, population,
    survey_quality, reviewed_status,
    spawners, returns
  )


ggplot(pop_group |> filter(group == 4) ,aes(x = year, y = spawners)) +
  geom_point(size=2, aes(color=survey_quality))+
  xlab("Year") +
  ylab("Spawners") +
  ylim(0,NA) +
  theme_minimal() +
  facet_wrap(~population, scales = "free_y", ncol = 3) +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size = 8)) + 
  ggtitle("Hecate lowlands")

ggsave("Figures/hecate-lowlands.spawners.jpeg", width = 8, height=5.5,units="in", dpi=600)


ggplot(pop_group |> filter(group == 5) ,aes(x = year, y = spawners)) +
  geom_point(size=2, aes(color=survey_quality))+
  xlab("Year") +
  ylab("Spawners") +
  ylim(0,NA) +
  theme_minimal() +
  facet_wrap(~population, scales = "free_y", ncol = 4) +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size = 8)) + 
  ggtitle("Inner Waters")

ggsave("Figures/inner-waters.spawners.jpeg", width = 10, height=5.5,units="in", dpi=600)

ggplot(pop_group |> filter(group == 6) ,aes(x = year, y = spawners)) +
  geom_point(size=2, aes(color=survey_quality))+
  xlab("Year") +
  ylab("Spawners") +
  ylim(0,NA) +
  theme_minimal() +
  facet_wrap(~population, scales = "free_y", ncol = 4) +
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size = 8)) + 
  ggtitle("Central Coast (South)")

ggsave("Figures/cc-south.spawners.jpeg", width = 10, height=5.5,units="in", dpi=600)

# abundance based estimates trends ----

pop_group_short_all <- pop_group |>
  filter(year>2000,
         survey_quality == "abundance based estimate",
         ! population %in% c("arnoup","nias", "tyler_ck")) |>
  select(year, group,population, survey_quality,spawners, returns)


pop_group_short_all$year_index <- pop_group_short_all$year-1999

   

dat_text <- data.frame(
  label = c("-10% (-40 to 35%)", "-59% (-77 to -28%)", "-59% (-76 to -32%)", "-40% (-60 to -10%)", "-46% (-68 to -10%)",
            "-64% (-76 to -46%)", "-51% (-67 to -26%)", "+1% (-37 to 64%)"),
  population   = c("green", "east_arm","west_arm","roscoe","quartcha","martin","elcho","cascade" )
)

ggplot(pop_group_short_all,aes(x = year, y = spawners)) +
  stat_smooth(formula=y~x, method="glm", color="grey", 
              method.args = list(family = gaussian(link = 'log')),
              na.rm=TRUE) +
  geom_point(size=2, aes(color=survey_quality))+
  xlab("Year") +
  ylab("Spawners") +
  theme_sleek() +
  facet_wrap(~population, scales = "free_y", ncol = 3,axes = "all_x")+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size = 8),
        legend.position = "none") + 
  geom_text(
    data    = dat_text,
    mapping = aes(x = Inf, y = Inf, label = label),
    hjust   = 1.1,
    vjust   = 1.5,
    color="grey50", 
    size=3)
ggsave("Figures/cc-spawner-trends.jpeg", width = 8, height=6,units="in", dpi=600)

# time trends for abundance based estimate systems ----

model<-lmer(log(spawners)~year_index+(year_index|population),data=pop_group_short_all)
model_s<-summary(model)

main_coef <- model_s$coefficients[2,1]
pop_coefs <- as.data.frame(ranef(model)) |>
  filter(term == "year_index") |>
  mutate(perc_change = round(as.numeric((exp((main_coef+condval)*(3*4))-1)*100)),
         perc_change_up = round(as.numeric((exp((main_coef+condval+(condsd*1.96))*(3*4))-1)*100)),
         perc_change_lwr = round(as.numeric((exp((main_coef+condval-(condsd*1.96))*(3*4))-1)*100)),
         population = grp,
         '% change (3-gen)' = perc_change,
         '% change (3-gen) upper' = perc_change_up,
         '% change (3-gen) lower' = perc_change_lwr)|> 
  select(population,'% change (3-gen)','% change (3-gen) upper','% change (3-gen) lower')

knitr::kable(pop_coefs, align = "l") 

# time trends for abundance based estimate systems HECATE STRAIT MAINLAND ----
pop_group_short_all_HSM <- pop_group |>
  filter(year>2000,
         population %in% c("belowe_ck","quaal", "sylvia_ck")) |>
  select(year, group,population, survey_quality,spawners, returns)


pop_group_short_all_HSM$year_index <- pop_group_short_all_HSM$year-1999
model<-lmer(log(spawners)~year_index+(year_index|population),data=pop_group_short_all_HSM)
model_s<-summary(model)

main_coef <- model_s$coefficients[2,1]
pop_coefs <- as.data.frame(ranef(model)) |>
  filter(term == "year_index") |>
  mutate(perc_change = round(as.numeric((exp((main_coef+condval)*(3*4))-1)*100)),
         perc_change_up = round(as.numeric((exp((main_coef+condval+(condsd*1.96))*(3*4))-1)*100)),
         perc_change_lwr = round(as.numeric((exp((main_coef+condval-(condsd*1.96))*(3*4))-1)*100)),
         population = grp,
         '% change (3-gen)' = perc_change,
         '% change (3-gen) upper' = perc_change_up,
         '% change (3-gen) lower' = perc_change_lwr)|> 
  select(population,'% change (3-gen)','% change (3-gen) upper','% change (3-gen) lower')

knitr::kable(pop_coefs, align = "l") 

# abundance based estimates trends HECATE STRAIT MAINLAND ----
dat_text <- data.frame(
  label = c("-64% (-62 to 65%)", "-27% (-22 to -31%)", "-68% (-66 to -69%)"),
  population   = c("belowe_ck","quaal", "sylvia_ck")
)

ggplot(pop_group_short_all_HSM,aes(x = year, y = spawners)) +
  stat_smooth(formula=y~x, method="glm", color="grey", 
              method.args = list(family = gaussian(link = 'log')),
              na.rm=TRUE) +
  geom_point(size=2, aes(color=survey_quality))+
  xlab("Year") +
  ylab("Spawners") +
  theme_sleek() +
  facet_wrap(~population, scales = "free_y", ncol = 3,axes = "all_x")+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size = 8),
        legend.position = "none") + 
  geom_text(
    data    = dat_text,
    mapping = aes(x = Inf, y = Inf, label = label),
    hjust   = 1.1,
    vjust   = 1.5,
    color="grey50", 
    size=3)
ggsave("Figures/cc-HSM-CU-spawner-trends.jpeg", width = 8, height=6,units="in", dpi=600)

