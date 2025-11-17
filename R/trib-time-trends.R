library(tidyverse)
library(here)
library(ggsidekick)

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

ggplot(pop_group_short_all,aes(x = year, y = spawners)) +
  geom_smooth(method="lm", color="grey") +
  geom_point(size=2, aes(color=survey_quality))+
  xlab("Year") +
  ylab("Spawners") +
  theme_minimal() +
  facet_wrap(~population, scales = "free_y", ncol = 3)+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size = 8))   

ggsave("Figures/cc-spawner-trends.jpeg", width = 10, height=5.5,units="in", dpi=600)
