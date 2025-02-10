library(tidyverse)
library(readxl)
library(here)

# trying to clean up the MRP data ########################################################
release <- read_xlsx(here("Data/MRP/Copy of 2024-03-11 Release report.xlsx"), sheet = 2) |>
  filter(SPECIES_NAME == "Coho") #RELEASE_STAGE_NAME == c("Smolt 1+", "Sea")

#summarise all types of coho released
release |>
  group_by(RELEASE_STAGE_NAME) |>
  summarise(n())

smolt.release <- filter(release, RELEASE_STAGE_NAME %in% c("Smolt 1+", "Seapen 1+", "Chan Sm 1+"))

filter(release, RELEASE_STAGE_NAME == "Seapen 2+") |> #check 2+ do we care?
  select(PROJ_NAME, BROOD_YEAR, TotalRelease)


#want release site, date, number, which fishery intercepted, estimated and expanded #
extractor <- read_xlsx(here("Data/MRP/GLASERD_20250203_125530.xlsx"), sheet = 2)

unique(extractor$`(RL) Release PSC Basin Name`) #check where releases are from

extractor_releases <- extractor |>
  select(`(RL) Tagcode`, `(RL) Brood Year`, `(RL) Release Year`, `(RL) Release Site Name`, 
         `(RL) Release PSC Basin Name`, `(RL) Num WithCWT 1st Mark (v4)`) |>
  distinct() |>
  rename(tagcode = `(RL) Tagcode`) |>
  mutate(release_age = `(RL) Release Year` - `(RL) Brood Year`) |>
  filter(release_age >= 2) #smolts only
  
extractor_recoveries <- extractor |>
  select(`(RC) Tagcode`, `(RC) Recovery Year`, `(RC) Catch Region Name`,
         `(RC) MRP Area Name`, `(RC) Age - Total`, `(RC) Fishery PSC Code`, `(RC) Gear Code`,  
         `(RC) Gear Aggregate Code`, `(RC) Gear PSC Code`,  `(RC) Estimated Number`, `(RC) MRP Fishery Code`) |>
  rename(tagcode = `(RC) Tagcode`)

#what recovery area should I summarize by?
#mike suggests Strata ID, but use the "Criteria" filter in the extractor to see the dups.
#ryan suggests these below. either way, will need to aggregate them up
unique(extractor_recoveries$`(RC) Catch Region Name`)
unique(extractor_recoveries$`(RC) MRP Area Name`) #this would make sense if multiple PFMAs weren't grouped together
unique(extractor_recoveries$`(RC) Fishery PSC Code`)

unique(extractor_recoveries$`(RC) Gear Code`)
unique(extractor_recoveries$`(RC) Gear PSC Code`)


extractor_recoveries_fishery <- extractor_recoveries |> 
  mutate(fishery = case_when(`(RC) MRP Fishery Code` == "N" ~ "FN-EO", #change codes to names, based on extractor "criteria" table
                             `(RC) MRP Fishery Code` == "Z" ~ "Sport", 
                             `(RC) MRP Fishery Code` == "C" ~ "Commercial", 
                             `(RC) MRP Fishery Code` == "T" ~ "Test", 
                             `(RC) MRP Fishery Code` == "F" ~ "FSC", 
                             `(RC) MRP Fishery Code` == "E" ~ "Escapement", 
                             `(RC) MRP Fishery Code` == "H" ~ "High Seas", 
                             `(RC) MRP Fishery Code` == "R" ~ "Research", 
                             `(RC) MRP Fishery Code` == "X" ~ "Unspecified", 
                             `(RC) MRP Fishery Code` == "A" ~ "FN - unspecified", 
                             `(RC) MRP Fishery Code` == "Y" ~ "groundfish trawl")) |>
  group_by(`tagcode`, `(RC) Recovery Year`, `(RC) Age - Total`, fishery) |>
  summarise(recoveries = sum(`(RC) Estimated Number`))

ER_fishery <- left_join(extractor_recoveries_fishery, extractor_releases, by = "tagcode") |>
  filter(!is.na(fishery), !is.na(recoveries)) |>
  mutate(ER = recoveries/`(RL) Num WithCWT 1st Mark (v4)`) |>
  arrange(`(RC) Recovery Year`, tagcode, fishery)

# exploring some groupings ###
ER_fishery |>
  group_by(fishery) |>
  summarise(n())

ER_fishery |>
  group_by(fishery, `(RC) Recovery Year`, `(RL) Release PSC Basin Name`) |>
  summarise(n())

group <- ER_fishery |>
  group_by(fishery, tagcode, `(RC) Recovery Year`, `(RL) Release PSC Basin Name`) |>
  summarise(groups = n())

filter(group, groups>1) #seems like most descriptive grouping

#why so many NAs in the data?
ER_fishery |>
  group_by(`(RL) Release PSC Basin Name`) |>
  summarise(n())  

# doing some plotting to glean any inference ###
ER_fishery <- filter(ER_fishery, !(fishery %in% c("Escapement", "Test")))

ER_fishery_ag <- ER_fishery |>
  group_by(`(RC) Recovery Year`, fishery) |>
  #or summarize all recoveries by all releases? otherwise it will be reporting a lot of 
    #"one off" recoveries?
  summarise(avg_ER = median(ER, na.rm = TRUE), 
            sd = sd(ER, na.rm = TRUE), 
            lwr = quantile(ER, .25, na.rm = TRUE), 
            upr = quantile(ER, .75, na.rm = TRUE))

ggplot(ER_fishery_ag, aes(`(RC) Recovery Year`, avg_ER, color = fishery)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = fishery, alpha = 0.2)) +
  geom_line() +
  labs(title = "Average ER of CWT marked Coho released on the CC", 
       x = "recovery year", y = "average ER (recoveries/releases)") +
  guides(alpha = "none")

ggplot(filter(ER_fishery_ag, `(RC) Recovery Year` >= 2010), 
       aes(`(RC) Recovery Year`, avg_ER, color = fishery)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = fishery, alpha = 0.2)) +
  geom_line(size = 1.5) +
  labs(title = "Reccent average ER of CWT marked Coho released on the CC", 
       x = "recovery year", y = "average ER (recoveries/releases)") +
  guides(alpha = "none")

ggplot(filter(ER_fishery, !is.na(`(RL) Release PSC Basin Name`)), 
       aes(`(RC) Recovery Year`, log(ER), color = fishery)) +
  geom_point() +
  facet_wrap(~`(RL) Release PSC Basin Name`) +
  labs(title = "log ER of CWT marked Coho released on the CC, by recovery area", 
       x = "recovery year", y = "log(ER) (recoveries/releases)") 
