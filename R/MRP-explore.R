library(tidyverse)
library(readxl)
library(here)

release <- read_xlsx(here("Data/MRP/Copy of 2024-03-11 Release report.xlsx"), sheet = 2) |>
  filter(SPECIES_NAME == "Coho") #RELEASE_STAGE_NAME == c("Smolt 1+", "Sea")

#summarise all types of coho released
release |>
  group_by(RELEASE_STAGE_NAME) |>
  summarise(n())

smolt.release <- filter(release, RELEASE_STAGE_NAME %in% c("Smolt 1+", "Seapen 1+", "Chan Sm 1+"))

filter(release, RELEASE_STAGE_NAME == "Seapen 2+") |> #check 2+ do we care?
  select(PROJ_NAME, BROOD_YEAR, TotalRelease)


extractor <- read.csv(here("Data/MRP/2025-01-24 Mrp Extractor Results Query #137479.csv"))
