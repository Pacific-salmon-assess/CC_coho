library(plyr)
library(tidyverse)
library(here)
library(ggsidekick)

troll <-read.csv(here("Data/FOS-Troll-2005-2024.csv"), header=T)
iREC <-read.csv(here("Data/iREC-2012-2025.csv"), header=T)

# Area F troll ----
areaFtroll <- troll |>
  filter(MGMT_AREA %in% c(101:105)) |>
  group_by(CALENDAR_YEAR) |>
  summarise(coho=sum(COHO_KEPT, na.rm = T),
            effort=sum(BOAT_DAYS, na.rm = T)) |>
  mutate(CPUE = coho/effort,
         Year = CALENDAR_YEAR)

a <- ggplot(data = areaFtroll, aes(x=Year, y = coho/1000)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Coho catch (1000s)") +
  theme_sleek() 
  
b <- ggplot(data = areaFtroll, aes(x=Year, y = effort)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Effort (boat days)") +
  theme_sleek() 
  
c <- ggplot(data = areaFtroll, aes(x=Year, y = CPUE)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("CPUE (coho /boat day)") +
  theme_sleek() 
  
cowplot::plot_grid(a, b, c,  labels="auto", ncol=2)
ggsave("Figures/NC-troll-coho.jpeg", width = 9, height=6,units="in", dpi=600)

# iREC ----
CC_iREC_catch <- iREC |>
  filter(Logistical_Area == "Central Coast",
         Item == "Coho",
         Year < 2025) |>
  group_by(Year, Disposition) |>
  summarise(coho_catch=sum(ESTIMATE_CAL, na.rm = T)) 

CC_iREC_effort <- iREC |>
  filter(Logistical_Area == "Central Coast",
         Item == "Adult Effort",
         Year < 2025) |>
  group_by(Year) |>
  summarise(effort=sum(ESTIMATE_CAL, na.rm = T)) 

CC_iREC <- left_join(CC_iREC_catch |>
                       group_by(Year) |>
                       summarise(coho_catch=sum(coho_catch, na.rm = T))
                     , CC_iREC_effort, by= "Year") |>
  mutate(CPUE = coho_catch/effort)

a <- ggplot(data = CC_iREC_catch, aes(x=Year, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Coho catch (1000s)")  +
  theme_sleek() +
  theme(legend.position = c(0.75, 0.85))

b <- ggplot(data = CC_iREC, aes(x=Year, y = effort)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Effort (days)") +
  theme_sleek() 

c <- ggplot(data = CC_iREC, aes(x=Year, y = CPUE)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("CPUE (coho/day)") +
  theme_sleek() 

cowplot::plot_grid(a, b, c,  labels="auto", ncol=2)
ggsave("Figures/CC-rec-coho.jpeg", width = 9, height=6,units="in", dpi=600)


# troll and iRec catch over time ----

a_troll <- ggplot(data = areaFtroll, aes(x=Year, y = coho/1000)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Coho catch (1000s)") +
  theme_sleek() + 
  ggtitle("North Coast Troll")

# Reorder sub_category factor levels
CC_iREC_catch$Disposition <- fct_rev(CC_iREC_catch$Disposition)

b_iREC <- ggplot(data = CC_iREC_catch, aes(x=Year, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Coho catch (1000s)")  +
  theme_sleek() +
  theme(legend.position = c(0.75, 0.85))+ 
  ggtitle("Central Coast recreational")+
  scale_y_continuous(position = "right")

cowplot::plot_grid(a_troll,b_iREC,  labels="", ncol=2)
ggsave("Figures/NC-troll-CC-rec-coho.jpeg", width = 11, height=4,units="in", dpi=600)
