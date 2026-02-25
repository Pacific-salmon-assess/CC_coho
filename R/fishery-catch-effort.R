library(plyr)
library(tidyverse)
library(here)
library(ggsidekick)

troll <- read.csv(here("Data/FOS-Troll-2005-2024.csv"), header=T)
iREC <- read.csv(here("Data/irec_A5_A9_coho_effort.csv"), header=T)
allFishery <- read.csv(here("Data/NC Coho Catch 1980-2025.csv"), header=T)

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
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM == "Coho",
         YEAR < 2026) |>
  mutate(Year=YEAR,
         Disposition=DISPOSITION) |>
  group_by(Year, Disposition) |>
  summarise(coho_catch=sum(ESTIMATE_CAL, na.rm = T)) 

CC_iREC_effort <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM != "Coho",
         YEAR < 2026) |>
  mutate(Year=YEAR,
         Disposition=DISPOSITION)|>
  group_by(Year) |>
  summarise(effort=sum(ESTIMATE_CAL, na.rm = T)) 

CC_iREC <- left_join(CC_iREC_catch |>
                       group_by(Year) |>
                       summarise(coho_catch=sum(coho_catch, na.rm = T))
                     , CC_iREC_effort, by= "Year") |>
  mutate(CPUE = coho_catch/effort)

CC_iREC_catch$Disposition <- fct_rev(CC_iREC_catch$Disposition)

a <- ggplot(data = CC_iREC_catch, aes(x=Year, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Coho catch (1000s)")  +
  theme_sleek() +
  theme(legend.position = c(0.7, 0.9),
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

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
  scale_y_continuous(position = "right")+
  scale_fill_brewer(palette = "Dark2")

cowplot::plot_grid(a_troll,b_iREC,  labels="", ncol=2)
ggsave("Figures/NC-troll-CC-rec-coho.jpeg", width = 11, height=4,units="in", dpi=600)


# all fishery catch ----

allFishery$fishery <- fct_rev(allFishery$fishery)


ggplot(data = allFishery, aes(x=year, y = harvest/1000, fill = fishery)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Coho catch (1000s)")  +
  theme_sleek() +
  theme(legend.position = c(0.75, 0.85),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = "Dark2")
ggsave("Figures/NC-coho-catch.jpeg", width = 7, height=5,units="in", dpi=600)


# fine scale iREC ----
CC_iREC_catch <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM == "Coho",
         YEAR < 2026,
         MONTH > 5,
         MONTH < 10, 
         AREA %in% c("Area 5", "Area 6", "Area 7", "Area 8", "Area 9")) |>
  mutate(Year=YEAR,
         Month=MONTH,
         Area=AREA,
         Disposition=DISPOSITION) |>
  group_by(Year, Month, Area, Disposition) |>
  summarise(coho_catch=sum(ESTIMATE_CAL, na.rm = T)) 

CC_iREC_catch$Disposition <- fct_rev(CC_iREC_catch$Disposition)
ggplot(data = CC_iREC_catch, aes(x=Month, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("Coho catch (1000s)")  +
  facet_grid(Area~Year, scales="free_y") +
  theme_sleek() +
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

ggsave("Figures/CC-rec-coho-finescale.jpeg", width = 11, height=8,units="in", dpi=600)


CC_iREC_effort <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM != "Coho",
         YEAR < 2026,
         MONTH > 5,
         MONTH < 10, 
         AREA %in% c("Area 5", "Area 6", "Area 7", "Area 8", "Area 9")) |>
  mutate(Year=YEAR,
         Month=MONTH,
         Area=AREA,
         Disposition=DISPOSITION) |>
  group_by(Year, Month, Area) |>
  summarise(effort=sum(ESTIMATE_CAL, na.rm = T)) 


CC_iREC_all_catch <- CC_iREC_catch |>
  group_by(Year, Month, Area) |>
  summarise(coho_catch_total=sum(coho_catch, na.rm = T)) 

CC_iREC_all_ret_rel <- left_join(CC_iREC_catch, CC_iREC_effort, by= c("Year", "Month", "Area")) |>
  mutate(CPUE = coho_catch/effort)


ggplot(data = CC_iREC_all_ret_rel, aes(x=Month, y = CPUE, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("CPUE (coho/day)")  +
  facet_grid(Area~Year, scales = "free_y") +
  theme_sleek() +
  geom_hline(yintercept = c(2,4),lty=2, col="grey") +
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

ggsave("Figures/CC-rec-coho-CPUE-finescale.jpeg", width = 11, height=8,units="in", dpi=600)

# iREC by type non guided non lodge----
CC_iREC_catch <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM == "Coho",
         YEAR < 2026) |>
  mutate(Year=YEAR,
         Disposition=DISPOSITION) |>
  group_by(Year, Disposition) |>
  summarise(coho_catch=sum(ESTIMATE_CAL_nG_nL, na.rm = T)) 

CC_iREC_effort <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM != "Coho",
         YEAR < 2026) |>
  mutate(Year=YEAR,
         Disposition=DISPOSITION)|>
  group_by(Year) |>
  summarise(effort=sum(ESTIMATE_CAL_nG_nL, na.rm = T)) 

CC_iREC <- left_join(CC_iREC_catch |>
                       group_by(Year) |>
                       summarise(coho_catch=sum(coho_catch, na.rm = T))
                     , CC_iREC_effort, by= "Year") |>
  mutate(CPUE = coho_catch/effort)

CC_iREC_catch$Disposition <- fct_rev(CC_iREC_catch$Disposition)

a <- ggplot(data = CC_iREC_catch, aes(x=Year, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Coho catch (1000s)")  +
  theme_sleek() +
  theme(legend.position = c(0.7, 0.9),
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

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
ggsave("Figures/CC-rec-coho-private.jpeg", width = 9, height=6,units="in", dpi=600)

# iREC by type guided and lodge----
CC_iREC_catch <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM == "Coho",
         YEAR < 2026) |>
  mutate(Year=YEAR,
         Disposition=DISPOSITION) |>
  group_by(Year, Disposition) |>
  summarise(coho_catch=sum(lodge-ch)) 

CC_iREC_effort <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM != "Coho",
         YEAR < 2026) |>
  mutate(Year=YEAR,
         Disposition=DISPOSITION)|>
  group_by(Year) |>
  summarise(effort=sum(c_across(c(ESTIMATE_CAL_G_L,ESTIMATE_CAL_G_nL,ESTIMATE_CAL_nG_L )))) 

CC_iREC <- left_join(CC_iREC_catch |>
                       group_by(Year) |>
                       summarise(coho_catch=sum(coho_catch, na.rm = T))
                     , CC_iREC_effort, by= "Year") |>
  mutate(CPUE = coho_catch/effort)

CC_iREC_catch$Disposition <- fct_rev(CC_iREC_catch$Disposition)

a <- ggplot(data = CC_iREC_catch, aes(x=Year, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Coho catch (1000s)")  +
  theme_sleek() +
  theme(legend.position = c(0.7, 0.9),
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

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
ggsave("Figures/CC-rec-coho-lodge_charter.jpeg", width = 9, height=6,units="in", dpi=600)

# fine scale iREC private vessels ----
CC_iREC_catch <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM == "Coho",
         YEAR < 2026,
         MONTH > 5,
         MONTH < 10, 
         AREA %in% c("Area 5", "Area 6", "Area 7", "Area 8", "Area 9")) |>
  mutate(Year=YEAR,
         Month=MONTH,
         Area=AREA,
         Disposition=DISPOSITION) |>
  group_by(Year, Month, Area, Disposition) |>
  summarise(coho_catch=sum(ESTIMATE_CAL_nG_nL, na.rm = T)) 

CC_iREC_catch$Disposition <- fct_rev(CC_iREC_catch$Disposition)
ggplot(data = CC_iREC_catch, aes(x=Month, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("Coho catch (1000s)")  +
  facet_grid(Area~Year, scales="free_y") +
  theme_sleek() +
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

ggsave("Figures/CC-rec-coho-finescale_private.jpeg", width = 11, height=8,units="in", dpi=600)


CC_iREC_effort <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM != "Coho",
         YEAR < 2026,
         MONTH > 5,
         MONTH < 10, 
         AREA %in% c("Area 5", "Area 6", "Area 7", "Area 8", "Area 9")) |>
  mutate(Year=YEAR,
         Month=MONTH,
         Area=AREA,
         Disposition=DISPOSITION) |>
  group_by(Year, Month, Area) |>
  summarise(effort=sum(ESTIMATE_CAL_nG_nL, na.rm = T)) 


CC_iREC_all_catch <- CC_iREC_catch |>
  group_by(Year, Month, Area) |>
  summarise(coho_catch_total=sum(coho_catch, na.rm = T)) 

CC_iREC_all_ret_rel <- left_join(CC_iREC_catch, CC_iREC_effort, by= c("Year", "Month", "Area")) |>
  mutate(CPUE = coho_catch/effort)


ggplot(data = CC_iREC_all_ret_rel, aes(x=Month, y = CPUE, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("CPUE (coho/day)")  +
  facet_grid(Area~Year, scales = "free_y") +
  theme_sleek() +
  geom_hline(yintercept = c(2,4),lty=2, col="grey") +
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

ggsave("Figures/CC-rec-coho-CPUE-finescale-private.jpeg", width = 11, height=8,units="in", dpi=600)

# fine scale iREC guide and lodge vessels ----
CC_iREC_catch <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM == "Coho",
         YEAR < 2026,
         MONTH > 5,
         MONTH < 10, 
         AREA %in% c("Area 5", "Area 6", "Area 7", "Area 8", "Area 9")) |>
  mutate(Year=YEAR,
         Month=MONTH,
         Area=AREA,
         Disposition=DISPOSITION) |>
  group_by(Year, Month, Area, Disposition) |>
  summarise(coho_catch=sum(c_across(c(ESTIMATE_CAL_G_L,ESTIMATE_CAL_G_nL,ESTIMATE_CAL_nG_L )))) 

CC_iREC_catch$Disposition <- fct_rev(CC_iREC_catch$Disposition)
ggplot(data = CC_iREC_catch, aes(x=Month, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("Coho catch (1000s)")  +
  facet_grid(Area~Year, scales="free_y") +
  theme_sleek() +
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

ggsave("Figures/CC-rec-coho-finescale-lodge-charter.jpeg", width = 11, height=8,units="in", dpi=600)


CC_iREC_effort <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM != "Coho",
         YEAR < 2026,
         MONTH > 5,
         MONTH < 10, 
         AREA %in% c("Area 5", "Area 6", "Area 7", "Area 8", "Area 9")) |>
  mutate(Year=YEAR,
         Month=MONTH,
         Area=AREA,
         Disposition=DISPOSITION) |>
  group_by(Year, Month, Area) |>
  summarise(effort=sum(c_across(c(ESTIMATE_CAL_G_L,ESTIMATE_CAL_G_nL,ESTIMATE_CAL_nG_L )))) 


CC_iREC_all_catch <- CC_iREC_catch |>
  group_by(Year, Month, Area) |>
  summarise(coho_catch_total=sum(coho_catch, na.rm = T)) 

CC_iREC_all_ret_rel <- left_join(CC_iREC_catch, CC_iREC_effort, by= c("Year", "Month", "Area")) |>
  mutate(CPUE = coho_catch/effort)


ggplot(data = CC_iREC_all_ret_rel, aes(x=Month, y = CPUE, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("CPUE (coho/day)")  +
  facet_grid(Area~Year, scales = "free_y") +
  theme_sleek() +
  geom_hline(yintercept = c(2,4),lty=2, col="grey") +
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

ggsave("Figures/CC-rec-coho-CPUE-finescale-lodge-charter.jpeg", width = 11, height=8,units="in", dpi=600)

# fine scale iREC UNCALIBRATED----
CC_iREC_catch <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM == "Coho",
         YEAR < 2026,
         MONTH > 5,
         MONTH < 10, 
         AREA %in% c("Area 5", "Area 6", "Area 7", "Area 8", "Area 9")) |>
  mutate(Year=YEAR,
         Month=MONTH,
         Area=AREA,
         Disposition=DISPOSITION) |>
  group_by(Year, Month, Area, Disposition) |>
  summarise(coho_catch=sum(ESTIMATE, na.rm = T)) 

CC_iREC_catch$Disposition <- fct_rev(CC_iREC_catch$Disposition)
ggplot(data = CC_iREC_catch, aes(x=Month, y = coho_catch/1000, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("Coho catch (1000s)")  +
  facet_grid(Area~Year, scales="free_y") +
  theme_sleek() +
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

ggsave("Figures/CC-rec-coho-finescale-raw.jpeg", width = 11, height=8,units="in", dpi=600)


CC_iREC_effort <- iREC |>
  filter(LOGISTICAL_AREA == "Central Coast",
         ITEM != "Coho",
         YEAR < 2026,
         MONTH > 5,
         MONTH < 10, 
         AREA %in% c("Area 5", "Area 6", "Area 7", "Area 8", "Area 9")) |>
  mutate(Year=YEAR,
         Month=MONTH,
         Area=AREA,
         Disposition=DISPOSITION) |>
  group_by(Year, Month, Area) |>
  summarise(effort=sum(ESTIMATE, na.rm = T)) 


CC_iREC_all_catch <- CC_iREC_catch |>
  group_by(Year, Month, Area) |>
  summarise(coho_catch_total=sum(coho_catch, na.rm = T)) 

CC_iREC_all_ret_rel <- left_join(CC_iREC_catch, CC_iREC_effort, by= c("Year", "Month", "Area")) |>
  mutate(CPUE = coho_catch/effort)


ggplot(data = CC_iREC_all_ret_rel, aes(x=Month, y = CPUE, fill = Disposition)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("CPUE (coho/day)")  +
  facet_grid(Area~Year, scales = "free_y") +
  theme_sleek() +
  geom_hline(yintercept = c(2,4),lty=2, col="grey") +
  theme(legend.position = "top",
        legend.title = element_blank())+
  scale_fill_brewer(palette = "Dark2")

ggsave("Figures/CC-rec-coho-CPUE-finescale-raw.jpeg", width = 11, height=8,units="in", dpi=600)