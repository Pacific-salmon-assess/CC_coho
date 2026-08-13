library(here)
library(tidyverse)
library(ggsidekick) 

# read in data ---------------------------------------------------------------------------

# escapement estimates ---
esc <- read.csv(here("Data/master-CC-coho-stream-escapement.Aug2026.csv")) |>
  mutate('Low' = as.numeric(Database.Estimate),
         'Medium-High' = as.numeric(Recommended.Estimate))|>
  select(Population, Stat.Area,CU,Year,'Low','Medium-High') |>
  pivot_longer(cols = c('Low','Medium-High'), names_to = "Quality")
  
# exploitation rate reconstructions ---
er <- read.csv(here("Data/CO ERs by CUs.csv"))  |>
  mutate(CU = CUs,
         `Canada (CCFN)` = Can_ER_Avg,
         `Canada (English 2018)` = Can_ER_E,
         Alaska = AK_ER_E)|>
  select(CU, Year, Alaska, `Canada (English 2018)`, `Canada (CCFN)`) |>
  filter(CU %in% c("Hecate_Low","North_Coast_Streams", "Bella_Coola_Dean")) |>
  pivot_longer(cols = c(Alaska, `Canada (English 2018)`, `Canada (CCFN)`), names_to = "Fishery",values_to = "er") |>
  mutate(
    area = case_when(
      CU == "Hecate_Low" ~ "Hecate Lowlands",
      CU == "North_Coast_Streams" ~ "Inner Waters",
      CU == "Bella_Coola_Dean" ~ "Central Coast (South)",
    )
  )

er$area_f <- factor(er$area, levels = c("Inner Waters", "Hecate Lowlands", "Central Coast (South)"))

# escapement plot ----
pop_order <- esc %>%
  distinct(Population, Stat.Area) %>%
  arrange(Stat.Area) %>%
  mutate(
    Population_Label = paste0(Population, " (", Stat.Area, ")")
  )

esc_ordered <- esc %>%
  left_join(pop_order, by = c("Population", "Stat.Area")) %>%
  mutate(
    Population_Label = factor(
      Population_Label,
      levels = pop_order$Population_Label
    )
  )

ggplot(esc_ordered, aes(x = Year, y = value, col = Quality)) + 
  geom_line(lwd = 1.1) +
  xlab("Year") +
  ylab("Spawners (000s)") +
  facet_wrap(~Population_Label, ncol=4, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  theme_sleek()  

ggsave(here("Figures/escapement-review.Aug2026.PNG"), width=9, height=11, units = "in")


# er plot ----
ggplot(er |> filter(Fishery != "Canada (English 2018)"), aes(x = Year, y = er, fill = Fishery)) + 
  geom_area() +
  xlab("Year") +
  ylab("Harvest rate") +
  facet_wrap(~area_f, ncol=1) +
  scale_fill_manual(values=c( "#E69F00", "#56B4E9")) +
  theme_sleek() +
  theme(legend.position = c(0.9,0.95),
        legend.title = element_text(size=9),
        legend.text = element_text(size=8)) 

ggsave(here("Figures/er-reconstruction-CCFN.PNG"), width=6.5, height=6.5, units = "in")

ggplot(er |> filter(Fishery != "Canada (CCFN)"), aes(x = Year, y = er, fill = Fishery)) + 
  geom_area() +
  xlab("Year") +
  ylab("Harvest rate") +
  facet_wrap(~CU_f, ncol=2) +
  scale_fill_manual(values=c( "#E69F00", "#56B4E9")) +
  theme_sleek() +
  theme(legend.position = c(0.75,0.15),
        legend.title = element_text(size=11),
        legend.text = element_text(size=10))
        

ggsave(here("Figures/er-reconstruction-english-2018.PNG"), width=10, height=6.5, units = "in")