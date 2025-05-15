library(here)
library(tidyverse)
library(ggsidekick) 

# read in data ---------------------------------------------------------------------------

# escapement estimates ---
esc <- read.csv(here("Data/CC-coho-stream-reviews-combined.07-Apr-2025.csv")) |>
  mutate(original.spw = as.numeric(Original.Escapement.Estimate),
         revised.spw = as.numeric(Recommended.Estimate))|>
  select(Population, Stat.Area,CU,Year,original.spw,revised.spw) |>
  pivot_longer(cols = c(original.spw,revised.spw ), names_to = "estimates")
  
# exploitation rate reconstructions ---
er <- read.csv(here("Data/CO ERs by CUs.csv"))  |>
  mutate(CU = CUs,
         `Canada (CCFN)` = Can_ER_Avg,
         `Canada (English 2018)` = Can_ER_E,
         Alaska = AK_ER_E)|>
  select(CU, Year, Alaska, `Canada (English 2018)`, `Canada (CCFN)`) |>
  filter(CU %in% c("Hecate_Low","Douglas_Kitimat","North_Coast_Streams", "Bella_Coola_Dean","Rivers_Inlet")) |>
  pivot_longer(cols = c(Alaska, `Canada (English 2018)`, `Canada (CCFN)`), names_to = "Fishery",values_to = "er")

er$CU_f <- factor(er$CU, levels = c("Douglas_Kitimat", "North_Coast_Streams", "Hecate_Low", "Bella_Coola_Dean", "Rivers_Inlet"))

# escapement plot ----
ggplot(esc, aes(x = Year, y = value, col = estimates)) + 
  geom_line(lwd = 1.1) +
  xlab("Year") +
  ylab("Spawners (000s)") +
  facet_wrap(~Population, ncol=4, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  theme_sleek()  

ggsave(here("Figures/org-vs-revised-coho-spwn.PNG"), width=10, height=6.5, units = "in")


# er plot ----
ggplot(er |> filter(Fishery != "Canada (English 2018)"), aes(x = Year, y = er, fill = Fishery)) + 
  geom_area() +
  xlab("Year") +
  ylab("Harvest rate") +
  facet_wrap(~CU_f, ncol=2) +
  scale_fill_manual(values=c( "#E69F00", "#56B4E9")) +
  theme_sleek() +
  theme(legend.position = c(0.75,0.15),
        legend.title = element_text(size=11),
        legend.text = element_text(size=10)) 

ggsave(here("Figures/er-reconstruction-CCFN.PNG"), width=10, height=6.5, units = "in")

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