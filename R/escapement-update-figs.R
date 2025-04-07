library(here)
library(tidyverse)
library(ggsidekick) #for theme_sleek() - doesn't work with some vs of R, hence the comment

# read in data ---------------------------------------------------------------------------

# escapement estimates ---
esc <- read.csv(here("Data/CC-coho-stream-reviews-combined.07-Apr-2025.csv")) |>
  mutate(original.spw = as.numeric(Original.Escapement.Estimate),
         revised.spw = as.numeric(Recommended.Estimate))|>
  select(Population, Stat.Area,CU,Year,original.spw,revised.spw) |>
  pivot_longer(cols = c(original.spw,revised.spw ), names_to = "estimates")
  


ggplot(esc, aes(x = Year, y = value, col = estimates)) + 
  geom_line(lwd = 1.1) +
  xlab("Year") +
  ylab("Spawners (000s)") +
  facet_wrap(~Population, ncol=4, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values=c( "#E69F00", "#56B4E9")) +
  theme_sleek()  

ggsave(here("Figures/org-vs-revised-coho-spwn.PNG"), width=10, height=6.5, units = "in")
