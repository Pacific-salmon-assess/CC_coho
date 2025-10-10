SR.dat<-read.csv(here("Data/Coho_Brood_MASTER2025Update.csv"), header=T)
co_pops<-read.table("Data/coho_groups_2025.txt",header=TRUE)
spawn_data <- left_join(SR.dat,co_pops|>select(pop_no,group),by="pop_no")

pop_group <- spawn_data |>
  filter(!is.na(total_run4),
         !is.na(escapement),
         group==7) |>
  mutate(spawners = as.numeric(escapement),
         returns = as.numeric(total_run4))|>
  select(year, group,population, spawners, returns)


ggplot(pop_group,aes(x = year, y = spawners)) +
  geom_smooth(method="lm", color="grey") +
  geom_point(size=2, color="dark grey")+
  xlab("Year") +
  ylab("Spawners") +
  theme_minimal() +
  facet_wrap(~population, scales = "free_y", ncol = 4) +
  theme(axis.title = element_text(size=12))  


pop_group_short<- pop_group|>
  filter(year>1999)

ggplot(pop_group,aes(x = year, y = returns)) +
  geom_smooth(data=pop_group_short,  method="lm", color="grey") +
  geom_point(size=2, color="dark grey")+
  xlab("Year") +
  ylab("Returns") +
  theme_minimal() +
  facet_wrap(~population, scales = "free_y", ncol = 4) +
  theme(axis.title = element_text(size=12))  
