###########################################
#### Retrospective coho analysis (final % change line graph)
#### Adds Bag 1 scenarios with 50% and 90% release reductions

setwd("C:/data/centralcoast")

library(readr)
library(dplyr)
library(ggplot2)

# -------------------------
# 1) Read data
kept_rel <- read_csv("irec_A6_A9_kept_released.csv", show_col_types = FALSE)
eff      <- read_csv("irec_A6_A9_boat_days.csv",      show_col_types = FALSE)

# -------------------------
# 2) Aggregate catch (use ESTIMATE + VARIANCE; NOT calibrated)
# Sum catch + variance by YEAR x MONTH x AREA x DISPOSITION
monthly_totals <- aggregate(
  cbind(total_catch = ESTIMATE, total_variance = VARIANCE) ~ YEAR + MONTH + AREA + DISPOSITION,
  data = kept_rel,
  FUN = function(x) sum(x, na.rm = TRUE)
)

# Wide format: separate Kept / Released columns (catch)
monthly_wide_catch <- reshape(
  monthly_totals[, c("YEAR","MONTH","AREA","DISPOSITION","total_catch")],
  idvar   = c("YEAR","MONTH","AREA"),
  timevar = "DISPOSITION",
  direction = "wide"
)

# Wide format: separate Kept / Released columns (variance)
monthly_wide_var <- reshape(
  monthly_totals[, c("YEAR","MONTH","AREA","DISPOSITION","total_variance")],
  idvar   = c("YEAR","MONTH","AREA"),
  timevar = "DISPOSITION",
  direction = "wide"
)

# Merge catch + variance wide tables
monthly_wide <- merge(monthly_wide_catch, monthly_wide_var,
                      by = c("YEAR","MONTH","AREA"), all = TRUE)

# Replace NA with 0 for convenience
monthly_wide[is.na(monthly_wide)] <- 0

# Clean up column names
names(monthly_wide) <- sub("^total_catch\\.", "catch_", names(monthly_wide))
names(monthly_wide) <- sub("^total_variance\\.", "var_", names(monthly_wide))
names(monthly_wide) <- tolower(names(monthly_wide))

# -------------------------
# 3) Aggregate effort (fisher_days = ESTIMATE; boat_trips = ESTIMATE_CAL)
eff_monthly <- aggregate(
  cbind(fisher_days = ESTIMATE, boat_trips = ESTIMATE_CAL) ~ YEAR + MONTH + AREA,
  data = eff,
  FUN = function(x) sum(x, na.rm = TRUE)
)
names(eff_monthly) <- tolower(names(eff_monthly))

# -------------------------
# 4) Merge catch + effort into panel
panel <- merge(monthly_wide, eff_monthly,
               by = c("year","month","area"),
               all.x = TRUE)

# CPUE (optional diagnostics)
panel$cpue_kept_fd <- ifelse(panel$fisher_days > 0, panel$catch_kept / panel$fisher_days, NA_real_)
panel$cpue_rel_fd  <- ifelse(panel$fisher_days > 0, panel$catch_released / panel$fisher_days, NA_real_)

# -------------------------
# 5) Build August scenarios
aug <- subset(panel, month == 8)

K <- aug[["catch_kept"]]
R <- aug[["catch_released"]]
E <- aug[["fisher_days"]]
Enc <- K + R

# Bag limits and release reduction assumptions
bag_limits <- c(2, 1)

# NOTE: release_reduction is the assumed proportional reduction in releases due to stop-fishing compliance
# We include 0%, 50%, 90% for both bag=2 and bag=1
release_reductions <- c(0, 0.5, 0.9)

# Post-release mortality assumptions (you can change later if needed)
prm <- c(0.2, 0.3, 0.4)

results <- vector("list", length(bag_limits) * length(release_reductions) * length(prm))
idx <- 1

for (L in bag_limits) {
  # kept under cap
  K_cap <- pmin(K, L * E)
  
  # fish not kept (potential releases if encounters are held constant)
  R_star <- pmax(Enc - K_cap, 0)
  
  for (red in release_reductions) {
    # compliance reduces releases
    R_new <- R_star * (1 - red)
    
    for (m in prm) {
      mort <- K_cap + m * R_new
      
      results[[idx]] <- data.frame(
        year = aug[["year"]],
        area = aug[["area"]],
        month = aug[["month"]],
        bag_limit = L,
        release_reduction = red,
        post_release_mortality = m,
        effort_fisher_days = E,
        kept_obs = K,
        rel_obs = R,
        kept_pred = K_cap,
        rel_pred = R_new,
        mortality_pred = mort
      )
      idx <- idx + 1
    }
  }
}

aug_scenarios <- do.call(rbind, results)

# -------------------------
# 6) Compute % change in mortality vs baseline (Scenario A)
# Choose which post-release mortality to display
m0 <- 0.3

# Baseline A (observed)
baseline_A <- aug_scenarios %>%
  filter(year >= 2013, year <= 2025,
         area %in% c("Area 6","Area 7")) %>%
  distinct(year, area, kept_obs, rel_obs) %>%
  mutate(mort_A = kept_obs + m0 * rel_obs) %>%
  select(year, area, mort_A)

# Scenario list for plotting (B–G; excludes baseline A)
scenario_spec <- tibble::tribble(
  ~bag_limit, ~release_reduction, ~scenario,
  2,          0.0,                "B: Bag 2, 0% release reduction",
  2,          0.5,                "C: Bag 2, 50% release reduction",
  2,          0.9,                "D: Bag 2, 90% release reduction",
  1,          0.0,                "E: Bag 1, 0% release reduction",
  1,          0.5,                "F: Bag 1, 50% release reduction",
  1,          0.9,                "G: Bag 1, 90% release reduction"
)

scen_BG <- aug_scenarios %>%
  filter(year >= 2013, year <= 2025,
         area %in% c("Area 6","Area 7"),
         post_release_mortality == m0) %>%
  inner_join(scenario_spec, by = c("bag_limit", "release_reduction")) %>%
  mutate(mort_scen = kept_pred + m0 * rel_pred) %>%
  select(year, area, scenario, mort_scen)

pct_df <- scen_BG %>%
  inner_join(baseline_A, by = c("year","area")) %>%
  mutate(
    pct_change = ifelse(mort_A > 0, 100 * (mort_scen - mort_A) / mort_A, NA_real_)
  )

# Force facet order (Area 6 top, Area 7 bottom)
pct_df$area <- factor(pct_df$area, levels = c("Area 6","Area 7"))

# Optional: lock scenario ordering in legend
pct_df$scenario <- factor(
  pct_df$scenario,
  levels = scenario_spec$scenario
)

# -------------------------
# 7) Plot final graph (line plot, % change)
ggplot(pct_df, aes(x = year, y = pct_change, color = scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~ area, ncol = 1) +
  scale_x_continuous(breaks = 2013:2025) +
  labs(
    x = "Year",
    y = paste0("% change in August coho mortality"),
    color = "Scenario"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
