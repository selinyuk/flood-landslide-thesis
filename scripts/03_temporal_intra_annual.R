# ============================================================
# scripts/03_temporal_intra_annual.R
# Seasonal and monthly distribution of events and fatalities
# by hemisphere (2000-2025).
# ============================================================

source("scripts/00_setup.R")

month_levels <- 1:12
month_labels <- month.abb

# ── 3A. SEASONAL ANALYSIS ────────────────────────────────────
seasonal_data <- thin |>
  assign_hemisphere() |>
  assign_season() |>
  filter(!is.na(season), !is.na(total_deaths)) |>
  group_by(hemisphere, hazard_type, season) |>
  summarise(total_fatalities = sum(total_deaths, na.rm = TRUE), .groups = "drop") |>
  mutate(
    season      = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
    hazard_type = factor(hazard_type, levels = c("Flood", "Landslide"))
  ) |>
  group_by(hemisphere, hazard_type) |>
  mutate(
    total_hazard_fatalities = sum(total_fatalities, na.rm = TRUE),
    Fatality_Percentage     = ifelse(
      total_hazard_fatalities > 0,
      (total_fatalities / total_hazard_fatalities) * 100,
      NA_real_
    )
  ) |>
  ungroup()

p_season <- ggplot(
  seasonal_data,
  aes(x = season, y = Fatality_Percentage, fill = hazard_type)
) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "black") +
  facet_wrap(~hemisphere, ncol = 1) +
  scale_fill_manual(values = c("Flood" = "black", "Landslide" = "white")) +
  labs(
    title = "Seasonal Share of Fatalities by Hemisphere",
    x = "Season", y = "Percentage of Fatalities", fill = "Hazard"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text  = element_text(face = "bold"),
    plot.title  = element_text(face = "bold", hjust = 0.5)
  )

print(p_season)
ggsave("output/figures/season_fatalities_pct_by_hemisphere.png", p_season,
       dpi = figures$dpi, width = 8, height = 7)
openxlsx::write.xlsx(seasonal_data, "output/tables/seasonal_fatalities_by_hemisphere.xlsx",
                     rowNames = FALSE)

# ── 3B. MONTHLY ANALYSIS ─────────────────────────────────────
monthly_data <- thin |>
  filter(!is.na(start_month)) |>
  assign_hemisphere() |>
  mutate(
    start_month = as.integer(start_month),
    hazard_type = factor(hazard_type, levels = c("Flood", "Landslide"))
  ) |>
  group_by(hemisphere, hazard_type, start_month) |>
  summarise(Num_Events = n(), Total_Fatalities = sum_known(total_deaths), .groups = "drop") |>
  complete(hemisphere, hazard_type, start_month = 1:12,
           fill = list(Num_Events = 0, Total_Fatalities = 0)) |>
  group_by(hemisphere, hazard_type) |>
  mutate(
    Event_Total    = sum(Num_Events, na.rm = TRUE),
    Fatality_Total = sum(Total_Fatalities, na.rm = TRUE),
    Event_Percentage    = ifelse(Event_Total > 0, (Num_Events / Event_Total) * 100, NA_real_),
    Fatality_Percentage = ifelse(Fatality_Total > 0, (Total_Fatalities / Fatality_Total) * 100, NA_real_),
    Monthly_Frequency_Ratio = ifelse(
      !is.na(Event_Percentage) & Event_Percentage > 0 & !is.na(Fatality_Percentage),
      Fatality_Percentage / Event_Percentage, NA_real_
    )
  ) |>
  ungroup() |>
  mutate(start_month_f = factor(start_month, levels = month_levels, labels = month_labels))

# Interannual SD of monthly fatalities
monthly_sd <- thin |>
  filter(!is.na(start_month)) |>
  assign_hemisphere() |>
  mutate(start_month = as.integer(start_month),
         hazard_type = factor(hazard_type, levels = c("Flood", "Landslide"))) |>
  group_by(hemisphere, hazard_type, start_year, start_month) |>
  summarise(yearly_fatalities = sum_known(total_deaths), .groups = "drop") |>
  complete(hemisphere, hazard_type, start_year, start_month = 1:12,
           fill = list(yearly_fatalities = 0)) |>
  group_by(hemisphere, hazard_type, start_month) |>
  summarise(Fatality_SD_Abs = stats::sd(yearly_fatalities, na.rm = TRUE), .groups = "drop")

monthly_data <- monthly_data |>
  left_join(monthly_sd, by = c("hemisphere", "hazard_type", "start_month"))

# Shared theme and fill scale
base_theme_month <- theme_minimal() +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5))
fill_scale_month <- scale_fill_manual(values = c("Flood" = "black", "Landslide" = "white"))

# Plot 1: monthly fatality share
p_month_fatalities_pct <- ggplot(
  monthly_data,
  aes(x = start_month_f, y = Fatality_Percentage, fill = hazard_type)
) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "black", na.rm = TRUE) +
  facet_wrap(~hemisphere, ncol = 1) +
  fill_scale_month +
  labs(title = "Monthly Share of Fatalities by Hemisphere",
       x = "Month", y = "Percentage of Fatalities", fill = "Hazard") +
  base_theme_month

# Plot 2: absolute monthly fatalities with SD error bars
p_month_fatalities_abs <- ggplot(
  monthly_data,
  aes(x = start_month_f, y = Total_Fatalities, fill = hazard_type)
) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "black", na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = pmax(Total_Fatalities - Fatality_SD_Abs, 0),
        ymax = Total_Fatalities + Fatality_SD_Abs),
    position = position_dodge(width = 0.85), width = 0.2, color = "red", na.rm = TRUE
  ) +
  facet_wrap(~hemisphere, ncol = 1, scales = "free_y") +
  fill_scale_month +
  labs(title = "Absolute Monthly Fatalities by Hemisphere",
       subtitle = "Red error bars show interannual standard deviation",
       x = "Month", y = "Fatalities", fill = "Hazard") +
  base_theme_month

# Plot 3: monthly fatality frequency ratio
p_month_ratio <- ggplot(
  monthly_data,
  aes(x = start_month_f, y = Monthly_Frequency_Ratio, fill = hazard_type)
) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8, color = "black", na.rm = TRUE) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  facet_wrap(~hemisphere, ncol = 1) +
  fill_scale_month +
  labs(title = "Monthly Fatality Frequency Ratio by Hemisphere",
       subtitle = "Dashed line at 1 indicates fatalities proportional to event frequency",
       x = "Month", y = "Fatality Frequency Ratio", fill = "Hazard") +
  base_theme_month

print(p_month_fatalities_pct)
print(p_month_fatalities_abs)
print(p_month_ratio)

ggsave("output/figures/month_fatalities_pct.png", p_month_fatalities_pct,
       dpi = figures$dpi, width = 8, height = 7)
ggsave("output/figures/month_fatalities_abs.png", p_month_fatalities_abs,
       dpi = figures$dpi, width = 8, height = 7)
ggsave("output/figures/month_ratio.png", p_month_ratio,
       dpi = figures$dpi, width = 8, height = 7)

openxlsx::write.xlsx(monthly_data, "output/tables/monthly_summary_by_hemisphere.xlsx",
                     rowNames = FALSE)

message("03_temporal_intra_annual.R complete.")
