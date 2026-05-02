# ============================================================
# scripts/07_socioeconomic_analysis.R
# Income-group fatality patterns (aggregate + per-event) and
# the human-versus-economic loss divergence (2000-2025).
# ============================================================

source("scripts/00_setup.R")

# ── 7A. INCOME GROUP ANALYSIS ─────────────────────────────────
income <- readxl::read_xlsx(paths$world_bank_income_xlsx,
                            sheet = "List of economies") |>
  janitor::clean_names() |>
  mutate(code = toupper(trimws(code)), income_group = trimws(income_group)) |>
  filter(!is.na(code), nchar(code) == 3) |>
  transmute(iso = code, income_group)

unmatched_income <- anti_join(thin, income, by = c("iso" = "iso"))
if (nrow(unmatched_income) > 0) {
  message("Unmatched countries (no income group):\n",
          paste(unique(unmatched_income$country), collapse = ", "))
}

dis_inc <- thin |>
  left_join(income, by = c("iso" = "iso")) |>
  filter(!is.na(income_group), !is.na(hazard_type))

# Total fatalities by income group and hazard type
fatal_by_inc <- dis_inc |>
  group_by(income_group, hazard_type) |>
  summarise(total_fatalities = sum_known(total_deaths), .groups = "drop") |>
  mutate(hazard_type = factor(hazard_type, levels = c("Flood", "Landslide")))

income_order <- fatal_by_inc |>
  group_by(income_group) |>
  summarise(total_all_hazards = sum(total_fatalities, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_all_hazards)) |>
  pull(income_group)

fatal_by_inc <- fatal_by_inc |>
  mutate(income_group = factor(income_group, levels = income_order))

p_income_totals <- ggplot(fatal_by_inc,
    aes(x = income_group, y = total_fatalities, fill = hazard_type)) +
  geom_col(position = "dodge", color = "black", na.rm = TRUE) +
  facet_wrap(~hazard_type) +
  theme_minimal() +
  labs(title = "Total Fatalities by Income Group and Hazard Type",
       x = "Income Group", y = "Fatalities", fill = "Hazard Type") +
  theme(legend.position = "top", strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_income_totals)
ggsave("output/figures/fatalities_by_income_and_hazard.png", p_income_totals,
       dpi = figures$dpi, width = figures$width + 1, height = figures$height)

# Per-event summary by income group
fatal_per_event_income <- dis_inc |>
  group_by(income_group, hazard_type) |>
  summarise(
    Mean_Fatalities        = round(mean(total_deaths,   na.rm = TRUE), 1),
    Median_Fatalities      = round(median(total_deaths, na.rm = TRUE), 1),
    Events                 = n(),
    Events_Deaths_Missing  = sum(is.na(total_deaths)),
    .groups = "drop"
  ) |>
  mutate(income_group = factor(income_group, levels = income_order),
         hazard_type  = factor(hazard_type,  levels = c("Flood", "Landslide"))) |>
  arrange(hazard_type, desc(Mean_Fatalities))

print(fatal_per_event_income)

# Boxplot: distribution of fatalities per event
dis_inc_box <- dis_inc |>
  filter(!is.na(total_deaths)) |>
  mutate(income_group = factor(income_group, levels = income_order),
         hazard_type  = factor(hazard_type,  levels = c("Flood", "Landslide")))

p_income_box <- ggplot(dis_inc_box,
    aes(x = income_group, y = total_deaths, fill = hazard_type)) +
  geom_boxplot(outlier.colour = "red") +
  scale_y_log10() +
  facet_wrap(~hazard_type) +
  theme_minimal() +
  labs(title = "Distribution of Fatalities per Event by Income Group and Hazard Type",
       x = "Income Group", y = "Total Deaths (log scale)", fill = "Hazard Type") +
  theme(legend.position = "top", strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_income_box)
ggsave("output/figures/box_fatalities_by_income_and_hazard.png", p_income_box,
       dpi = figures$dpi, width = figures$width + 1, height = figures$height)

openxlsx::write.xlsx(fatal_by_inc,         "output/tables/fatalities_by_income_group_and_hazard.xlsx",      rowNames = FALSE)
openxlsx::write.xlsx(fatal_per_event_income,"output/tables/fatalities_per_event_by_income_group_and_hazard.xlsx", rowNames = FALSE)

# ── 7B. HUMAN VS ECONOMIC LOSS ────────────────────────────────
econ_human_data <- thin |>
  rename(damage_adj_000 = total_damage_adjusted_000_us) |>
  filter(!is.na(total_deaths), !is.na(damage_adj_000), damage_adj_000 > 0) |>
  mutate(
    Impact_Category = case_when(
      total_deaths >= 100 & damage_adj_000 <  10000  ~ "High Deaths, Low Cost",
      total_deaths <  10  & damage_adj_000 >= 100000 ~ "Low Deaths, High Cost",
      total_deaths >= 100 & damage_adj_000 >= 100000 ~ "High Deaths & High Cost",
      TRUE ~ "Other"
    )
  )

p_econ_human <- ggplot(econ_human_data,
    aes(x = damage_adj_000 * 1000, y = total_deaths, color = Impact_Category)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_x_log10(labels = scales::dollar_format(scale = 1e-6, suffix = "M")) +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title    = "Disaster Impact: Human vs Economic Losses",
    subtitle = "Events with known deaths and inflation-adjusted damage",
    x = "Adjusted Total Damage (US$, log)", y = "Total Fatalities (log)",
    color = "Impact Category"
  )

print(p_econ_human)
ggsave("output/figures/econ_vs_human.png", p_econ_human,
       dpi = figures$dpi, width = figures$width, height = figures$height)

openxlsx::write.xlsx(econ_human_data, "output/tables/economic_vs_human_loss_data.xlsx",
                     rowNames = FALSE)

message("07_socioeconomic_analysis.R complete.")
