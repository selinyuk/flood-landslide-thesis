# ============================================================
# scripts/06_flood_landslide_correlation.R
# Log-log quantile regression (tau = 0.5) comparing country-
# level flood and landslide fatalities, with population size
# colour coding (2000-2025).
# ============================================================

source("scripts/00_setup.R")

# ── Country-wide totals (wide format) ────────────────────────
ctry_totals <- thin |>
  group_by(country, iso, hazard_type) |>
  summarise(total_fatalities = sum_known(total_deaths), .groups = "drop")

ctry_wide <- ctry_totals |>
  pivot_wider(names_from = hazard_type, values_from = total_fatalities,
              values_fill = NA_real_) |>
  rename(flood_total = Flood, landslide_total = Landslide)

# ── Population data ───────────────────────────────────────────
pop <- read_population_csv(paths$population_csv) |>
  filter(`Series Code` == "SP.POP.TOTL") |>
  transmute(
    iso            = toupper(trimws(`Country Code`)),
    population_2025 = as.numeric(`2025 [YR2025]`)
  ) |>
  filter(!is.na(iso), nchar(iso) == 3)

ctry_compare <- ctry_wide |>
  left_join(pop, by = "iso") |>
  filter(
    !is.na(flood_total), !is.na(landslide_total),
    flood_total > 0, landslide_total > 0,
    !is.na(population_2025), population_2025 > 0
  ) |>
  mutate(
    iso3          = iso,
    log_flood     = log10(flood_total),
    log_landslide = log10(landslide_total)
  )

# ── Median quantile regression (tau = 0.5) ────────────────────
rq_model  <- quantreg::rq(log_landslide ~ log_flood, tau = 0.5, data = ctry_compare)
rq_summary <- summary(rq_model, se = "nid")

quantile_results <- tibble::tibble(
  term      = rownames(rq_summary$coefficients),
  estimate  = rq_summary$coefficients[, 1],
  std_error = rq_summary$coefficients[, 2],
  t_value   = rq_summary$coefficients[, 3],
  p_value   = rq_summary$coefficients[, 4]
)
print(quantile_results)
openxlsx::write.xlsx(quantile_results,
  "output/tables/quantile_regression_loglog_flood_vs_landslide.xlsx", rowNames = FALSE)

# ── 95% confidence band ───────────────────────────────────────
pred_data <- tibble::tibble(
  log_flood = seq(min(ctry_compare$log_flood), max(ctry_compare$log_flood), length.out = 100)
)
pred_ci <- predict(rq_model, newdata = pred_data, interval = "confidence", level = 0.95)
pred_data <- pred_data |>
  mutate(fit = pred_ci[, "fit"], lower = pred_ci[, "lower"], upper = pred_ci[, "higher"])

# ── Scatter plot ──────────────────────────────────────────────
p_scatter_quantile <- ggplot(ctry_compare, aes(x = log_flood, y = log_landslide)) +
  geom_ribbon(data = pred_data,
    aes(x = log_flood, ymin = lower, ymax = upper),
    inherit.aes = FALSE, fill = "red", alpha = 0.15) +
  geom_line(data = pred_data,
    aes(x = log_flood, y = fit),
    inherit.aes = FALSE, color = "red", linewidth = 1) +
  geom_point(aes(color = population_2025), size = 2.5, alpha = 0.8) +
  ggrepel::geom_text_repel(aes(label = iso3), size = 3, max.overlaps = 100,
    box.padding = 0.3, point.padding = 0.2, segment.color = "grey50") +
  scale_color_viridis_c(
    option = "plasma", trans = "log10", name = "Population (2025)",
    labels = scales::label_number(scale_cut = scales::cut_short_scale())
  ) +
  labs(
    title    = "Flood vs Landslide Fatalities by Country (EM-DAT, 2000-2025)",
    subtitle = "Point colour = population size; red line = median quantile regression (tau = 0.5); shaded band = 95% CI",
    x = "Log10 Flood Fatalities", y = "Log10 Landslide Fatalities"
  ) +
  theme_minimal()

print(p_scatter_quantile)

ggsave(
  "output/figures/scatter_flood_vs_landslide_quantile_loglog_population.png",
  p_scatter_quantile,
  dpi = figures$dpi, width = figures$width, height = figures$height
)

message("06_flood_landslide_correlation.R complete.")
