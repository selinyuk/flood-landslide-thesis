# ============================================================
# scripts/04_temporal_long_term.R
# Annual event frequency, total fatalities, average fatalities
# per event, and Poisson regression trend model (2000-2025).
# ============================================================

source("scripts/00_setup.R")

# ── Annual summaries ─────────────────────────────────────────
yearly_data <- thin |>
  group_by(start_year, hazard_type) |>
  summarise(
    Num_Events              = n(),
    Total_Fatalities        = sum_known(total_deaths),
    Avg_Fatalities_Per_Event = ifelse(
      !is.na(Total_Fatalities) & Num_Events > 0,
      Total_Fatalities / Num_Events,
      NA_real_
    ),
    .groups = "drop"
  )

# ── Poisson regression on event counts ───────────────────────
yearly_counts <- thin |>
  group_by(start_year, hazard_type) |>
  summarise(Num_Events = n(), .groups = "drop") |>
  mutate(hazard_type = factor(hazard_type, levels = c("Flood", "Landslide")))

poisson_model <- glm(
  Num_Events ~ start_year * hazard_type,
  family = poisson(link = "log"),
  data   = yearly_counts
)

coef_table <- summary(poisson_model)$coefficients
ci_table   <- confint.default(poisson_model)

trend_results <- tibble(
  term         = rownames(coef_table),
  estimate     = unname(coef_table[, "Estimate"]),
  std_error    = unname(coef_table[, "Std. Error"]),
  z_value      = unname(coef_table[, "z value"]),
  p_value      = unname(coef_table[, "Pr(>|z|)"]),
  ci_lower     = unname(ci_table[, 1]),
  ci_upper     = unname(ci_table[, 2]),
  irr          = exp(estimate),
  irr_ci_lower = exp(ci_lower),
  irr_ci_upper = exp(ci_upper)
)

overdispersion_value <- sum(residuals(poisson_model, type = "pearson")^2) /
  df.residual(poisson_model)

poisson_model_info <- tibble(
  model          = "Poisson event-count model with year, hazard type, and interaction",
  formula        = "Num_Events ~ start_year * hazard_type",
  overdispersion = overdispersion_value
)

print("Poisson regression coefficients:"); print(trend_results)
print("Poisson model info:");             print(poisson_model_info)

openxlsx::write.xlsx(trend_results,      "output/tables/trend_events_poisson_regression.xlsx", rowNames = FALSE)
openxlsx::write.xlsx(poisson_model_info, "output/tables/trend_events_poisson_model_info.xlsx", rowNames = FALSE)

# ── Predicted trend lines with CI ────────────────────────────
trend_grid <- expand.grid(
  start_year  = seq(min(yearly_counts$start_year), max(yearly_counts$start_year)),
  hazard_type = levels(yearly_counts$hazard_type)
)

pred_link <- predict(poisson_model, newdata = trend_grid, type = "link", se.fit = TRUE)
trend_grid$predicted_events <- exp(pred_link$fit)
trend_grid$ci_lower         <- exp(pred_link$fit - 1.96 * pred_link$se.fit)
trend_grid$ci_upper         <- exp(pred_link$fit + 1.96 * pred_link$se.fit)

# ── Plots ─────────────────────────────────────────────────────
p_ts_fatalities <- ggplot(yearly_data,
    aes(x = start_year, y = Total_Fatalities, color = hazard_type)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  scale_color_discrete(name = "Hazard Type") +
  theme_minimal() +
  labs(title = "Total Fatalities per Year", x = "Year", y = "Fatalities")

p_ts_avg <- ggplot(yearly_data,
    aes(x = start_year, y = Avg_Fatalities_Per_Event, color = hazard_type)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(na.rm = TRUE) +
  scale_color_discrete(name = "Hazard Type") +
  theme_minimal() +
  labs(title = "Average Fatalities per Event per Year",
       x = "Year", y = "Fatalities per Event")

p_ts_trend <- ggplot(yearly_counts,
    aes(x = start_year, y = Num_Events, color = hazard_type)) +
  geom_point() +
  geom_ribbon(data = trend_grid,
    aes(x = start_year, ymin = ci_lower, ymax = ci_upper, fill = hazard_type),
    alpha = 0.2, inherit.aes = FALSE) +
  geom_line(data = trend_grid,
    aes(x = start_year, y = predicted_events, color = hazard_type),
    linewidth = 1.1) +
  scale_color_discrete(name = "Hazard Type") +
  scale_fill_discrete(name  = "Hazard Type") +
  theme_minimal() +
  labs(title    = "Trend in Number of Events (Poisson Regression)",
       subtitle = "Fitted lines with 95% confidence intervals",
       x = "Year", y = "Events")

print(p_ts_fatalities); print(p_ts_avg); print(p_ts_trend)

ggsave("output/figures/ts_fatalities.png",          p_ts_fatalities, dpi = figures$dpi, width = figures$width, height = figures$height)
ggsave("output/figures/ts_avg_fatalities.png",      p_ts_avg,        dpi = figures$dpi, width = figures$width, height = figures$height)
ggsave("output/figures/ts_trend_events_poisson.png",p_ts_trend,      dpi = figures$dpi, width = figures$width, height = figures$height)

message("04_temporal_long_term.R complete.")
