# ============================================================
# scripts/01_summary_statistics.R
# High-fatality events (>1,000 deaths) and descriptive
# summary statistics for floods and landslides.
# ============================================================

source("scripts/00_setup.R")

# ── Generate summary statistics ──────────────────────────────
generate_summary <- function(data) {
  known_death_data <- data |> filter(!is.na(total_deaths))

  if (nrow(known_death_data) == 0) {
    min_f <- q1_f <- med_f <- q3_f <- max_f <- NA_real_
  } else {
    min_f <- suppressWarnings(min(known_death_data$total_deaths, na.rm = TRUE))
    q1_f  <- as.numeric(quantile(known_death_data$total_deaths, 0.25, na.rm = TRUE))
    med_f <- median(known_death_data$total_deaths, na.rm = TRUE)
    q3_f  <- as.numeric(quantile(known_death_data$total_deaths, 0.75, na.rm = TRUE))
    max_f <- max(known_death_data$total_deaths, na.rm = TRUE)
  }

  data |>
    summarise(
      Fatalities                    = sum_known(total_deaths),
      Injured_People                = sum(no_injured,   na.rm = TRUE),
      Homeless_People               = sum(no_homeless,  na.rm = TRUE),
      Affected_People               = sum(no_affected,  na.rm = TRUE),
      Total_Affected_People         = sum(total_affected, na.rm = TRUE),
      Events_With_Known_Deaths      = sum(!is.na(total_deaths)),
      Events_Deaths_Missing         = sum(is.na(total_deaths)),
      Min_Fatalities                = min_f,
      Q1_Fatalities                 = q1_f,
      Median_Fatalities             = med_f,
      Q3_Fatalities                 = q3_f,
      Max_Fatalities                = max_f,
      Events_With_Known_Deaths_Per_Year =
        Events_With_Known_Deaths / n_distinct(start_year)
    )
}

# ── High-fatality events ─────────────────────────────────────
high_fatal <- thin |>
  filter(hazard_type %in% c("Flood", "Landslide"),
         !is.na(total_deaths),
         total_deaths > 1000) |>
  select(hazard_type, total_deaths, event_date)

openxlsx::write.xlsx(
  high_fatal,
  "output/tables/high_fatal_events_over_1000.xlsx",
  rowNames = FALSE
)

# ── Flood and landslide summaries ────────────────────────────
sum_flood <- thin |> filter(hazard_type == "Flood")     |> generate_summary()
sum_land  <- thin |> filter(hazard_type == "Landslide") |> generate_summary()

print("Flood Summary:");     print(sum_flood)
print("Landslide Summary:"); print(sum_land)

openxlsx::write.xlsx(sum_flood, "output/tables/summary_floods.xlsx",     rowNames = FALSE)
openxlsx::write.xlsx(sum_land,  "output/tables/summary_landslides.xlsx",  rowNames = FALSE)

message("01_summary_statistics.R complete.")
