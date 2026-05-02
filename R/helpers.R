# ============================================================
# R/helpers.R
# Shared helper functions for the flood & landslide analysis
# ============================================================

# ── Northern-hemisphere ISO3 country codes ───────────────────
northern_iso3 <- c(
  "AFG","ALB","ARE","ARM","AZE","BGD","BIH","BLR","BTN","BWA",
  "CHN","CMR","COD","CUB","CYP","DJI","DOM","DZA","EGY","ERI",
  "ETH","GEO","GIN","GRC","HND","HTI","IDN","IND","IRN","IRQ",
  "ISR","ITA","JOR","JPN","KAZ","KEN","KGZ","KWT","LBN","LBY",
  "LKA","MAR","MDG","MEX","MHL","MLI","MMR","MNG","MRT","NER",
  "NGA","NPL","OMN","PAK","PHL","PRK","PSE","QAT","ROU","RUS",
  "SAU","SDN","SEN","SLB","SLE","SOM","SSD","SYR","TCD","TGO",
  "THA","TJK","TKM","TUN","TUR","TZA","UKR","UZB","VEN","VNM","YEM"
)

# ── General utilities ────────────────────────────────────────

assert_file_exists <- function(path) {
  if (!file.exists(path)) stop(sprintf("File not found: %s", path))
}

# log10(x + 1) so zero values do not break log-style displays
safe_log10 <- function(x, offset = 1) {
  ifelse(is.na(x), NA_real_, log10(pmax(x, 0) + offset))
}

# Sum known values; if everything is missing keep NA
sum_known <- function(x) {
  if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
}

# ── Data readers ─────────────────────────────────────────────

read_emdat_xlsx <- function(path, sheet) {
  assert_file_exists(path)
  df <- readxl::read_excel(path, sheet = sheet)
  names(df) <- janitor::make_clean_names(names(df))
  df
}

read_population_csv <- function(path) {
  assert_file_exists(path)
  readr::read_csv(path, show_col_types = FALSE)
}

# ── Data preparation ─────────────────────────────────────────

make_event_date <- function(df) {
  df |>
    dplyr::mutate(
      start_year  = as.integer(start_year),
      start_month = as.integer(start_month),
      start_day   = dplyr::if_else(is.na(start_day), 1L, as.integer(start_day)),
      event_date  = as.Date(sprintf("%04d-%02d-%02d", start_year, start_month, start_day))
    )
}

# Collapse EM-DAT hazard types into Flood / Landslide
collapse_hazard <- function(df) {
  df |>
    dplyr::mutate(
      hazard_type = dplyr::case_when(
        disaster_type == "Flood" ~ "Flood",
        disaster_type %in% c("Mass movement (dry)", "Mass movement (wet)") ~ "Landslide",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(hazard_type))
}

assign_hemisphere <- function(df) {
  df |>
    dplyr::mutate(
      hemisphere = dplyr::if_else(iso %in% northern_iso3, "Northern", "Southern")
    )
}

assign_season <- function(df) {
  df |>
    dplyr::mutate(
      start_month = as.integer(start_month),
      season = dplyr::case_when(
        (start_month %in% c(12, 1, 2) & hemisphere == "Northern") |
          (start_month %in% c(6, 7, 8)  & hemisphere == "Southern") ~ "Winter",
        (start_month %in% c(3, 4, 5)  & hemisphere == "Northern") |
          (start_month %in% c(9, 10, 11) & hemisphere == "Southern") ~ "Spring",
        (start_month %in% c(6, 7, 8)  & hemisphere == "Northern") |
          (start_month %in% c(12, 1, 2) & hemisphere == "Southern") ~ "Summer",
        (start_month %in% c(9, 10, 11) & hemisphere == "Northern") |
          (start_month %in% c(3, 4, 5)  & hemisphere == "Southern") ~ "Autumn",
        TRUE ~ NA_character_
      )
    )
}
