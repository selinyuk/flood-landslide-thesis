# ============================================================
# scripts/00_setup.R
# Load packages, read config, source helpers, and prepare the
# master "thin" dataset used by all downstream scripts.
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(countrycode)
  library(maps)
  library(openxlsx)
  library(scales)
  library(janitor)
  library(yaml)
  library(tibble)
  library(quantreg)
  library(ggrepel)
})

# ── Config ───────────────────────────────────────────────────
cfg     <- yaml::read_yaml("config/config.yml")
project <- cfg$project
paths   <- cfg$paths
figures <- cfg$figures

set.seed(project$seed)

# ── Helper functions ─────────────────────────────────────────
source("R/helpers.R")

# ── Output directories ───────────────────────────────────────
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# ── Load and prepare main dataset ────────────────────────────
raw <- read_emdat_xlsx(paths$emdat_xlsx, paths$emdat_sheet) |>
  make_event_date() |>
  collapse_hazard()

thin <- raw |>
  select(
    country, subregion, iso, start_year, start_month, start_day, event_date,
    disaster_type, disaster_subtype, hazard_type, total_deaths,
    no_injured, no_homeless, no_affected, total_affected,
    total_damage_adjusted_000_us
  )

message("Setup complete. 'thin' dataset ready with ", nrow(thin), " rows.")
