# ============================================================
# scripts/08_who_regional_ranking.R
# Compare flood + landslide deaths (EM-DAT, 2021) against
# WHO cause-of-death rankings by region and globally.
# Produces:
#   output/tables/who_ranked_all_causes_2021.xlsx
#   output/tables/who_top20_plus_disaster_2021.xlsx
# ============================================================

source("scripts/00_setup.R")

tryCatch({
  
  # ── Load WHO sheet ──────────────────────────────────────────
  sheets <- readxl::excel_sheets(paths$emdat_xlsx)
  
  if (!is.null(paths$who_sheet_name) && paths$who_sheet_name %in% sheets) {
    who_raw <- readxl::read_excel(paths$emdat_xlsx, sheet =
                                    paths$who_sheet_name)
  } else if (!is.null(paths$who_sheet_index) && paths$who_sheet_index <=
             length(sheets)) {
    who_raw <- readxl::read_excel(paths$emdat_xlsx, sheet =
                                    paths$who_sheet_index)
  } else {
    stop(sprintf(
      "WHO sheet not found. Available sheets: %s\nCheck config:
who_sheet_name / who_sheet_index.",
      paste(sheets, collapse = ", ")
    ))
  }
  
  who <- who_raw |>
    janitor::clean_names()
  
  needed <- c("region", "cause", "death")
  if (!all(needed %in% names(who))) {
    stop(sprintf(
      "WHO sheet must contain columns: Region, Cause, Death (any
case).\nFound: %s",
      paste(names(who), collapse = ", ")
    ))
  }
  
  who <- who_raw |>
    janitor::clean_names() |>
    transmute(
      region = region,
      Cause  = cause,
      Death  = as.numeric(death)
    )
  
  # ── Country → WHO region mapping ───────────────────────────
  region_mapping <- list(
    "Africa" = c(
      "Algeria","Angola","Benin","Burundi","Central African
Republic","Chad",
      
      "Congo","Ethiopia","Ghana","Guinea","Kenya","Madagascar","Mozambique",
      "Namibia","Niger","Nigeria","Rwanda","Sao Tome and Principe",
      "South Africa","South Sudan","Sudan","Uganda",
      "United Republic of Tanzania"
    ),
    "Americas" = c(
      "Argentina","Bolivia (Plurinational State
of)","Brazil","Canada","Chile",
      "Colombia","Costa Rica","Ecuador","El Salvador","Guatemala","Guyana",
      "Haiti","Mexico","Panama","Paraguay","Peru",
      "United States of America","Venezuela (Bolivarian Republic of)"
    ),
    "South-East Asia" = c(
      "Bangladesh","Bhutan","Democratic People's Republic of
Korea","India",
      "Indonesia","Maldives","Myanmar","Nepal","Sri Lanka","Thailand"
    ),
    "Europe" = c(
      "Albania","Austria","Belgium","Bosnia and Herzegovina","Bulgaria",
      "Czechia","France","Georgia","Germany","Italy","Luxembourg",
      "Netherlands (Kingdom of the)","Poland","Romania","Russian
Federation",
      
      "Serbia","Slovakia","Slovenia","Spain","Sweden","Switzerland","Ukraine"
    ),
    "Eastern Mediterranean" = c(
      "Afghanistan","Egypt","Iran (Islamic Republic of)","Iraq","Pakistan",
      "Saudi Arabia","Syrian Arab Republic","Tunisia","Turkey","Yemen"
    ),
    "Western Pacific" = c(
      "Australia","Cambodia","China","Japan","Malaysia","Mongolia",
      "New Zealand","Philippines","Republic of Korea","Viet Nam"
    )
  )
  
  assign_region <- function(country) {
    for (r in names(region_mapping)) {
      if (country %in% region_mapping[[r]]) return(r)
    }
    return(NA_character_)
  }
  
  # ── EM-DAT 2021 disaster deaths by WHO region ───────────────
  thin2021 <- thin |>
    filter(start_year == 2021) |>
    mutate(region = vapply(country, assign_region, character(1))) |>
    filter(!is.na(region))
  
  disaster_by_region <- thin2021 |>
    filter(hazard_type %in% c("Flood", "Landslide")) |>
    group_by(region) |>
    summarise(Death = sum(total_deaths, na.rm = TRUE), .groups = "drop") |>
    mutate(Cause = "Flood & Landslide")
  
  disaster_global <- disaster_by_region |>
    summarise(Death = sum(Death), .groups = "drop") |>
    mutate(region = "Global", Cause = "Flood & Landslide")
  
  # ── WHO cause totals by region + global ────────────────────
  who_by_region <- who |>
    filter(region != "Global") |>
    group_by(region, Cause) |>
    summarise(Death = sum(Death, na.rm = TRUE), .groups = "drop")
  
  who_global <- who_by_region |>
    group_by(Cause) |>
    summarise(Death = sum(Death, na.rm = TRUE), .groups = "drop") |>
    mutate(region = "Global")
  
  # ── Combine and rank ────────────────────────────────────────
  combined <- bind_rows(
    who_by_region,
    who_global,
    disaster_by_region,
    disaster_global
  ) |>
    group_by(region, Cause) |>
    summarise(Death = sum(Death, na.rm = TRUE), .groups = "drop") |>
    group_by(region) |>
    arrange(region, desc(Death)) |>
    mutate(Rank = row_number()) |>
    ungroup()
  
  # ── Population by WHO region (for mortality rate) ───────────
  pop_raw <- read_population_csv(paths$population_csv)
  
  pop_countries <- pop_raw |>
    filter(`Series Code` == "SP.POP.TOTL") |>
    transmute(
      country    = `Country Name`,
      Population = as.numeric(`2021 [YR2021]`),
      region     = vapply(country, assign_region, character(1))
    )
  
  pop_by_region <- pop_countries |>
    filter(!is.na(region)) |>
    group_by(region) |>
    summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop")
  
  pop_global_row <- pop_countries |>
    summarise(Population = sum(Population, na.rm = TRUE)) |>
    mutate(region = "Global")
  
  pop_all <- bind_rows(pop_by_region, pop_global_row)
  
  # ── Add mortality rate ──────────────────────────────────────
  ranked_with_pop <- combined |>
    left_join(pop_all, by = "region") |>
    mutate(Mortality_per_100k = round((Death / Population) * 100000, 4))
  
  # ── Export full ranked table ────────────────────────────────
  openxlsx::write.xlsx(
    ranked_with_pop,
    "output/tables/who_ranked_all_causes_2021.xlsx",
    rowNames = FALSE
  )
  
  # ── Export top-20 per region + disaster row ─────────────────
  get_top20_plus_disaster <- function(df, top_n = 20,
                                      disaster_label = "Flood & Landslide")
  {
    regs <- unique(df$region)
    out  <- lapply(regs, function(reg) {
      g   <- df |> filter(region == reg) |> arrange(Rank, desc(Death))
      top <- head(g, top_n)
      if (!(disaster_label %in% top$Cause)) {
        dis <- g |> filter(Cause == disaster_label)
        if (nrow(dis) > 0) top <- bind_rows(top, dis)
      }
      top |> distinct() |> arrange(Rank, desc(Death))
    })
    bind_rows(out)
  }
  
  export_tbl <- get_top20_plus_disaster(ranked_with_pop)
  
  openxlsx::write.xlsx(
    export_tbl,
    "output/tables/who_top20_plus_disaster_2021.xlsx",
    rowNames = FALSE
  )
  
  message("WHO regional ranking complete. Tables saved in output/tables/.")
  
}, error = function(e) {
  message("08_who_regional_ranking.R failed: ", e$message)
  message("Check that your WHO sheet name/index is correct in
config/config.yml")
})
