# Flood and Landslide Fatality Analysis (2000–2025)

Master's thesis analysis of global flood and landslide mortality patterns using EM-DAT event-level data.

## Project Structure

```
├── config/
│   └── config.yml                   # Paths, figure settings, random seed
├── data/                            # Raw input data
│   ├── emdat.xlsx                   # EM-DAT data + WHO sheet (see config)
│   ├── world_bank_population.csv
│   └── world_bank_income.xlsx
├── R/
│   └── helpers.R                    # Shared utility functions
├── scripts/
│   ├── 00_setup.R                   # Packages, config, master dataset
│   ├── 01_summary_statistics.R      # High-fatality events & descriptive stats
│   ├── 02_subtype_analysis.R        # Subtype shares & fatality-frequency ratio
│   ├── 03_temporal_intra_annual.R   # Seasonal & monthly patterns
│   ├── 04_temporal_long_term.R      # Yearly trends & Poisson regression
│   ├── 05_spatial_analysis.R        # Choropleth maps & subregional charts
│   ├── 06_flood_landslide_correlation.R   # Cross-country quantile regression
│   ├── 07_socioeconomic_analysis.R        # Income groups & human/economic loss
│   └── 08_who_ranking.R                   # WHO cause-of-death ranking vs disasters
├── output/
│   ├── figures/                     # PNG plots (generated on run)
│   └── tables/                      # XLSX tables (generated on run)
└── run_all.R                        # Master runner — executes all scripts in order
```

## How to Run

1. Clone or download this repository
2. Open R and set your working directory to the project root
3. Run the full pipeline:

```r
source("run_all.R")
```

Or run a single script independently:

```r
source("scripts/08_who_ranking.R")
```

## Data Sources

| Dataset | Source |
|---------|--------|
| EM-DAT disaster events | CRED — Centre for Research on the Epidemiology of Disasters |
| WHO cause-of-death statistics | WHO Global Health Estimates (2021), simplified version in emdat.xlsx |
| Population estimates | World Bank DataBank (`SP.POP.TOTL`, 2021/2025) |
| Income classifications | World Bank Atlas method (2024 GNI per capita) |

## R Environment

- R version 4.5.2
- RStudio 2026.01.2+418

Key packages: `dplyr`, `tidyr`, `ggplot2`, `readxl`, `openxlsx`, `quantreg`, `ggrepel`, `countrycode`, `maps`, `yaml`, `janitor`.

## Reproducibility

All analyses are fully scripted. `run_all.R` executes scripts 01–08 in sequence, each sourcing `scripts/00_setup.R` for a consistent environment. The random seed is set in `config/config.yml`.
