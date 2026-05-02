# ============================================================
# run_all.R
# Master script - runs the full analysis pipeline in order.
# Run this from the project root directory:
#   Rscript run_all.R
# ============================================================

scripts <- c(
  "scripts/01_summary_statistics.R",
  "scripts/02_subtype_analysis.R",
  "scripts/03_temporal_intra_annual.R",
  "scripts/04_temporal_long_term.R",
  "scripts/05_spatial_analysis.R",
  "scripts/06_flood_landslide_correlation.R",
  "scripts/07_socioeconomic_analysis.R"
)

for (s in scripts) {
  message("\n", strrep("=", 60))
  message("Running: ", s)
  message(strrep("=", 60))
  source(s, local = new.env())
}

message("\nAll scripts complete. Outputs saved in ./output/")
