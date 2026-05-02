# ============================================================
# scripts/02_subtype_analysis.R
# Subtype-level fatality and injury breakdown, plus
# fatality-frequency ratio by subtype.
# ============================================================

source("scripts/00_setup.R")

landslide_subtypes <- c(
  "Landslide (dry)", "Landslide (wet)", "Mudslide", "Rockfall (dry)",
  "Rockfall (wet)", "Sudden Subsidence (wet)", "Avalanche (dry)", "Avalanche (wet)"
)

flood_subtypes <- c(
  "Coastal flood", "Flash flood", "Flood (General)", "Riverine flood"
)

# ── Fatality / injury shares by subtype ──────────────────────
subtype_summary <- thin |>
  filter(disaster_subtype %in% c(landslide_subtypes, flood_subtypes)) |>
  group_by(disaster_subtype) |>
  summarise(
    Fatalities = sum_known(total_deaths),
    Injured    = sum(no_injured, na.rm = TRUE),
    Num_Events = n(),
    .groups = "drop"
  ) |>
  mutate(
    Category = case_when(
      disaster_subtype %in% flood_subtypes     ~ "Flood",
      disaster_subtype %in% landslide_subtypes ~ "Landslide"
    )
  ) |>
  group_by(Category) |>
  mutate(
    Fatality_Share = (Fatalities / sum(Fatalities, na.rm = TRUE)) * 100,
    Event_Share    = (Num_Events  / sum(Num_Events,  na.rm = TRUE)) * 100,
    Fatality_Frequency_Ratio = Fatality_Share / Event_Share
  ) |>
  ungroup()

# Long format for bar chart
subtype_long <- subtype_summary |>
  select(disaster_subtype, Category, Fatalities, Injured, Fatality_Share) |>
  pivot_longer(
    cols      = c(Fatalities, Injured),
    names_to  = "Impact_Type",
    values_to = "Count"
  ) |>
  group_by(Category, Impact_Type) |>
  mutate(Percentage = (Count / sum(Count, na.rm = TRUE)) * 100) |>
  ungroup()

maxp_subtype <- max(subtype_long$Percentage, na.rm = TRUE)

# ── Plot: % fatalities & injuries by subtype ─────────────────
p_subtypes <- ggplot(
  subtype_long,
  aes(x = Percentage, y = reorder(disaster_subtype, Percentage), fill = Impact_Type)
) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = ifelse(is.na(Percentage), "NA", sprintf("%.1f%%", Percentage))),
    position = position_dodge(width = 0.9),
    hjust = -0.2, size = 3, na.rm = TRUE
  ) +
  scale_fill_manual(values = c("black", "darkgray")) +
  scale_x_continuous(limits = c(0, maxp_subtype * 1.2)) +
  facet_wrap(~Category, scales = "free_y") +
  labs(
    title = "Percentage of Fatalities and Injured by Disaster Subtype",
    x = "Percentage (%)", y = "Subtype", fill = "Impact Type"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

print(p_subtypes)

ggsave(
  "output/figures/subtypes_percentages.png", p_subtypes,
  dpi = figures$dpi, width = figures$width, height = figures$height
)

# ── Plot: fatality frequency ratio by subtype ─────────────────
p_ratio <- ggplot(
  subtype_summary,
  aes(x = Fatality_Frequency_Ratio, y = reorder(disaster_subtype, Fatality_Frequency_Ratio),
      fill = Category)
) +
  geom_col() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("Flood" = "black", "Landslide" = "grey60")) +
  facet_wrap(~Category, scales = "free_y") +
  labs(
    title = "Fatality Frequency Ratio by Disaster Subtype",
    x = "Fatality Frequency Ratio", y = "Subtype", fill = "Category"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_ratio)

ggsave(
  "output/figures/subtype_fatality_frequency_ratio.png", p_ratio,
  dpi = figures$dpi, width = figures$width, height = figures$height
)

openxlsx::write.xlsx(
  subtype_summary, "output/tables/subtype_summary.xlsx", rowNames = FALSE
)

message("02_subtype_analysis.R complete.")
