# ============================================================
# scripts/05_spatial_analysis.R
# Country-level choropleth maps (log-scaled) and subregional
# bar charts for flood and landslide fatalities (2000-2025).
# ============================================================

source("scripts/00_setup.R")

# ── Country totals ────────────────────────────────────────────
ctry_totals <- thin |>
  group_by(country, iso, hazard_type) |>
  summarise(total_fatalities = sum_known(total_deaths), .groups = "drop")

# ── 5A. CHOROPLETH MAPS ───────────────────────────────────────
world_map <- map_data("world") |>
  mutate(iso3 = countrycode(region, "country.name", "iso3c"))

flood_map_data <- ctry_totals |>
  filter(hazard_type == "Flood") |>
  transmute(iso3 = iso, total_fatalities,
            total_fatalities_log1 = safe_log10(total_fatalities))

landslide_map_data <- ctry_totals |>
  filter(hazard_type == "Landslide") |>
  transmute(iso3 = iso, total_fatalities,
            total_fatalities_log1 = safe_log10(total_fatalities))

flood_map_merged     <- world_map |> left_join(flood_map_data,     by = "iso3")
landslide_map_merged <- world_map |> left_join(landslide_map_data, by = "iso3")

map_break_values <- c(0, 1, 10, 100, 1000, 10000, 100000)
map_breaks_log1  <- safe_log10(map_break_values)

map_theme <- theme_minimal() +
  theme(
    axis.text = element_blank(), axis.ticks = element_blank(),
    panel.grid = element_blank(), legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

p_flood_map <- ggplot(flood_map_merged,
    aes(x = long, y = lat, group = group, fill = total_fatalities_log1)) +
  geom_polygon(color = "black", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "magma", na.value = "grey90", name = "Fatalities",
    breaks = map_breaks_log1, labels = c("0","1","10","100","1K","10K","100K")
  ) +
  labs(title = "Total Flood Fatalities by Country", x = NULL, y = NULL) +
  map_theme +
  guides(fill = guide_colorbar(title.position = "top", label.position = "bottom"))

p_landslide_map <- ggplot(landslide_map_merged,
    aes(x = long, y = lat, group = group, fill = total_fatalities_log1)) +
  geom_polygon(color = "black", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "magma", na.value = "grey90", name = "Fatalities",
    breaks = map_breaks_log1, labels = map_break_values
  ) +
  labs(title = "Total Landslide Fatalities by Country", x = NULL, y = NULL) +
  map_theme +
  guides(fill = guide_colorbar(title.position = "top", label.position = "bottom"))

print(p_flood_map);     print(p_landslide_map)

ggsave("output/figures/map_floods.png",     p_flood_map,
       dpi = figures$dpi, width = figures$width + 3, height = figures$height + 1)
ggsave("output/figures/map_landslides.png", p_landslide_map,
       dpi = figures$dpi, width = figures$width + 3, height = figures$height + 1)

# ── 5B. SUBREGIONAL BAR CHART ─────────────────────────────────
subregion_data <- thin |>
  filter(subregion != "Micronesia") |>
  group_by(subregion, hazard_type) |>
  summarise(total_fatalities = sum_known(total_deaths), .groups = "drop")

subregion_order <- subregion_data |>
  filter(hazard_type == "Flood") |>
  arrange(desc(total_fatalities)) |>
  pull(subregion)

subregion_data <- subregion_data |>
  mutate(subregion = factor(subregion, levels = subregion_order))

subregion_break_values <- c(1, 10, 100, 1000, 10000, 100000)

p_subregion <- ggplot(subregion_data,
    aes(x = subregion, y = total_fatalities, fill = hazard_type)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8,
           color = "black", na.rm = TRUE) +
  scale_fill_manual(values = c("Flood" = "black", "Landslide" = "white")) +
  scale_y_log10(breaks = subregion_break_values,
                labels = c("1","10","100","1K","10K","100K")) +
  labs(title = "Fatalities by Subregion", x = NULL,
       y = "Fatalities (log10 scale)", fill = "Hazard") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(p_subregion)

ggsave("output/figures/subregion_fatalities_log.png", p_subregion,
       dpi = figures$dpi, width = figures$width + 2, height = figures$height)

message("05_spatial_analysis.R complete.")
