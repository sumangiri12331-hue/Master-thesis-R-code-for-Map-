library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggrepel)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create min–max prevalence by country
country_range <- R_data %>%
  group_by(`Respondents are orginally from:`) %>%
  summarise(
    min_prev = min(`Prevalence (Percentages)`, na.rm = TRUE),
    max_prev = max(`Prevalence (Percentages)`, na.rm = TRUE)
  ) %>%
  mutate(
    label = paste0(
      `Respondents are orginally from:`, "\n",
      round(min_prev, 1), "-", round(max_prev, 1), "%"
    )
  )

# Merge with world map
map_data <- world %>%
  left_join(country_range,
            by = c("name" = "Respondents are orginally from:"))

# Get label positions
label_points <- st_point_on_surface(map_data)

# FINAL MAP
p <- ggplot(map_data) +
  
  # Base map
  geom_sf(fill = "grey90", color = "white", linewidth = 0.3) +
  
  # Custom country colours
  geom_sf(data = subset(map_data, name == "Iraq"),
          fill = "#4b0082", color = "white") +   # dark purple
  
  geom_sf(data = subset(map_data, name == "Syria"),
          fill = "#e41a1c", color = "white") +   # red
  
  geom_sf(data = subset(map_data, name == "Lebanon"),
          fill = "#4daf4a", color = "white") +   # green
  
  geom_sf(data = subset(map_data, name == "Jordan"),
          fill = "#87ceeb", color = "white") +   # sky blue
  
  geom_sf(data = subset(map_data, name == "Palestine"),
          fill = "#c9a227", color = "white") +   # dark yellow
  
  geom_sf(data = subset(map_data, name == "Israel"),
          fill = "#fff3b0", color = "white") +   # light yellow
  
  # Labels
  geom_label_repel(
    data = label_points,
    aes(x = st_coordinates(label_points)[,1],
        y = st_coordinates(label_points)[,2],
        label = label),
    size = 3,
    fontface = "bold",
    fill = "white",
    color = "black",
    label.size = 0.2,
    segment.color = "grey50",
    max.overlaps = Inf
  ) +
  
  coord_sf(xlim = c(34, 48), ylim = c(27, 38), expand = FALSE) +
  
  labs(
    title = "Tobacco Use Prevalence Range by Country"
  ) +
  
  theme_minimal(base_size = 14)

# Print map
p

# SAVE HIGH QUALITY FILE (NO COMPRESSION ERROR ON MAC)
ggsave(
  filename = "MiddleEast_Prevalence_Map_FINAL.tiff",
  plot = p,
  width = 12,
  height = 9,
  dpi = 600
)