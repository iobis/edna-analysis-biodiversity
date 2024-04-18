library(jsonlite)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggrepel)
library(biooracler)
library(terra)
library(tidyterra)

samples <- fromJSON("https://raw.githubusercontent.com/iobis/edna-tracker-data/data/generated.json")$samples

sites <- samples %>% 
  filter(!blank) %>%
  group_by(parent_area_name) %>% 
  summarize(
    area_latitude = mean(area_latitude, na.rm = TRUE),
    area_longitude = mean(area_longitude, na.rm = TRUE),
    samples = n()
  ) %>% 
  filter(!is.na(area_latitude) & !is.na(area_longitude)) %>% 
  st_as_sf(coords = c("area_longitude", "area_latitude"), crs = 4326, remove = FALSE) %>% 
  arrange(parent_area_name) %>% 
  mutate(id = 1:n())

world <- ne_countries(scale = "medium", returnclass = "sf")

# temperature layer

temp <- terra::rast("map/thetao_baseline_2000_2019_depthsurf_f740_e681_cf8f_U1713443834620.nc")
plot(temp$thetao_mean)

# plot
# TODO: try to merge alpha and color scales

ggplot() +
  geom_spatraster(data = temp$thetao_mean, aes(alpha = after_stat(value))) +
  scale_fill_viridis_c(name = "Ocean surface temperature (°C)", na.value = "#ffffff", option = "magma") +
  scale_alpha_continuous(guide = "legend") +
  geom_sf(data = world, color = NA, fill = "#ffffff") +
  geom_sf(data = sites, size = 3) +
  geom_label_repel(data = sites, aes(x = area_longitude, y = area_latitude, label = stringr::str_wrap(parent_area_name, 30)), force = 20, force_pull = 0, size = 3) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm"),
    legend.key.height = unit(0.2, "cm")
  ) +
  coord_sf() +
  guides(
    fill = guide_colourbar(name = "temp", title.position = "top", title.hjust = 0.5)
  )

ggsave("map/map.png", width = 14, height = 7, dpi = 300, bg = "white")
