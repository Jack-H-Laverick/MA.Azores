
# Create an object defining the geographic extent of the model domain

#### Set up ####

rm(list=ls())                                                   

Packages <- c("tidyverse", "sf", "stars", "rnaturalearth", "raster")                  # List handy packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

source("./R scripts/@_Region file.R")                                       # Define project region 

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = crs)                                                   # Assign polar projection

EEZ <- read_sf("./Data/eez")

land <- sfheaders::sf_remove_holes(EEZ) %>% 
  st_difference(EEZ)

shelf <- read_sf("./Data/shelf/") %>%
  st_difference(st_transform(land, st_crs(.))) %>% 
  filter(!str_detect(et_source, "Banco")) %>% 
  st_union() %>% 
  st_sf() %>% 
  mutate(Elevation = exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), ., "mean"))

GEBCO <- read_stars("../Shared data/GEBCO_2020.nc")
st_crs(GEBCO) <- st_crs(EEZ)
GFW <- raster("../Shared data/distance-from-shore.tif")

crop <- as(extent(-35, -23, 35, 41), "SpatialPolygons")
crs(crop) <- crs(GEBCO)

#GEBCO <- crop(GEBCO, crop)
GFW <- crop(GFW, crop)

# #### Polygons based on depth ####

Depths <- GEBCO[EEZ] %>% 
  st_as_stars()

Depths[[1]][Depths[[1]] > units::set_units(0, "m") | Depths[[1]] < units::set_units(-700, "m")] <- NA

Depths[[1]][Depths[[1]] > units::set_units(-100, "m")] <- units::set_units(-100, "m")
Depths[[1]][Depths[[1]] < units::set_units(-100, "m")] <- units::set_units(-700, "m")

Bottom <- st_as_stars(Depths) %>%
  st_as_sf(merge = TRUE) %>%
  st_make_valid() %>%
  group_by(GEBCO_2020.nc) %>%
  summarise(Depth = abs(mean(GEBCO_2020.nc))) %>%
  st_make_valid()

ggplot(Bottom) +
  geom_sf(aes(fill = as.character(Depth)), colour = NA) +
  theme_minimal()

ggplot(filter(Bottom, Depth == 100)) +
  geom_sf(data = shelf, colour = NA, fill = "red") +
  geom_sf(fill = "blue", colour = NA) +
  theme_minimal()

#### Polygons based on distance ####

Distance <- GFW
Distance[GFW == 0 | GFW > 2.9] <- NA  # Distance appears to be in KM not m as stated on the website.

Distance[is.finite(Distance)] <- 2.9  # Distance appears to be in KM not m as stated on the website.

Distance <- st_as_stars(Distance) %>% 
  st_as_sf(merge = TRUE) %>% 
  st_make_valid() %>% 
  group_by(distance.from.shore) %>% 
  summarise(Distance = (mean(distance.from.shore))) %>% 
  st_make_valid()

ggplot() +
  geom_sf(data = Distance, fill = "blue", colour = NA) + 
  geom_sf(data = shelf, fill = "red", colour = NA) +
  theme_minimal() 

#### Expand inshore and cut offshore ####

# sf_use_s2(T)
 
 inshore <- st_union(shelf, st_make_valid(st_transform(Distance, st_crs(shelf)))) 
 
exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), Distance, "mean")

ggplot() +
  geom_sf(data = EEZ, fill = "white", size = 0.1) +
  geom_sf(data = Bottom, fill = "lightblue", size =0.1) +
  geom_sf(data = inshore, fill = "red", size = 0.1) +
  theme_minimal()

ggsave("./Figures/bathymetry/EEZ.png", width = 18, height = 10, units = "cm", dpi = 700)

Offshore <- sfheaders::sf_remove_holes(st_transform(EEZ, st_crs(inshore))) %>% 
  st_difference(sfheaders::sf_remove_holes(inshore)) %>% 
  dplyr::select(1)

#### Format to domains object ####

Domains <- bind_rows(st_make_valid(Offshore), st_make_valid(inshore)) %>% 
  transmute(Shore = c("Offshore", "Inshore"),
            area = as.numeric(st_area(.)),
            Elevation = exactextractr::exact_extract(raster("../Shared data/GEBCO_2020.nc"), ., "mean")) %>% 
  st_transform(crs = crs) 

saveRDS(Domains, "./Objects/Domains.rds")

map <- ggplot() + 
  geom_sf(data = Domains, aes(fill = Shore), colour = NA) +
  scale_fill_manual(values = c("#FFEB3B", "#FF7043")) +
  coord_sf(xlim = st_bbox(st_transform(EEZ,crs))[c(1,3)], ylim = st_bbox(st_transform(EEZ,crs))[c(2,4)]) +
  theme_minimal() +
  labs(caption = "Final model area") +
  NULL
ggsave_map("./Figures/bathymetry/Domains.png", map)

map_distance <- ggplot() +
  geom_stars(data = st_as_stars(GFW) %>% st_transform(crs)) +
  geom_sf(data = world, size = 0.1, fill = "white") +
  coord_sf(xlim = st_bbox(st_transform(EEZ,crs))[c(1,3)], ylim = st_bbox(st_transform(EEZ,crs))[c(2,4)]) +
  theme_minimal() +
  NULL
ggsave_map("./Figures/bathymetry/Distance.png", map_distance)

