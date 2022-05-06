
# Summarise the data extracted from NEMO-MEDUSA, dealing with deep convection issues
# readRDS("./Objects/vertical boundary/.")  # Marker so network script can see where the data is being pulled from

#### Setup ####

rm(list=ls())                                                                   # Wipe the brain

Packages <- c("tidyverse", "data.table", "furrr", "sf")                               # List packages
lapply(Packages, library, character.only = TRUE)                                # Load packages

plan(multisession)

overhang <- readRDS("./Objects/Overhang.rds")

deep_convection_is <- 0.14                                                      # Threshold above which vertical diffusivity = deep convection

data <- list.files("./Objects/overhang/", full.names = T) %>% # Import data
  future_map(readRDS) %>% 
  rbindlist()

#### Quantify the amount of deep convection ####

## For more discussion see the appropriate entry in ./Notes

total_mixing <- group_by(data, Month) %>%                                                                   
  summarise(Deep_convection_proportion = mean(Vertical_diffusivity > deep_convection_is)) # What proportion of values are deep convection?

ggplot(total_mixing) +
  geom_line(aes(x = Month, y = Deep_convection_proportion)) +
  theme_minimal() +
  ylim(0,1) +
  labs(y = "Proportion of model domain as deep convection")

#### Mean vertical diffusivity ignoring deep convection ####

normal_mixing <- select(data, Vertical_diffusivity, Year, Month) %>%            # Discard excess variables
  filter(Vertical_diffusivity < deep_convection_is) %>%                         # Remove deep convection
  group_by(Year, Month) %>%                                                     # Create a monthly time series
  summarise(Vertical_diffusivity = mean(Vertical_diffusivity, na.rm = T)) %>% 
  ungroup()

saveRDS(normal_mixing, "./Objects/overhang diffusivity.rds")

#### Mean velocities and concentrations ####

sf_use_s2(T)
samples <- filter(data, Year == 1980, Month == 1 ) %>% 
  st_as_sf(coords = c("longitude", "latitude"), remove = F)

area <- st_union(samples) %>%                                               # Combine              
  st_voronoi() %>%                                                          # And create a voronoi tesselation
  st_collection_extract(type = "POLYGON") %>%                               # Expose the polygons
  sf::st_sf() %>%                                                           # Reinstate sf formatting
  st_join(samples) %>%                                                       # Rejoin meta-data from points
  arrange(x, y) %>%                                                         # Order the polygons to match the points
  st_set_crs(4326) %>% 
  st_transform(crs = st_crs(overhang)) %>% 
  st_intersection(st_make_valid(overhang)) %>% 
  mutate(area_m2 = as.numeric(st_area(.))) %>% 
  select(x, y, area_m2)
    
ggplot(area) +                                                              # Check the polygons match correctly with points
  geom_sf(aes(fill = area_m2), size = 0.05, colour = "white") +
  theme_minimal() +
  NULL

exchanges <- select(data, Vertical_velocity, DIN, Detritus, Year, Month, x, y) %>%        # Discard excess variables
  mutate(Direction = ifelse(Vertical_velocity > 0, "Upwelling", "Downwelling")) %>% # Identify upwelling and downwelling
  left_join(st_drop_geometry(area)) %>% 
  mutate(Vertical_velocity = abs(Vertical_velocity)*area_m2) %>%                    # Scale up flow rate to volume of water
  group_by(Year, Month, Direction) %>%                                              # Create a monthly time series
  summarise(DIN = weighted.mean(DIN,  w = Vertical_velocity, na.rm = T),            # Flow-weighted mean concentrations of DIN and detritus
            Detritus = weighted.mean(Detritus, w = Vertical_velocity, na.rm = T),
            Vertical_velocity = sum(Vertical_velocity, na.rm = T)) %>% 
  ungroup()

saveRDS(exchanges, "./Objects/overhang exchanges.rds")
