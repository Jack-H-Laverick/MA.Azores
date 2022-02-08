
# Create a spatial grid to bind extracted NM model outputs to, including distance from shore and bathymetry
# Remember to mount the idrive by typing midrive into the Konsole

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "ncdf4", "sf")                                # List handy data packages
lapply(packages, library, character.only = TRUE)                            # Load packages

Space <- list.files("/mnt/idrive/Science/MS/Shared/CAO/nemo/ALLARC/1980/", recursive = TRUE, full.names = TRUE) %>% 
  .[1] %>%                                                                  # Name an example NM file
  nemomedusR::get_spatial()                                                 # And pull the spatial variables

#### Get NEMO-MEDUSA bathymetry data ####

NM_bath <- "/mnt/idrive/Science/MS/Shared/CAO/nemo/GRID/bathy_meter.nc"

raw <- nc_open(NM_bath)
bath_lat <- ncvar_get(raw, varid = "nav_lat")
bath_lon <- ncvar_get(raw, varid = "nav_lon")
bath_bath <- ncvar_get(raw, varid = "Bathymetry")
nc_close(raw)

#### Crop to the North Atlantic files extent ####

## The two matrices have the same widths, so just subset the columns. Both matrices have the same top latitude, so it's
## just a question of how far south to go.

bath <- bath_bath[, (ncol(bath_bath)-ncol(Space$nc_lat)+1): ncol(bath_bath)]

lat <- bath_lat[, (ncol(bath_bath)-ncol(Space$nc_lat)+1): ncol(bath_bath)]

lon <- bath_lon[, (ncol(bath_bath)-ncol(Space$nc_lat)+1): ncol(bath_bath)]

#### Further region specific crop to speed up extraction ####

grid <- setNames(reshape2::melt(lat), c("x", "y", "Latitude")) %>% 
  left_join(setNames(reshape2::melt(lon), c("x", "y", "Longitude"))) %>% 
  left_join(setNames(reshape2::melt(bath), c("x", "y", "Bathymetry"))) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) #%>% # Set dataframe to SF format
#  st_transform(crs) 

ggplot(grid) +
  geom_raster(aes(x=x, y=y, fill = Bathymetry))

saveRDS(grid, file = "./Objects/NA_grid.rds")      # Save
