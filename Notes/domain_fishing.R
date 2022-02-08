
#### Chunk 1 ####

Packages <- c("tidyverse", "data.table", "furrr", "raster", "stars") # List packages
lapply(Packages, library, character.only = TRUE)                     # Load packages

plan(multisession)                                                   # Need to reduce the workers because markdown 

####

Fishing <- list.files("../Shared data/GFW", recursive = TRUE, pattern = ".csv", full.names = TRUE) %>% 
  future_map(~{
    day <- fread(.x)[
      fishing_hours != 0 &                                                 # For the cells with fishing activity
      cell_ll_lat %between% c(32, 44) & cell_ll_lon %between% c(-38, -18), # and in our spatial window 
      .(fishing_hours = sum(fishing_hours)),                               # sum the fishing effort
      keyby = .(date, cell_ll_lon, cell_ll_lat) ]},                        # by date and cell.
    .progress = T) %>% 
  rbindlist()

total <- Fishing[, .(fishing_hours = sum(fishing_hours)),                     # sum the fishing effort
    keyby = .(cell_ll_lon, cell_ll_lat)]                                  # across all timesteps


raster <- rasterFromXYZ(total) %>% st_as_stars()

st_crs(raster) <- 4326

saveRDS(raster, "./Notes/Cache/domain_GFW.rds")
