
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("./Objects/Months/.")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "furrr", "ncdf4")                             # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multisession)                                                          # Choose the method to parallelise by with furrr

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/nemo/ALLARC", 
                        recursive = TRUE, full.names = TRUE) %>%
  as.data.frame() %>%                                                       # Turn the vector into a dataframe
  separate(".", into = c(rep(NA, 9), "Year", "File"), sep = "/", remove = FALSE) %>% # Extract the year and month from the file name
  mutate(date = paste0(str_extract_all(File, "\\d+")), 
         Month = str_sub(date, start = 5, end = 6)) %>%                     # Pull month
  separate(".", into = c("Path", "File"), sep = 51) %>%                     # Extract the year and month from the file name
  mutate(Type = case_when(str_detect(File, "ptrc_T") ~ "ptrc_T_",       # Query file types
                          str_detect(File, "grid_T") ~ "grid_T_",
                          str_detect(File, "grid_U") ~ "grid_U_",
                          str_detect(File, "grid_V") ~ "grid_V_",
                          str_detect(File, "grid_W") ~ "grid_W_",
                          str_detect(File, "icemod") ~ "icemod_")) %>% 
  filter(!File %in% c("ptrc_T_20000625.nc", "ptrc_T_20470130.nc")) %>%      # Drop corrupted files
  filter(Type != "grid_W_") %>%                                             # Drop the vertical water movement files
  select(Path, File, date, Year, Month, Type)

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  select(-c(Elevation, area))                                               # Drop unneeded data which would get included in new NM files

crop <- readRDS("./Objects/Domains.rds") %>%                                # Load SF polygons of the MiMeMo model domains
  st_buffer(dist = 50000) %>%                                               # It needs to be a bit bigger for sampling flows at the domain boundary
  summarise() %>%                                                           # Combine polygons to avoid double sampling
  mutate(Shore = "Buffer")

Bathymetry <- readRDS("./Objects/NA_grid.rds") %>%                          # Import NEMO-MEDUSA bathymetry
  st_drop_geometry() %>%                                                    # Drop sf geometry column 
  select(-c("x", "y"), latitude = Latitude, longitude = Longitude)          # Clean column so the bathymetry is joined by lat/lon

#### Build summary scheme ####

sf_use_s2(F)

scheme <- scheme_strathE2E(get_spatial(paste0(all_files$Path[1], all_files$File[1]), grid_W = F),
                           Bathymetry, 60, 600, crop) %>% 
  select(x, y, layer, group, weight, slab_layer, longitude, latitude, Bathymetry) %>%   # Get a scheme to summarise for StrathE2E
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F) %>% # Convert to sf object
  st_join(st_transform(domains, crs = 4326)) %>%                            # Attach model zone information
  st_drop_geometry()                                                        # Drop sf formatting

start <- scheme_to_start()                                                  # Get netcdf vectors which define the minimum
count <- scheme_to_count()                                                  # amount of data to import
scheme <- scheme_reframe(scheme) 

ice_scheme <- filter(scheme, layer == 1) %>%                                # Ice data is stored as a matrix, so needs a new scheme
  arrange(group) %>% 
  transmute(n = xyindex_to_nindex(x, y, count[1]))

scheme_result <- arrange(scheme, group) %>%                                 # Create a meta-data object to attach to the summaries
  select(x, y, slab_layer, longitude, latitude, Shore, Bathymetry) %>% 
  distinct() %>% 
  mutate(slab_layer = if_else(slab_layer == 1, "S", "D"),
         weights = case_when(slab_layer == "S" & Bathymetry >= SDepth ~ SDepth,     # Weights for zonal averages by thickness of water column
                             slab_layer == "S" & Bathymetry < SDepth ~ Bathymetry,
                             slab_layer == "D" & Bathymetry >= DDepth ~ (DDepth - SDepth),
                             slab_layer == "D" & Bathymetry < DDepth ~ (Bathymetry - SDepth)))

#### extract ####

tictoc::tic()
all_files %>%
  split(., f = list(.$Month, .$Year)) %>%                                   # Specify the timestep to average files over.
#  .[1:12] %>% 
  future_map(NEMO_MEDUSA, analysis = "slabR", summary = scheme_result,
             scheme = scheme, ice_scheme = ice_scheme$n, start = start,  
             count = count, out_dir = "./Objects/Months", .progress = T)    # Perform the extraction and save an object for each month (in parallel)
tictoc::toc()
