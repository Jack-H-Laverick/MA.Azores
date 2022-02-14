
#### Set up ####

rm(list=ls())                                                               # Wipe the brain
library(MiMeMo.tools) 
library(furrr)
source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multisession)                                                          # Instruction for parallel processing

domains <- readRDS("./Objects/Domains.rds") %>%                             # Load SF polygons of the MiMeMo model domains
  filter(Shore == "Offshore") %>%                                  # Islands only need the offshore polygon because the inshore zone is a ring
  st_cast("POLYGON") %>% 
  rownames_to_column("ID") %>% 
  filter(ID == 8)                                                           # Strip out annoying whispy bits of land which break code

#### Break up polygon ####

Edges <- st_cast(domains, "MULTILINESTRING", group_or_split = TRUE) %>%     # Simplify polygon to mutli-linestrings
  st_cast("LINESTRING", group_or_split = TRUE) %>%                          # Split line into it's own row 
  boundaries(crs = crs) %>%                                                 # Break the linestrings of a domain into transects
  transmute(Length = as.numeric(`.`),                                       # Fix column name
            Shore = "Offshore",                                             # Reintroduce zone name
            Segment)                                                        # Keep segment column

ggplot() +                                                                  # Check we're getting the inshore edges correctly
  geom_sf(data = Edges, colour = "red") +                  
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank()) +
  NULL

saveRDS(Edges, "./Objects/Split_boundary.rds")
