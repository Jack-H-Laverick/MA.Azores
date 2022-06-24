
#### Setup                                            ####

library(tidyverse)
source("./R scripts/@_Region file.R")

Fishing <- read.csv(str_glue("./StrathE2E/{implementation}/2010-2019/Param/fishing_distribution_CELTIC_SEA.csv")) %>%   # Read in example boundary drivers
  mutate(Habitat_DO = ifelse(Gear_name == "Longline_mackerel", 0.1, 0),
         Habitat_d3 = ifelse(Gear_name == "Longline_mackerel", Habitat_d3 - 0.1, Habitat_d3))

write.csv(Fishing, row.names = F,
         file = str_glue("./StrathE2E/{implementation}/2010-2019/Param/fishing_distribution_{toupper(implementation)}.csv"))   # Read in example boundary drivers
