
## Overwrite example boundary data

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

Boundary_template <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Driving/chemistry_CELTIC_SEA_2003-2013.csv"))  # Read in example boundary drivers

#### Last minute data manipulation ####

My_boundary_data<- readRDS("./Objects/Boundary measurements.rds") %>%                        # Import data
  filter(between(Year, 2010, 2019)) %>%                                                      # Limit to reference period
  group_by(Month, Compartment, Variable) %>%                                                 # Average across years
  summarise(Measured = mean(Measured, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(Month) %>%                                                                         # Order months ascending
  mutate(Compartment = factor(Compartment, levels = c("Inshore S", "Offshore S", "Offshore D"),
                              labels = c("Inshore S" = "SI", "Offshore S" = "SO", "Offshore D" = "D"))) %>%
  pivot_wider(names_from = c(Compartment, Variable), names_sep = "_", values_from = Measured) # Spread columns to match template

My_overhang <- readRDS("./Objects/overhang exchanges.rds") %>% 
  filter(between(Year, 2010, 2019), Direction == "Upwelling") %>%                            # Limit to reference period 
  group_by(Month) %>%                                                                        # Average across years
  summarise(DIN = mean(DIN, na.rm = T),
            Detritus = mean(Detritus, na.rm = T)) %>%  
  ungroup() %>% 
  arrange(Month)                                                                             # Order months ascending
  
#### Create new file ####

Boundary_new <- mutate(Boundary_template, 
                       SO_nitrate = My_boundary_data$SO_DIN * 
                           (Boundary_template$SO_nitrate/(Boundary_template$SO_nitrate + Boundary_template$SO_ammona)),# Multiply DIN by the proportion of total DIN as nitrate
                       SO_ammonia = My_boundary_data$SO_DIN * 
                           (Boundary_template$SO_ammona/(Boundary_template$SO_nitrate + Boundary_template$SO_ammona)), # Multiply DIN by the proportion of total DIN as ammonium
                       SO_phyt = My_boundary_data$SO_Phytoplankton,
                       SO_detritus = My_boundary_data$SO_Detritus,
                       D_nitrate = My_boundary_data$D_DIN * 
                         (Boundary_template$D_intrate/(Boundary_template$D_intrate + Boundary_template$D_ammonia)),    # Multiply DIN by the proportion of total DIN as nitrate
                       D_ammonia = My_boundary_data$D_DIN * 
                         (Boundary_template$D_ammonia/(Boundary_template$D_intrate + Boundary_template$D_ammonia)),    # Multiply DIN by the proportion of total DIN as ammonium
                       D_phyt = My_boundary_data$D_Phytoplankton,
                       D_detritus = My_boundary_data$D_Detritus,
                       SI_nitrate = My_boundary_data$SI_DIN * 
                         (Boundary_template$SI_nitrate/(Boundary_template$SI_nitrate + Boundary_template$SI_ammonia)), # Multiply DIN by the proportion of total DIN as nitrate
                       SI_ammonia = My_boundary_data$SI_DIN * 
                         (Boundary_template$SI_ammonia/(Boundary_template$SI_nitrate + Boundary_template$SI_ammonia)), # Multiply DIN by the proportion of total DIN as ammonium
                       SI_phyt = My_boundary_data$SI_Phytoplankton, 
                       SI_detritus = My_boundary_data$SI_Detritus,
                       ## Overhang
                        DO_nitrate = My_overhang$DIN *
                          (Boundary_template$D_intrate/(Boundary_template$D_intrate + Boundary_template$D_ammonia)),    # Multiply DIN by the proportion of total DIN as nitrate
                        DO_ammonia	= My_overhang$DIN *
                          (Boundary_template$D_ammonia/(Boundary_template$D_intrate + Boundary_template$D_ammonia)),
                        DO_detritus = My_overhang$Detritus
                       ) %>% 
  select(-c(SO_ammona, D_intrate))                                          # Fix Mike's typos
                       
write.csv(Boundary_new, file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Driving/chemistry_{toupper(implementation)}_2010-2019.csv"), row.names = F)

