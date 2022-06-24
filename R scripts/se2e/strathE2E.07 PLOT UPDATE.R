
## Overwrite example boundary data

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)
source("./R scripts/@_Region file.R")

#### chemistry #### 

new <- read.csv(str_glue("./StrathE2E/{implementation}/2010-2019/Driving/chemistry_{toupper(implementation)}_2010-2019.csv")) %>%   # Read in example boundary drivers
         select(SO_nitrate, SO_ammonia, SO_phyt, SO_detritus, D_nitrate, D_ammonia, 
                D_phyt, D_detritus, SI_nitrate, SI_ammonia, SI_phyt, SI_detritus,
                DO_detritus, DO_nitrate, DO_ammonia) %>% 
  mutate(Month = 1:12) %>% 
  pivot_longer(!Month, names_to = "Var", values_to = "Value") 

ggplot() +
  geom_line(data = new, aes(x = Month, y = Value)) +
  theme_minimal() +
  facet_wrap(vars(Var), scales = "free_y")

ggsave("./Figures/StrathE2E updated chemistry.png")

#### physics #### 

new <- read.csv(str_glue("./StrathE2E/{implementation}/2010-2019/Driving/physics_{toupper(implementation)}_2010-2019.csv")) %>%   # Read in example boundary drivers
  select(SO_OceanIN, D_OceanIN, SI_OceanIN, SI_OceanOUT, SO_SI_flow, SO_temp, D_temp, SI_temp, log10Kvert,
         DO_log10Kvert, D_SO_upwelling, SO_D_downwelling, DO_D_upwelling, D_DO_downwelling) %>% 
  mutate(Month = 1:12) %>% 
  pivot_longer(!Month, names_to = "Var", values_to = "Value")

ggplot() +
  geom_line(data = new, aes(x = Month, y = Value)) +
  theme_minimal() +
  facet_wrap(vars(Var), scales = "free_y")

ggsave("./Figures/StrathE2E updated physics.png")

