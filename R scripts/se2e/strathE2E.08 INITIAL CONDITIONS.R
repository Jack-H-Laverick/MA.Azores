
library(StrathE2E2)
source("./R scripts/@_Region file.R")

model <- e2e_read(implementation,"2010-2019", models.path = "StrathE2E/", results.path = "StrathE2E/Results/")
 
results <- e2e_run(model,nyears = 50)                                # Run the model

e2e_plot_ts(model, results)                                          # Have we reached a steady state?

#### Update starting conditions ####

# e2e_extract_start(model, results, csv.output = TRUE)                # Update starting conditions to the end of a simulation
# 
# file.rename(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Param/initial_values-base.csv"),
#             stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Param/initial_values_{toupper(implementation)}_2010-2019.csv"))
# 
# unlink(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/Param/initial_values_{toupper(implementation)}_2003-2013.csv"))
# 
# ## Update set up file
# 
# Setup_file <- read.csv(stringr::str_glue("./StrathE2E/{implementation}/2010-2019/MODEL_SETUP.csv"))
# 
# Setup_file[4,1] <- stringr::str_glue("initial_values_{implementation}_2010-2019.csv")
# 
# write.csv(Setup_file,
#           file = stringr::str_glue("./StrathE2E/{implementation}/2010-2019/MODEL_SETUP.csv"),
#           row.names = F)
