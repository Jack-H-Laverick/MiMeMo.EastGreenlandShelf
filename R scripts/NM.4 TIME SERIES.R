
# Pull a time series from the monthly data files extracted from NEMO - MEDUSA on the idrive
# readRDS("~/Data/MiMeMo/Months")  # Marker so network script can see where the data is coming from

#### Set up ####

setwd("~/Data/MiMeMo/Months")  
rm(list=ls())                                                                 # Wipe the brain

packages <- c("tidyverse", "data.table", "radiant.data", "pbapply", "tictoc") # List packages
lapply(packages, library, character.only = TRUE)                              # Load packages
source("~/R scripts/NM.z FUNCTIONS.R")  

#### Extract time series ####

tic("Creating time series by compartment")                                    # Time the data extraction

TS <- list.files(pattern = ".rds") %>%                                        # Get list of .rds files
  pblapply(summarise_ts) %>%                                                  # Read in the months and calaculate mean and sd by compartments
  rbindlist() %>%                                                             # Combine timesteps into series
  mutate(Planar_current = Zonal_avg + Meridional_avg,                         # Get the resultant current force without direction
         date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"), # Create a single date column for plotting
         Compartment = paste(Shore, Depth, sep = " ")) %>%                    # Build a single compartment column for plotting by
  saveRDS("~/Data/MiMeMo/TS")                                                            # Save out time series in the folder above
toc()                                                                         # Stop timing
