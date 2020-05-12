
# Average the data pulled from NEMO - MEDUSA for creating decadal maps
# readRDS("~/Data/MiMeMo/Months")  # Marker so network script can see where the data is coming from

#### Set up ####

setwd("~/Data/MiMeMo/Months")                                               
rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "data.table", "sf", "tictoc")                    # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("~/R scripts/NM.z FUNCTIONS.R")                                        # Load custom functions

#### Average by decade spatially ####

tic("Creating decadal spatial averages")                                    # Time the operation
  
SP <- list.files(pattern = ".rds") %>%                                      # Get list of .rds files
  purrr::map(decadal) %>%                                                   # Read in data, remove unnecessary columns and create a decade column
  rbindlist() %>%                                                           # Combine dataframes
  mutate(Decade = as.factor(Decade)) %>%                                    # Change decade to factor
  split(., f = list(.$Decade, .$Depth)) %>%                                 # Split into a large dataframe per decade (and depth to help plotting)
  lapply(strip_ice) %>%                                                     # Remove snow and ice variables from deep data before averaging
  purrr::map(summarise_sp) %>%                                              # Average the variables per decade
  purrr::map(reproj) %>%
  saveRDS("~/Data/MiMeMo/SPATIAL")                                          # Save out spatial file in the folder above WD
toc()                                                                       # Stop timing
  
# Check NAs are preserved in maps