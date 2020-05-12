
# readRDS("./Objects/Currents/")    # Marker so network script can see where the data is coming from

#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("tidyverse", "viridis", "data.table", "furrr", "tictoc") # List handy data packages
Geo_packages <- c("stars", "sf", "rnaturalearth")                           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/bounds.z FUNCTIONS.R")                                  # Pull custom functions

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

Transects <- readRDS("./Objects/Boundary_transects.rds") %>%                # Import transects to sample at
   filter(Shore == "Offshore") %>%                                          # Turns out we only need the boundaries of the offshore polygon (plus a few for inshore to the open ocean)
   split(list(.$current))                                                   # Split by the current which needs to be sampled

grid <- readRDS("./Objects/Fixed_grid.rds")                                 # Joining data to the grid reorders the datapoints
lat <- matrix(grid$Latitude, nrow=235, ncol=190)                            # Reshape to a matrix for stars
lon <- matrix(grid$Longitude, nrow=235, ncol=190)                                  

#### Extract water exchanges between horizontal compartments ####

tic()
Flows <- list.files("./Objects/Currents/", full.names = T) %>%              # Get the names of all data files 
   future_map(Sample, .progress = T) %>%                                    # Sample the currents in each file and aggregate
   rbindlist() %>%                                                          # Bind into a dataframe
   saveRDS("./Objects/H-Flows.rds")                                         # Save
toc()