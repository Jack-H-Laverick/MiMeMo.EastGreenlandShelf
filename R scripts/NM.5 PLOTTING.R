
#### Set up ####

setwd("~/Data/MiMeMo")
rm(list=ls(all.names = TRUE))                                               # Wipe the brain

Tidy_packages <- c("tidyverse", "furrr", "tictoc", "viridis", "ggnewscale") # List handy data packages
Geo_packages <- c("mapproj", "rnaturalearth", "sf", "rgdal")                # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("~/R scripts/NM.z FUNCTIONS.R")                  # Bring in the functions I've written

plan(multiprocess)                                                          # Set parallel processing
  
lines <- readRDS("~/Data/Bathymetry GEBCO/Many_contours_proj.rds") %>%      # Read in all the contours
  filter(level %in% c("-30", "-200", "-1000"))                              # Take contours of interest

SP <- readRDS("~/Data/MiMeMo/SPATIAL")                                                    # Read in spatial data
TS <- readRDS("~/Data/MiMeMo/TS")                                                         # Read in time series

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 3035)                                                  # Assign polar projection

#### Plotting ####
    
vars_ts <- c("Ice_pres", "Ice_conc_avg", "Ice_Thickness_avg", "Snow_Thickness_avg", 
          "Vertical_diffusivity_avg", "Vertical_velocity_avg",
          "Salinity_avg", "Temperature_avg", "DIN_avg", "Chlorophyll_avg")  # List of variables to plot   
vars_sp <- str_sub(vars_ts, end = -5) %>% .[-1]                             # Tweak the variable names for spatial plots

sapply(vars_ts, ts_plot)                                                    # Save a time series figure for each variable.

tic("Plotting spatial figures")                                             
 future_map2(rep(SP[1:12], each = length(vars_sp)), 
             rep(vars_sp, times = length(SP)/2), point_plot)                # Plot spatial maps in parallel
toc()
  
tic("Plotting spatial figures")                                             
future_map2(rep(SP[13:24], each = length(vars_sp)),                         # I can't run all in one go because I run out of RAM
            rep(vars_sp, times = length(SP)/2), point_plot)                 # Plot spatial maps in parallel
toc()

tic ("Plotting current figures")
 future_map(SP, stick_plot)                                                 # Plot currents in parallel
toc()
