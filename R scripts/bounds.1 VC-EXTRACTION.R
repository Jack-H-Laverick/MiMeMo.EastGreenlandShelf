
# Interpolate to 60 m interface and extract current information as a timeseries by region
# Remember to mount the idrive first  

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("tidyverse", "sf", "rcdo", "ncdf4", "data.table", "tictoc", "furrr" ) # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/bounds.z FUNCTIONS.R")  

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

domains <- readRDS("./Objects/Domains.rds")                                 # Load SF polygons of the MiMeMo model domains

points <- readRDS("./Objects/Fixed_grid.rds") %>%                           # Load in Bathymetry pulled from GEBCO
  st_join(domains) %>%                                                      # Clip a set of lat-lons to the model domains.
  filter(Shore == "Offshore") %>%                                           # Limit to offshore zone                  
  filter(between(Bathymetry, -400, -60) & Shore_dist > 20000) %>%           # Reinstate conditions for offshore zone, as cliiping polygon is slightly larger
  dplyr::select(-c(Shore_dist, Bathymetry, Shore))                          # Drop unnecessary information

#### Get the names of vertical current files ####

all_files <- list.files("/mnt/idrive/Science/MS/Shared/CAO/mimemo/clipped_medusa", recursive = TRUE, full.names = TRUE) %>%
  as_tibble() %>%                                                           # Turn the vector into a dataframe/tibble
  separate(value, into = c("Path", "File"), sep = 61) %>%                   # Extract the year and month from the file name
  separate(File, into = c("Type", "Date"), 
           remove = FALSE, sep = -11) %>%                                   # Extract the year and month from the file name
  mutate(Date = str_sub(Date, end = -4))                                    # Drop file extension to get number

all_files[which(all_files$Date == "_2047105"),]$Type <- "grid_T_"           # One file has been incorrectly labelled, I've corrected the name. Now all timesteps have 6 files
all_files[which(all_files$Date == "_2047105"),]$Date <- "20471005"          

all_files  <- all_files %>%
  separate(Date, into = c("Year", "Month", "Day"),sep = c(4, 6)) %>%        # Extract the year and month from the file name 
  mutate(Year = as.integer(Year),                                           # Set time as integers 
         Month = as.integer(Month), 
         Day = as.integer(Day)) %>%
  filter(Type == "grid_W_") %>%                                             # Limit to files containing vertical current data
  split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
  .[sapply(., function(x) dim(x)[1]) > 0]                                   # Drop empty dataframes (Months which weren't observed but split introduces)

#### Extract currents at the vertical boundary for offshore boxes ####

tic() 
TS <- all_files %>%                                                        # All the monthly packets of files                          
    future_map(avg_month) %>%                                              # Interpolate, extract, and average by month as a time step for all files (in parallel)
    rbindlist() %>%
    mutate(Date = as.Date(paste("15", Month, Year, sep = "/"), format = "%d/%m/%Y"),
           Flow = ifelse(is.na(Flow), "Velocity", Flow),                   # Replace Nas with velocity if not splitting upwelling and downwelling 
           Value = ifelse(Region == "Greenland", Value * st_area(domains)[1], # Scale by the area of the model compartment
                          Value * st_area(domains)[3]))
toc()
  
saveRDS(TS, "./Objects/V-Flows.rds")


  