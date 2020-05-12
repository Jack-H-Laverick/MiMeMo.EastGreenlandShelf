
# Pull the contents of netcdf files: Remember to mount the idrive by typing midrive into the Konsole
# saveRDS("~/Data/MiMeMo/Months")  # Marker so network script can see where the data is being saved too, it's buried in a function

#### Set up ####

setwd("~/Data/MiMeMo")  
rm(list=ls())                                                               # Wipe the brain

packages <- c("tidyverse", "data.table", "sf", "tictoc", "furr", "ncdf4", "pbapply", "radiant.data") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages
source("~/R scripts/NM.z FUNCTIONS.R")                                        # Load custom functions

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

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
         Day = as.integer(Day))  

examples <- group_by(all_files, Type) %>% slice(1)                          # Get one example for each file type
trial <- group_by(all_files, Type) %>% slice(1:3)                           # Get one example for each file type


domains <- readRDS("~/Data/Bathymetry GEBCO/Domains.rds")                   # Load SF polygons of the MiMeMo model domains

#### Set the spatial environment ####

# nc_raw <- nc_open(paste0(examples[4,"Path"], examples[4,"File"]))     
# depths1 <- nc_raw$dim$deptht$vals
# lat1 <- ncvar_get(nc_raw, "nav_lat")
# lon1 <- ncvar_get(nc_raw, "nav_lon")
# nc_close(nc_raw)  
#
## 1, 4, 5, 6 same lon 3 (V) and 2 (U) are unique - same for lat. I'll use the same lat-lons for all the pixels, coordinates for a current are the corners of a pixel instead of the centre
## Ice mod doesnt have depth info, 4 (W) files have their own depth index, all the rest are the same
#
## Not sure how to clip this, could use GEBCO again
#nc_raw <- nc_open("/mnt/idrive/Science/MS/Shared/CAO/nemo/GRID/bathy_meter.nc") # Get full NM bathymetry
#nc_bath <- ncvar_get(nc_raw, "Bathymetry")
#nc_close(nc_raw)  

Space <- get_spatial(paste0(examples[1,"Path"], examples[1,"File"]))        # Pull space from the first file

W <- nc_open(paste0(examples[4,"Path"], examples[4,"File"]))                # Get the different depth vector for W files
DepthsW <- W$dim$depthw$vals ; nc_close(W) ; rm(W) 

# output <- Space$nc_lat %>%                                                # Create a spatial dataframe, grab latitudes
#    as.numeric() %>%                                                       # Make numeric
#    as_tibble() %>%                                                        # Convert to tibble/dataframe
#    rename(Latitude = value) %>%                                           # Rename single column
#    mutate(Longitude = as.numeric(Space$nc_lon))                           # Add in Longitudes
#  saveRDS(object = output, file = "~/Data/MiMeMo/MMM grid.rds")

output <- readRDS("~/Data/MiMeMo/MMM_GEBCO_grid.rds")                       # Load in Bathymetry pulled from GEBCO
mask_bathy <- matrix(abs(output$Bathymetry), 235, 190)                      # Create a bathymetry matrix

Shallow_mark <- between(Space$nc_depth[1:38], 0, 60)                        # Find the positions in the depth vector for shallow slice
Deep_mark <- Space$nc_depth[1:38] > 60                                      # Find the elements in the array to extract for deep slice
Shallow_mark_W <- between(DepthsW[1:39], 0, 60)                             # And for W files
Deep_mark_W <- DepthsW[1:39] > 60                            

sw <- get_weights(0, 60)                                                    # Work out water column proportions for weighted means                                                      
dw <- get_weights(60, 400)
sww <- get_weights.W(0, 60)                                                 # And for W files                                                      
dww <- get_weights.W(60, 400)

start3D <- c(1,1,1,1) ; count3D <- c(-1,-1,38,-1)                           # Spatial cropping at import for variables with depths shallower than 400 m
start3DW <- c(1,1,1,1) ; count3DW <- c(-1,-1,39,-1)                         # Spatial cropping at import for variables with depths shallower than 400 m (W files)

spine <- st_join(output, domains) %>%                                       # Clip a set of lat-lons to the model domains. It will be faster to left join by these than to perform the spatial clip for each of the files
  drop_na(Region) %>%                                                       # Drop points outside of the polygons
  filter(Shore_dist > 0)                                                    # Remove points on land

spine <- bind_rows(mutate(spine, Depth = "S"), mutate(spine, Depth = "D")) %>%  # double up the dataframe with a Depth column
  Compartmentalise()                                                        # Reperform filtering for compartments, as the cliiping polygons are conservative, and calculate weights for averaging

#### Build the monthly sumaries ####

tic("Creating monthly data objects from netcdf files")                      # Time the data extraction

overnight <- all_files %>%  
  split(., f = list(.$Month, .$Year)) %>%                                   # Get a DF of file names for each time step to summarise to
  .[sapply(., function(x) dim(x)[1]) > 0] %>%                               # Drop empty dataframes (Months which weren't observed but split introduces)
  future_map(whole_month)                                                   # Perform the extraction and save an object for each month (in parallel)
toc()                                                                       # Stop timing 

## Check the averaging in the type_in_month function

#### testing ####

# tic("test")
# test <- get_vertical(examples[4,"Path"], examples[4,"File"])
# toc()
# 
# 
# look <- pull(filter(trial, Type == "ptrc_T_"))
# 
# prep <- link(trial)
# 
# 
# nc_raw <- nc_open(paste0(examples[1,"Path"], examples[1,"File"]))     
# nc_saline <- ncvar_get(nc_raw, "vosaline", start3D, count3D)
# nc_close(nc_raw)
# 
# length(as.numeric(stratify(nc_saline, Shallow_mark, sw)))
# length(as.numeric(stratify(nc_saline, Deep_mark, dw)))
