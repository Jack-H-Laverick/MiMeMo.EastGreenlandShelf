
# Global Fishing Watch outputs daily estimates of fishing activity using a neural network based on VMS movement patterns.
# Data is annonymised and aggregated in 100th of a degree cells.

#### Set up ####

rm(list=ls())                                                                 # Wipe the brain

Tidy_packages <- c("tidyverse", "ggfortify", "transformr")                    # List handy data packages
Geo_packages <- c("rgdal", "maps", "raster", "rgeos", "sp")                   # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)        # Load packages
source("./R scripts/gfw.z FUNCTIONS.R")                                       # Bring in the functions I've written

Data <- list.files(path = "./Data/GFW daily_csvs", pattern ="*.csv") %>%     # Very slow! reads in 11gb of raw data 
         sapply(read.csv, header = TRUE, simplify = FALSE) 

#Arctic_fishing <- readRDS("./Objects/Arctic_GFW.rds", header = TRUE)        # Read in Arctic subset produced by code below 

#### Set spatial environment ####

FAO <- readOGR(dsn="FAO_map", layer = "FAO_AREAS")                                        # Import FAO shapefile

Arctic_FIDs <- filter(FAO@data, F_CODE %in% c("27.1.a", "27.1.b", "27.2.a.1", "27.2.a.2", # Find polygons
                                              "27.2.b.1", "27.2.b.2", "27.14.a")) %>% dplyr::select(FID)             # of interest

FAO1 <- FAO[FAO@data$FID == 267,]                                                         # Extract polygon
FAO2 <- FAO[FAO@data$FID == 268,]                                                         # Complex architecture
FAO3 <- FAO[FAO@data$FID == 275,]                                                         # of the FAO shapefile
FAO4 <- FAO[FAO@data$FID == 351,]                                                         # seems to inihibit %in% 
FAO5 <- FAO[FAO@data$FID == 352,]                                                         # operator
FAO6 <- FAO[FAO@data$FID == 353,]
FAO7 <- FAO[FAO@data$FID == 358,]

Division_27.2.b <- gUnion(FAO5, FAO6)                                                     # Dissolve the sub-divisions
Division_27.2.a <- gUnion(FAO4, FAO7)
Sub_area_27.1 <- gUnion(FAO1, FAO3)

FAO_arctic <- bind(Sub_area_27.1, FAO2, Division_27.2.a, Division_27.2.b) 

rm(FAO1,FAO3, FAO4, FAO5, FAO6, FAO7, Arctic_FIDs, FAO)

#### Limit data to Arctic FAO regions ####

Arctic_fishing <- Data %>%
  lapply(Arctic_boats) %>%                                                    # Apply clipping function to each daily csv file
  bind_rows() %>%                                                             # Combine csvs
  separate(Date, into = c("Year", "Month", "Day"), sep = "-")                 # Split dat column
saveRDS(Arctic_fishing, "Arctic_GFW.rds")

#### Create seasonal and regional averages ####

Monthly_ts <- group_by(Arctic_fishing, long, lat, geartype, Year, Month) %>%  # This is useful later too
 summarise(fishing = sum(fishing_hours)) %>%                                  # Count monthly fishing hours in grids squares 
  ungroup()                                                                   # Speeds up operations
   
Seasonal <- group_by(Monthly_ts, long, lat, geartype, Month) %>%
             summarise(fishing = mean(fishing)) %>%                           # Average per month fishing hours in grids squares 
              ungroup()                                                       # If you don't do this filtering takes a million years after!  
saveRDS(Seasonal, file = "./Objects/Seasonal.rds")

test1 <- FAO_boats(Monthly_ts, FAO2)                                          # Clip data points to each FAO region
test2 <- FAO_boats(Monthly_ts, Division_27.2.a) 
test3 <- FAO_boats(Monthly_ts, Division_27.2.b)
test4 <- FAO_boats(Monthly_ts, Sub_area_27.1)

Regional_ts <- bind_rows(test1, test2, test3, test4) %>%                      # Recombine now there's a region column
  group_by(geartype, Month, Region) %>%                                       # Group spatially, by month, and gear
  summarise(Regional_fishing = sum(fishing))                                  # Count fishing hours
saveRDS(Regional_ts, file = "./Objects/Regional_ts.rds")
