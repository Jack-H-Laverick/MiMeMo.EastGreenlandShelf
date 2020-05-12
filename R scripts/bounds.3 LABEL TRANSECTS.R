
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Tidy_packages <- c("tidyverse", "viridis", "data.table", "furrr", "tictoc") # List handy data packages
Geo_packages <- c("stars", "sf", "rnaturalearth")                           # List GIS packages
lapply(c(Tidy_packages, Geo_packages), library, character.only = TRUE)      # Load packages
source("./R scripts/bounds.z FUNCTIONS.R")  

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

domains <- readRDS("./Objects/Domains.rds")                                 # Load SF polygons of the MiMeMo model domains

Edges <- readRDS("./Objects/Split_boundary.rds")                            # Load in segments of domain boundaries

grid <- readRDS("./Objects/Fixed_grid.rds")                                 # Import NEMO-MEDUSA grid with bathymetry
lat <- matrix(grid$Latitude, nrow=235, ncol=190)                            # Reshape to a matrix for stars
lon <- matrix(grid$Longitude, nrow=235, ncol=190)                                  

Bath <- grid$Bathymetry %>%                                                 # Pull the Bathymetry
  matrix(nrow=235, ncol=190) %>%                                            # Convert data to matrix for stars
  st_as_stars() %>%                                                         # Set up stars object
  st_as_stars(curvilinear=list(X1 = lon, X2 = lat)) %>%                     # Pass coordinate matrices and state the grid is curved
  st_as_sf(as_points = FALSE, merge = FALSE) %>%                            # Convert each cell to an SF polygon for fast intersections
  st_transform(crs = 3035)                                                  # Reproject

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 3035)                                                  # Assign polar projection

#### Get weights (length and depth for each boundary segment ####

Depths <- map(Edges, st_intersects, y = Bath) %>%                           # Which cells do transects share space with?
  unlist(recursive = F) %>%                                                 # Remove the top level of the list (by Domain)
  future_map(function(x) mean(Bath[x,]$A1))                                 # Average the values inc ells touched by a transect

Weighted <- bind_rows(Edges, .id = "Compartment") %>%                       # Combine transects
  rename(Length = ".") %>%                                           
  mutate(Depth = as.numeric(Depths)) %>%                                    # Attach the depths caluclated above
  mutate(Weights = Length * abs(Depth)) %>%                                 # Weight the transect by the area of the window (length and depth)
  st_set_crs(3035)                                                          # Need to reinstate the crs 

 ggplot() +                                                                 # Check segments have pulled bathymetry
   geom_sf(data = Bath, aes(fill = A1), colour = NA) +
   geom_sf(data = world, size = 0.1, fill = "black", colour = "black") +
   geom_sf(data = Weighted, aes(colour = Weights)) +
   scale_colour_viridis(option = "viridis", na.value = "red") +
   coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
   theme_minimal()

#### Does a segment need Meridional or Zonal currents? ####

check <- future_map(1:nrow(Weighted), check_grid) %>%                       # Check segments are parlell in Lat/Lon
   rbindlist()                                                              # Bind into a dataframe

## Which currrent do we want depending on orientation?
outcomes <- data.frame(X = c(TRUE, FALSE), Y = c(TRUE, FALSE), current = c("Awkward", "Awkward")) %>%
  bind_rows(data.frame(X = FALSE, Y = TRUE, current = "Meridional")) %>%
  bind_rows(data.frame(X = TRUE, Y = FALSE, current = "Zonal"))             # Which current do we want depending on segment orientation?

Checked <- bind_cols(Weighted, check) %>%                                   # Add in weights for each segment when summing later
  left_join(outcomes) %>%                                                   # Add column of which current to grab
  filter(current != "Awkward",                                              # Drop tricky ones for function
         !grepl("Inshore", Compartment))                                    # Turns out the only inshore transects we need are the "awkward ones"

#### Which direction do currents flow over transects (in or out of box) and categorise by the exchange between boxes ####

Transects <- future_map(1:nrow(Checked), direction) %>%                     # Get a labesl of each transect
  rbindlist() %>%                                                           # Combine into a dataframe
  bind_cols(Checked, .) %>%                                                 # Bind to geometries
  select(-c(X, Y, Length, Depth))                                        # Drop redundant information
saveRDS(Transects, "./Objects/Boundary_transects.rds")

## To visualise "direction function" behaviour, uncomment the plot in the end of the function 
#direction(1)                                                               # Grab current directions relative to domain
##ggsave("./Figures/Current example zoom.png", plot = last_plot(), width = 16, height = 10, units = "cm", dpi = 500)
#direction(2001)
##ggsave("./Figures/Current example zoom2.png", plot = last_plot(), width = 16, height = 10, units = "cm", dpi = 500)
#direction(35469)

ggplot() +                                                                 # Check segments have pulled bathymetry
  geom_sf(data = world, size = 0.1, fill = "black", colour = "black") +
  geom_sf(data = Transects, aes(colour = Neighbour)) +
  scale_colour_viridis(option = "viridis", na.value = "red", discrete = T) +
  coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
  theme_minimal()

Awkward <- bind_cols(Weighted, check) %>%                                   # Add in weights for each segment when summing later
  left_join(outcomes) %>%                                                   # Add column of which current to grab
  filter(current == "Awkward") %>%                                          # Drop tricky ones for function
  filter(Compartment == "Barents Sea.Offshore")




