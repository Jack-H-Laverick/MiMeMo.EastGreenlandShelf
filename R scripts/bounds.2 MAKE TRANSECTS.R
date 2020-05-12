
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("tidyverse", "data.table", "furrr", "sf", "rnaturalearth")    # List packages
lapply(Packages, library, character.only = TRUE)                            # Load packages
source("./R scripts/bounds.z FUNCTIONS.R")  

plan(multiprocess)                                                          # Instruction for parallel processing

domains <- readRDS("./Objects/Domains.rds")                                 # Load SF polygons of the MiMeMo model domains

world <- ne_countries(scale = "medium", returnclass = "sf") %>%             # Get a world map
  st_transform(crs = 3035)                                                  # Assign polar projection

#### Check how much overlap there is between domain boundaries ####

ggplot() +                                                                  
   geom_sf(data = world, size = 0.1, fill = "black", colour = "black") +
   geom_sf(data = domains, fill = NA, colour = "white", size = 0.1) +        # overlap with coast?
   geom_sf(data = domains, fill = NA, size = 0.1) + 
   coord_sf(xlim = c(6100000, 2800000), ylim = c(7500000, 4400000)) +
   theme_minimal()
 ggsave("./Figures/boundaries.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)

# Looks like there is overlap between offshore and inshore.
# Decided to use the Offshore polygon, use the inshore polygon for Inshore to out of bounds inshore flows only 

#### Break up polygon ####

 Edges <- st_cast(domains, "MULTILINESTRING", group_or_split = TRUE) %>%    # Simplify polygon to mutli-linestrings
   st_cast("LINESTRING", group_or_split = TRUE) %>%                         # Split line into it's own row 
   split(., f = list(.$Region, .$Shore)) %>%                                # Separate out by model box
   future_map(boundaries)                                                   # Break the linestrings of a domain into transects
 saveRDS(Edges, "./Objects/Split_boundary.rds")
 
 ggplot() +                                                                 # Show the code worked
   geom_sf(data = Edges[[2]], aes(colour = Segment)) +                  
   theme_minimal() +
   theme(legend.position = "none")
 ggsave("./Figures/Segments.png", plot = last_plot(), scale = 1, width = 16, height = 10, units = "cm", dpi = 500)



