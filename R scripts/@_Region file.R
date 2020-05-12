
## Set repeated commands specific to the project region
## This version is parameterised for the east Greenland shelf

#e <- ; w <- s <- ; n <-                                                 # Coordinates to clip the global bathymetry file to (as a fraction of a sphere from minimum to maximum Lat Lon)
  
crs <- 3035                                                              # Specify the map projection for the project

zoom <- coord_sf(xlim = c(4100000, 2900000), ylim = c(6750000, 5250000)) # Specify the plotting window for SF maps in this region

ggsave_map <- function(filename, plot) {
  ggsave(filename, plot, scale = 1, width = 11, height = 10, units = "cm", dpi = 500)
  
}                             # Set a new default for saving maps in the correct size

#### bathymetry.5 MODEL DOMAIN ####

Region_mask <- matrix(c(-30, 69.9,
                           -18, 70,
                           0, 75,
                           0, 80,
                           -8, 81.6,
                           -18, 81.3,
                           -30, 69.9),
                         ncol = 2, byrow = T) %>% 
  list() %>% 
  st_polygon() %>% 
  st_sfc() %>% 
  st_sf(Region = "Greenland",.)
st_crs(Region_mask) <- st_crs(4326)                                          
Region_mask <- st_transform(Region_mask, crs = crs)
