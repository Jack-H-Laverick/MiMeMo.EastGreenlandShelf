
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

packages <- c("MiMeMo.tools", "tidyverse", "data.table", "sf", "tictoc", "furrr", "ncdf4") # List packages
lapply(packages, library, character.only = TRUE)                            # Load packages

source("./R scripts/@_Region file.R")                                       # Define project region 

plan(multiprocess)                                                          # Choose the method to parallelise by with furrr

domain <- readRDS("./Objects/Domains.rds")

#### Grid and clip ####

raw <- nc_open("./Data/Promice freshwater/runoff_land_MAR_2017.nc")         # Open an example file

lat <- ncvar_get(raw, "lat")                                                # Get coordinates
lon <- ncvar_get(raw, "lon")

Runoff <- reshape2::melt(ncvar_get(raw, "runoff")) %>%                      # Reshape matrix to 3 column dataframe
  mutate(Latitude = lat[Var2],                                              # Get coordinates by index
         Longitude = lon[Var2]) %>% 
  rename(Time = Var1) %>%                                                   # Rename the time dimension
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%             # Convert to SF
  st_transform(crs = crs)                                                   # Transform to project projection

nc_close(raw)                                                               # Close netcdf file

#ggplot(filter(Runoff, Time == 1)) + geom_sf(aes(colour = value))
#   geom_sf(data = domain, fill = NA) +
#   NULL

footprint <- st_union(domain) %>% st_sf(Location = "inside",.)              # Combine shore zones to avoid duplicate points

limit <- st_join(filter(Runoff, Time ==1),                                  # Check the location of one set of points
                 st_buffer(footprint, dist = 30000)) %>%                    # are they within 30km of the model domain boundary
  drop_na()                                                                 # Drop points outside

plot(limit)

#### Function ####

Monthly_runoff <- function(file, year) {

# file <-  "./Data/Promice freshwater/runoff_land_MAR_2017.nc"              # Testing
# year <- 2017
  
raw <- nc_open(file)                                                        # Open the file
  
Runoff <- ncvar_get(raw, "runoff") %>%                                      # Import data
  .[,limit$Var2] %>%                                                        # Drop pixels outside model domain
  rowSums() %>%                                                             # Quick sum of all pixels in a time step
  data.frame(Runoff = .,                                                    # Create dataframe
             Month = chron::month.day.year(1:length(.), c(1, 1, year))$month) %>% # Convert julian time step (row number) to month
  group_by(Month) %>% 
  summarise(Runoff = sum(Runoff)) %>%                                       # Total the runoff in a month                                  
  ungroup() %>%                                                             # Ungroup is good practice
  mutate(Year = year)   

nc_close(raw)                                                               # Close netcdf file

return(Runoff)
}

#### Process ####

runoff_files <- list.files("./Data/Promice freshwater/", pattern = ".nc", full.names = T) %>% # List all the netcdf files in the folder 
  data.frame() %>%                                                          # Convert to dataframe
  mutate(file = as.character(`.`),
         year = as.numeric(str_sub(file, -7, -4))) %>%                      # Create a year column
  select(-`.`)

Runoff_TS <- pmap_dfr(runoff_files, Monthly_runoff)                         # Create a dataframe by processing all months
saveRDS(Runoff_TS, "./Objects/Terrestrial runoff.rds")                      # Save

ggplot(Runoff_TS) + 
  geom_line(aes(x = as.POSIXct(paste(Year, Month, "01", sep = "/")), y = Runoff)) +
  theme_minimal() +
  labs( x = "Time", caption = "Monthly freshwater runoff 1979 - 2017 into Greenland model domain",
        y = expression(paste("Terrestrial runoff (", m^3, s^-1, ")")))

ggsave("./Figures/Terrestiral runoff.png", plot = last_plot(), width = 16, height = 10, units = "cm", dpi = 500)

