
#### Setup ####

rm(list=ls(all.names = TRUE))                                               # Wipe the brain

library(MiMeMo.tools)

domains <- readRDS("./Objects/Domains.rds")                                 # Import model domain

#### Extract nutrient samples from ICES oceanography data ####

ICES <- read.csv("./Data/WOD/0721188b.csv") %>% 
  filter(Cruise == "5899") %>% 
  select(Date = `yyyy.mm.ddThh.mm`, Latitude = `Latitude..degrees_north.`, Longitude = `Longitude..degrees_east.`,
         Depth = `PRES..db.`, Ammonia = `AMON..umol.l.`, DIN = `NTOT..umol.l.`) %>% 
  drop_na() %>%                                                             # Only use complete cases
  group_by(Latitude, Longitude, Date) %>%                                   # Per cast
  arrange(Depth, .by_group = TRUE)                                          # Order depths ascending

shallow_proportion <- ICES %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 0, max_depth = 60), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

deep_proportion <- ICES %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 60, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

finalICES <- rbind(shallow_proportion, deep_proportion) %>%                 # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(Latitude, Longitude, Date, Depth_layer) %>%                      # Per cast
  summarise(Ammonia = weighted.mean(Ammonia, weights),                      # Weighted averages
            DIN =  weighted.mean(DIN, weights),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  mutate(Proportion = Ammonia/DIN,                                          # Get a proportion of ammonia to total DIN
         source = "Greenland (13-16)") %>% 
  select(source, Proportion, Depth_layer, Latitude, Longitude) 
  
#### Extract from World Ocean Database ####

WOD <- readLines("./Data/WOD/ocldb1626964011.2671.OSD.csv")

castID <- rep(1, length(WOD))                                               # Identify which lines belong to the same cast
  
for (i in 2:length(WOD)) {
  
  if (WOD[i] == "#--------------------------------------------------------------------------------," ) {
    castID[i] <- castID[i-1] + 1
  } else {
    castID[i] <- castID[i-1]}

}

Casts <- split(WOD, castID)                                                 # Split the txt into a list of casts

Checks <- map(Casts, ~{any(str_detect(.x, "Ammonia"))}) %>%                 # Which casts contain ammonia data?
  unlist()

sum(Checks == T)                                                            # How many casts contain ammonia?

Ammonia <- Casts[Checks] %>%                                                # Keep only casts with ammonia
  map_df(~{  

  meta <- .x[2:(which(str_detect(.x, "VARIABLES ,"))-1)] %>%                # Extract the lines containing metadata
    rbind() %>% 
    t() %>% 
    as.data.frame() %>%                                                     # Convert to dataframe
    filter(str_detect(., "Latitude|Longitude|Year|Month|Day")) %>%          # Keep only variables of interest
    separate(data = ., sep = ",", col = `.`, into = c("Var", NA, "Value")) %>% # Split into name and value
    mutate(Var = str_trim(Var)) %>%                                         # Drop white space
    pivot_wider(names_from = Var, values_from = Value)                      # Reshape dataframe
  
  done <- .x[which(str_detect(.x, "VARIABLES ,")):(length(.x)-1)] %>%       # Extract data from cast
    rbind() %>%              
    t() %>% 
    as.data.frame() %>%                                                     # convert to dataframe
    separate(data = ., col = `.`, sep = ",",                                # split into columns
             into = str_trim(unlist(str_split(.[1,], pattern = ","))) %>%   # Getting column names from first row
               .[. != ""] %>%                                               # Dropping empty names
               make.unique()) %>%                                           # And making duplicate names unique
    .[4:nrow(.),] %>% 
    transmute(across(c(Depth, Nitrate, Ammonia), as.numeric)) %>% 
    cbind(meta)                                                             # Combine metadata and cast
}) %>% 
  drop_na() %>%                                                             # Only use complete cases
  group_by(Latitude, Longitude, Year, Month, Day) %>%                       # Per cast
  arrange(Depth, .by_group = TRUE)                                          # Order depths ascending

shallow_proportion <- Ammonia %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 0, max_depth = 60), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

deep_proportion <- Ammonia %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 60, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

#### Combine and summarise data sources ####

final <- rbind(shallow_proportion, deep_proportion) %>%                     # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(Latitude, Longitude, Year, Month, Day, Depth_layer) %>%          # Per cast
  summarise(Ammonia = weighted.mean(Ammonia, weights),                      # Weighted averages
            DIN =  weighted.mean(Nitrate + Ammonia, weights),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  mutate(Proportion = Ammonia/DIN,                                          # Get a proportion of ammonia to total DIN
         source = "Greenland (84-96)",
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  bind_rows(finalICES) %>%                                                  # Combine sources
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_transform(crs = 3035) %>% 
  st_join(domains) %>%                                                      # Check which are in the model domain
  st_drop_geometry() %>%                                                    # Simplify the output
  drop_na() %>%   
  group_by(Depth_layer) %>%                                                 # Decided not to group by shore because there were few inshore samples   
  summarise(Proportion = weighted.mean(Proportion, Samples),                # Calculate average, weighting by the number of samples
            Casts = n())                                                    # Number of CTD casts contributing to each estimate

saveRDS(final, "./Objects/Ammonia to DIN.rds")
