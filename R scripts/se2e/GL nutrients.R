
library(tidyverse)
library(patchwork)

domains <- readRDS("./Objects/Domains.rds")                    # Import model domain

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

finalICES <- rbind(shallow_proportion, deep_proportion) %>%                     # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(Latitude, Longitude, Date, Depth_layer) %>%          # Per cast
  summarise(Ammonia = weighted.mean(Ammonia, weights),           # Weighted averages
            DIN =  weighted.mean(DIN, weights),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  mutate(Proportion = Ammonia/DIN,                                          # Get a proportion of ammonia to total DIN
         source = "Greenland (13-16)") %>% 
  select(source, Proportion, Depth_layer, Latitude, Longitude) #%>% 
#
#   st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
#   st_transform(crs = 3035) %>% 
#   st_join(domains) %>%                                                      # Check which are in the model domain
#   st_drop_geometry() %>%                                                    # Simplify the output
#   drop_na()  
# 
# ggplot() +
#   geom_sf(data = domains) +
#   geom_sf(data = finalICES, colour = "red")

#### World Ocean Database ####

WOD <- readLines("./Data/WOD/ocldb1626964011.2671.OSD.csv")

castID <- rep(1, length(WOD)) 
  
for (i in 2:length(WOD)) {
  
  if (WOD[i] == "#--------------------------------------------------------------------------------," ) {
    castID[i] <- castID[i-1] + 1
  } else {
    castID[i] <- castID[i-1]}

}

Casts <- split(WOD, castID)

Checks <- map(Casts, ~{any(str_detect(.x, "Ammonia"))}) %>% 
  unlist()

sum(Checks == T)

Ammonia <- Casts[Checks]

iterate <- map_df(Ammonia, ~{  

  meta <- .x[2:(which(str_detect(.x, "VARIABLES ,"))-1)] %>% 
    rbind() %>% 
    t() %>% 
    as.data.frame() %>% 
    filter(str_detect(., "Latitude|Longitude|Year|Month|Day")) %>% 
    separate(data = ., sep = ",", col = `.`, into = c("Var", NA, "Value")) %>% 
    mutate(Var = str_trim(Var)) %>% 
    pivot_wider(names_from = Var, values_from = Value)
  
  done <- .x[which(str_detect(.x, "VARIABLES ,")):(length(.x)-1)] %>% 
    rbind() %>% 
    t() %>% 
    as.data.frame() %>% 
    separate(data = ., col = `.`, sep = ",",
             into = str_trim(unlist(str_split(.[1,], pattern = ","))) %>% 
               .[. != ""] %>% 
               make.unique()) %>% 
    .[4:nrow(.),] %>% 
    transmute(across(c(Depth, Nitrate, Ammonia), as.numeric)) %>% 
    cbind(meta)
})

# plot <- select(iterate, Latitude, Longitude) %>% 
#   distinct()
# 
# ggplot(plot) +
#   geom_point(aes(x = as.numeric(Longitude), y = as.numeric(Latitude)))
# 
# plot2 <- select(iterate, Year) %>% 
#   distinct()
# 
# ggplot(plot2) +
#   geom_density(aes(x = as.numeric(Year)))


all_data <- iterate %>% 
  drop_na() %>%                                                             # Only use complete cases
  group_by(Latitude, Longitude, Year, Month, Day) %>%                       # Per cast
  arrange(Depth, .by_group = TRUE)                                          # Order depths ascending


shallow_proportion <- all_data %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 0, max_depth = 60), # Calculate share of the depth column per sample
         Depth_layer = "Shallow")                                           # Label depth layer 

deep_proportion <- all_data %>% 
  mutate(weights = calculate_depth_share(Depth, min_depth = 60, max_depth = 400), # Calculate share of the depth column per sample
         Depth_layer = "Deep")                                              # Label depth layer 

final <- rbind(shallow_proportion, deep_proportion) %>%                     # Combine estimates
  filter(weights > 0) %>%                                                   # Drop samples outside the depth window
  group_by(Latitude, Longitude, Year, Month, Day, Depth_layer) %>%          # Per cast
  summarise(Ammonia = weighted.mean(Ammonia, weights),           # Weighted averages
            DIN =  weighted.mean(Nitrate + Ammonia, weights),
            Samples = n()) %>%                                              # Number of samples contributing to each estimate
  ungroup() %>% 
  mutate(Proportion = Ammonia/DIN,                                          # Get a proportion of ammonia to total DIN
         source = "Greenland (84-96)",
         Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) #%>% 
#
# st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
#   st_transform(crs = 3035) %>% 
#   st_join(domains) %>%                                                      # Check which are in the model domain
#   st_drop_geometry() %>%                                                    # Simplify the output
#   drop_na()  
# 
# ggplot() +
#   geom_sf(data = domains) +
#   geom_sf(data = final, colour = "red")


final2 <- select(final2, Proportion, Depth_layer) %>% 
  mutate(source = "Barents Sea (17-19)")
  
plot <- bind_rows(finalICES, final, final2)
  
 a <- ggplot(plot) +
   geom_violin(aes(x = Depth_layer, y = log10(Proportion), colour = source, fill = source == "Barents Sea (17-19)"), 
               position = "identity", key_glyph = draw_key_abline) +
   scale_fill_manual(values = c(NA, "black")) +
   scale_colour_manual(values = c("black", "red", "orange")) +
   geom_point(data = filter(finalICES, Depth_layer == "Deep"), aes(y = log10(Proportion), x = Depth_layer), colour = "red", fill = NA) +
   theme_minimal() +
   guides(fill = FALSE) +
   labs(x = "Depth layer", y = "log10(Ammonia/DIN)", subtitle = "log-scale")

 b <- ggplot(plot) +
   geom_violin(aes(x = Depth_layer, y = Proportion, colour = source, fill = source == "Barents Sea (17-19)"), 
               position = "identity", key_glyph = draw_key_abline) +
   scale_fill_manual(values = c(NA, "black")) +
   scale_colour_manual(values = c("black", "red", "orange")) +
   geom_point(data = filter(finalICES, Depth_layer == "Deep"), aes(y = Proportion, x = Depth_layer), colour = "red", fill = NA) +
   theme_minimal() +
   guides(fill = FALSE) +
   labs(x = "Depth layer", y = "Ammonia/DIN", subtitle = "proportion-scale")
 
 b + a +
   plot_layout(ncol = 2, guides = "collect")

#ggsave("./Figures/saltless/DIN correction check.png", width = 18, height = 10, units = "cm") 
