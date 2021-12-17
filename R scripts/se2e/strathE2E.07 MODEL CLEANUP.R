
# Remove the files which have been replaced by ones for the new region
unlink("./StrathE2E/Models/Greenland_Sea/2011-2019/Driving/chemistry_BS_2011-2019.csv")
unlink("./StrathE2E/Models/Greenland_Sea/2011-2019/Param/physical_parameters_BS.csv")
unlink("./StrathE2E/Models/Greenland_Sea/2011-2019/Driving/physics_BS_2011-2019.csv") # Delete old file
unlink("./StrathE2E/Models/Greenland_Sea/2011-2019/Param/fishing_activity_BS_2011-2019.csv")

# Point StrathE2E to new files

library(tidyverse)

setup <- read.csv("./StrathE2E/Models/Greenland_Sea/2011-2019/MODEL_SETUP.csv") %>% 
  mutate(Filename = case_when(Filename == "physical_parameters_BS.csv" ~ "physical_parameters_GS.csv",
                              Filename == "chemistry_BS_2011-2019.csv" ~ "chemistry_GS_2011-2019.csv",
                              Filename == "physics_BS_2011-2019.csv" ~ "physics_GS_2011-2019.csv",
                              Filename == "fishing_activity_BS_2011-2019.csv" ~ "fishing_activity_GS_2011-2019.csv",
                              T ~ Filename)) %>% 
  write.csv("./StrathE2E/Models/Greenland_Sea/2011-2019/MODEL_SETUP.csv", row.names = F)
