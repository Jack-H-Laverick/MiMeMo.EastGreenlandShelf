
library(tidyverse)

#### Turn off fishing effort ####

Effort <- read.csv("./StrathE2E/Models/Greenland_Sea/2011-2019/Param/fishing_activity_BS_2011-2019.csv") %>% 
  mutate(`Activity_.s.m2.d.` = 0) %>% 
  write.csv(file = "./StrathE2E/Models/Greenland_Sea/2011-2019/Param/fishing_activity_GS_2011-2019.csv", row.names = F)
