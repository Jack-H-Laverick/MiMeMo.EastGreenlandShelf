
## Overwrite example boundary data

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(MiMeMo.tools)

#### chemistry #### 

new <- read.csv("./StrathE2E/Models/Greenland_Sea/2011-2019/Driving/chemistry_GS_2011-2019.csv") %>%   # Read in example boundary drivers
  mutate(Month = 1:12) %>% 
  pivot_longer(!Month, names_to = "Var", values_to = "Value") %>% 
  mutate(Model = "GL")

comparison <- read.csv("./StrathE2E/Models/Barents_Sea/2011-2019/Driving/chemistry_BS_2011-2019.csv") %>% 
  mutate(Month = 1:12) %>% 
  pivot_longer(!Month, names_to = "Var", values_to = "Value") %>% 
  mutate(Model = "BS") %>% 
  bind_rows(new)

ggplot() +
  geom_line(data = comparison, aes(x = Month, y = Value, colour = Model)) +
  theme_minimal() +
  facet_wrap(vars(Var), scales = "free_y")

ggsave("./Figures/StrathE2E updated chemistry.png")

#### physics #### 

new <- read.csv("./StrathE2E/Models/Greenland_Sea/2011-2019/Driving/physics_GS_2011-2019.csv") %>%   # Read in example boundary drivers
  mutate(Month = 1:12) %>% 
  pivot_longer(!Month, names_to = "Var", values_to = "Value") %>% 
  mutate(Model = "GL")

comparison <- read.csv("./StrathE2E/Models/Barents_Sea/2011-2019/Driving/physics_BS_2011-2019.csv") %>% 
  mutate(Month = 1:12) %>% 
  pivot_longer(!Month, names_to = "Var", values_to = "Value") %>% 
  mutate(Model = "BS") %>% 
  bind_rows(new)

ggplot() +
  geom_line(data = comparison, aes(x = Month, y = Value, colour = Model)) +
  theme_minimal() +
  facet_wrap(vars(Var), scales = "free_y")

ggsave("./Figures/StrathE2E updated physics.png")

