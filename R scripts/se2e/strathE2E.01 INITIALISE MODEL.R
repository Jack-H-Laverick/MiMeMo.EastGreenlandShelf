
## Initialise model

#### Setup ####

rm(list=ls())                                                               # Wipe the brain

library(StrathE2EPolar)

e2ep_copy(model.name = "Barents_Sea", model.variant =  "2011-2019" , dest.path = "./StrathE2E") # Copy example model

file.rename("./StrathE2E/Models/Barents_Sea", "./StrathE2E/Models/Greenland_Sea")               # Rename model

e2ep_copy(model.name = "Barents_Sea", model.variant =  "2011-2019" , dest.path = "./StrathE2E") # Keep example model
