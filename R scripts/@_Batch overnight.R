
#### List the scripts you want to batch ####

#source("~/R scripts/@_MeMiMo NM EXTRACTION.R")        # Extract and reformat NEMO - MEDUSA data
#source("~/R scripts/@_MeMiMo NM TIME SERIES.R")       # Create time series averages from the NEMO - MEDUSA data
#source("~/R scripts/@_MeMiMo NM SPATIAL.R")           # Create decadal spatial averages from the NEMO - MEDUSA data
#source("~/R scripts/@_MeMiMo NM PLOTTING.R")          # Create all the plots
                    
source("./R scripts/bathymetry.1 DATA WRANGLING.R")
source("./R scripts/bathymetry.2 PLOTTING.R")
source("./R scripts/bathymetry.3 RAYSHADER.R")
source("./R scripts/bathymetry.4 CHOICES.R")
source("./R scripts/bathymetry.5 MODEL DOMAIN.R")

#source("./R scripts/sediment.1 DATA WRANGLING.R")
#source("./R scripts/sediment.2 PLOTTING.R")
# source("./R scripts/sediment.3 GRID.R")
# source("./R scripts/sediment.4 RANDOM FOREST.R")
