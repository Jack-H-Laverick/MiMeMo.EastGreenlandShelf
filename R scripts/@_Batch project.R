
# A full bacth of scripts for MiMeMo 

# Running this will execute the entire project code.

#### Project exploration ####
source("~/R scripts/bathymetry DATA WRANGLING.R") # Extract and reformat GEBCO bathymetry
source("~/R scripts/bathymetry PLOTTING.R")       # View bathymetry in3 ways 
source("~/R scripts/bathymetry CHOICES.R")        # Create choice maps 
source("~/R scripts/bathymetry MODEL DOMAIN.R")   # Define model domain 
source("~/R scripts/MiMeMo Schematic.R")          # Graphic for the model 

#### Internal model drivers ####
source("~/R scripts/NM EXTRACTION.R")             # Extract and reformat NEMO - MEDUSA data
source("~/R scripts/NM TIME SERIES.R")            # Create time series averages
source("~/R scripts/NM SPATIAL.R")                # Create decadal spatial averages
source("~/R scripts/NM PLOTTING.R")               # Create the plots

#### Compartment flows ####
source("~/R scripts/bounds VC-EXTRACTION.R")      # Extract offshore vertical exchanges
source("~/R scripts/bounds MAKE TRANSECTS.R")     # Define box boundaries
source("~/R scripts/bounds DIRECTION EXAMPLE.R")  # Trialling a function
source("~/R scripts/bounds LABEL TRANSECTS.R")    # Identify flows
source("~/R scripts/bounds C-Extraction.R")       # Extract currents from NEMO - MEDUSA
source("~/R scripts/bounds SAMPLE TRANSECTS.R")   # Calculate horizontal flows by box

#### Fishing ####
source("~/R scripts/effort gfw DATA WRANGLING.R") # Rework fishing effort
source("~/R scripts/effort gfw PLOTTING.R")       # Plot fihsing activity

#### Sediment ####
source("~/R scripts/sediment DATA WRANGLING.R")   # Rework sediment data
source("~/R scripts/sediment PLOTTING.R")         # Plot sediment data
