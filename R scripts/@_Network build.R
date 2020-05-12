
## Automatically trawl my R scripts to find objects being written and read, 
## and then connect these scripts for a network graph

library(tidyverse)                                                         # Enter the tidyverse

Scripts <- list.files("./R scripts",  pattern = ".R", full.names = T) %>%  # Read in all the Rscripts 
  as.data.frame() %>% 
  filter(!grepl('@|X_|Z_', .)) %>%                                         # Ignore files labelled in these ways
  mutate(Script = as.character(.)) %>%                                    
  #separate(Script, into = c(NA, "Script"), sep = "R scripts") %>%          # Drop long, specific, path lead 
  #mutate(Script = paste0("./R scripts", Script)) %>%                       # Replace with relative path lead
  select(-.)

Links <- function(script) {
  
  #script <- Scripts[16,1]                                                  # Testing
  example <- readLines(script)                                              # Get each line in a script as a character vector
  
  imports <- grepl("readRDS\\(", example)                                   # Which lines use readRDS
  exports <- grepl("saveRDS\\(", example)                                   # Which lines use saveRDS
  functions <- grepl("source\\(", example)                                  # Does the script call a function file?
  
  From <- example[exports] %>%                                              # Which objects come from this script
    data.frame(Object = .) %>%                                              # A column of objects being saved
    mutate(Object = as.character(Object),                                   
           From = script) %>%                                               # Attach the script name
    separate(From, into = c(NA, "From"), sep = "./R scripts/") %>%          # Shrink the script name
    separate(Object, into = c(NA, "Object"), sep = "[.]")                  # Isolate the object name between "~ and )"
  
  To <- example[imports] %>%                                                # Which objects are read into this script
    data.frame(Object = .) %>%                                              
    mutate(Object = as.character(Object),
           To = script) %>% 
    separate(To, into = c(NA, "To"), sep = "./R scripts/") %>%              # Shrink the script name
    separate(Object, into = c(NA, "Object"), sep = "[.]")                  # Isolate the file name between "~ and )"
  
  Functions <- example[functions] %>% 
    data.frame(From = .) %>% 
    mutate(From = as.character(From),
           To = script) %>% 
    separate(From, into = c(NA, "From"), sep = "./R scripts/") %>%          # Shrink the script name
    separate(To, into = c(NA, "To"), sep = "./R scripts/") %>%              # Shrink the script name
    separate(From, into = c("From", NA), sep = " ") %>%                     # Shrink the script name
    mutate(From = paste(From, "FUNCTIONS.R"))
  
  Links <- bind_rows(From, To) %>% 
    bind_rows(Functions)
  
  return(Links)  
}    # Function to return the files read and saved by a script, and any function files called

#### Establish the relations file ####

Scripts2 <- filter(Scripts, !grepl('FUNCTIONS', Scripts$Script))          # Ignore functions files as relationships aren't object mediated (but keep Scripts for defining nodes later)

all <- map(Scripts2[,1], Links) %>%                                       # Check for links in all R scripts
  bind_rows()

From <- select(all, -To) %>%                                              # Grab just the files creating objects
  drop_na() 

To <- select(all, -From) %>%                                              # Grab just the files reading in objects
  drop_na()

#### Format for a network graph ####

nodes <- data.frame(id = seq(1:length(unique(Scripts$Script))),
                    label = as.character(unique(Scripts$Script))) %>% 
  separate(label, into = c(NA, "label"), sep = "./R scripts/")

Edges <- full_join(From, To) %>%                                          # Join Tos and Froms by the shared object 
  drop_na() %>%                        
  bind_rows(filter(all, is.na(Object)))  %>%                              # Add in the relationships to functions files, which aren't mediated by an object
  distinct() %>%                                                          # Remove any repeated links
  left_join(nodes, by = c("From" = "label")) %>%                          # add in numerical code for scripts
  select(-From, from = id) %>%                                            # relabel
  left_join(nodes, by = c("To" = "label")) %>%                            # repeat relabelling for to column
  select(-To, to = id) %>% 
  mutate(arrows = "middle")                                               # Add arrows in the middle of the edge when plotting

nodes <- mutate(nodes, shape = ifelse(grepl("FUNCTIONS", label),"diamond", "dot")) %>% # Add shape to nodes now join is finished                                                 
  separate(label, into = c("group", NA), remove = F)                      # Add a group field for colour

##### Graph it ####

library(visNetwork)

toy <- visNetwork(nodes, Edges, width = "100%", height = "1500") %>%              # Build the network
  visLayout(randomSeed = 10) %>%                                          # Control the randomisation of the layout
  visLegend(width = 0.15) %>%                                             # Add a legend
  visOptions(highlightNearest = list(enabled = TRUE, labelOnly = FALSE),  # Control the highlighting when you select a script
             nodesIdSelection = TRUE) %>%                                 # Allow the user to select a script from a drop down list
  visGroups(groupname = "bathymetry", shape = "dot", color = list(background = "grey", border = "black", # Control colouring and highlighting per group
                                                                  highlight = list(background = "white", border = "black"))) %>%
  visGroups(groupname = "NM", shape = "dot",color = list(background = "orange", border = "darkorange",
                                                         highlight = list(background = "white", border = "darkorange"))) %>% 
  visGroups(groupname = "bounds", shape = "dot", color = list(background = "#97C2FC", border = "#2B7CE9",
                                                              highlight = list(background = "white", border = "#2B7CE9"))) %>% 
  visGroups(groupname = "gfw", shape = "dot",color = list(background = "violet", border = "purple",
                                                          highlight = list(background = "white", border = "purple"))) %>% 
  visGroups(groupname = "ices", shape = "dot",color = list(background = "purple", border = "purple",
                                                          highlight = list(background = "white", border = "purple"))) %>% 
  visGroups(groupname = "sediment", shape = "dot", color = list(background = "yellow", border = "orange",
                                                                highlight = list(background = "white", border = "orange"))) %>% 
  visGroups(groupname = "StrathE2E", shape = "dot", color = list(background = "green", border = "darkgreen",
                                                                highlight = list(background = "white", border = "darkgreen")))
toy
# visSave(toy, file = "@_Network tool.html")                              # Save as HTML file



