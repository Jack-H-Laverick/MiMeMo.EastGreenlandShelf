
## Keeping track of my scripts

library(tidyverse) ; library(visNetwork)

#### Set up ####

relations <-read.csv("~/R scripts/@_Script network.csv", na.strings = "", check.names=FALSE) %>% 
  pivot_longer(everything(), names_to = "From", values_to = "To", values_ptypes = list(To = character()))

nodes <- data.frame(id = seq(1:length(unique(relations$From))),
                    label = as.character(unique(relations$From)))

edges <-relations %>%
  drop_na() %>%
  left_join(nodes, by = c("From" = "label")) %>% 
  select(-From, from = id) %>% 
  left_join(nodes, by = c("To" = "label")) %>% 
  select(-To, to = id) %>% 
  mutate(arrows = "middle")                                                  # Add arrows in the middle of the edge

nodes <- mutate(nodes, shape = ifelse(grepl("FUNCTIONS", label),"diamond", "dot")) %>% # Add shape to nodes now join is finished                                                 
  separate(label, into = c("group", NA), remove = F)                         # Add a group field for colour

#### Build network widget ####

toy <- visNetwork(nodes, edges, width = "100%", height = "750") %>%          # Build the network
  visLayout(randomSeed = 200) %>%                                            # Control the randomisation of the layout
  visLegend(width = 0.15, ncol = 2) %>%                                      # Add a legend
  visOptions(highlightNearest = list(enabled = TRUE, labelOnly = FALSE),     # Control the highlighting when you select a script
             nodesIdSelection = TRUE) %>%                                    # Allow the user to select a script from a drop down list
  visGroups(groupname = "bathymetry", shape = "dot", color = list(background = "grey", border = "black", # Control colouring and highlighting per group
                                      highlight = list(background = "white", border = "black"))) %>%
  visGroups(groupname = "NM", shape = "dot",color = list(background = "orange", border = "darkorange",
                              highlight = list(background = "white", border = "darkorange"))) %>% 
  visGroups(groupname = "bounds", shape = "dot", color = list(background = "#97C2FC", border = "#2B7CE9",
                                               highlight = list(background = "white", border = "#2B7CE9"))) %>% 
  visGroups(groupname = "gfw", shape = "dot",color = list(background = "violet", border = "purple",
                                  highlight = list(background = "white", border = "purple"))) %>% 
  visGroups(groupname = "sediment", shape = "dot", color = list(background = "yellow", border = "orange",
                                  highlight = list(background = "white", border = "orange")))

# visSave(toy, file = "code network.html")                                    # Save as HTML file

                   