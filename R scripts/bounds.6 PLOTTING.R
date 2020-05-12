
#### Set up ####

rm(list=ls())                                                               # Wipe the brain

Packages <- c("tidyverse", "tidygraph", "ggraph")                           # List GIS packages
lapply(Packages, library, character.only = TRUE)                            # Load packages

H_Flows <- readRDS("./Objects/H-Flows.rds") %>%                             # Load flow data
  filter(Year == 1980 & Month == 1)                                         # Use an example year

V_Flows <- readRDS("./Objects/V-Flows.rds")

#### Combine vertical and horizontal flows ####

Exchanges <- mutate(V_Flows, Direction = ifelse(Value > 0, "In", "Out"),    # Introduce missing columns
                    Shore = "Offshore",
                    Neighbour = "Offshore (D)",
                    Depth = "S") %>%
  filter(Flow != "Eddy Diffusivity") %>%                                    # Limit to velocities
  select(-c(Flow, Date)) %>%
  rename(Flow = Value) %>%                      
  filter(Year == 1980 & Month == 1) %>%                                     # Limit to example month
  bind_rows(H_Flows)                                                        # Combine to horizontal flows

#### Plot vertical currents only ####

ggplot(TS, aes(x=Date, y= Value, colour = Region)) +
  geom_line(size = 0.2) +
  geom_smooth(span = 0.008, size = 0.2, se = FALSE) +
  facet_grid(Flow ~., scales = "free_y") +
  theme_minimal() +
  labs(title = "NM time series of vertical currents") +
  NULL

 ggsave("./Figures/flows/TS_Vertical currents.png", plot = last_plot(), width = 16, height = 10, units = "cm", dpi = 500)

#### Create network ####

flows <- mutate(Exchanges, source = ifelse(Direction == "In", paste0(Region, "\n", Neighbour, " (", Depth, ")"), # Create compartment levels, correcty based on direction of the flow
                                           paste0(Region, "\n", Shore, " (", Depth, ")")),
                destination = ifelse(Direction == "In", paste0(Region, "\n", Shore, " (", Depth, ")"),
                                     paste0(Region, "\n", Neighbour, " (", Depth, ")"))) %>%
  mutate(source = ifelse(!grepl("Offshore", source), str_sub(source, end = -5), source),                       # Remove the depth label if not an offshore label
         destination = ifelse(!grepl("Offshore", destination), str_sub(destination, end = -5), destination)) %>%
  select(source, destination, Flow) %>%                                     # Retain relevant information
  rename(weight = Flow) %>%
  mutate(source = gsub("(D) (S)", "(D)", source, fixed = T),                # Remove double depth labels when combing H and V flows
       destination = gsub("(D) (S)", "(D)", destination, fixed = T))

nodes <- tibble(label = unique(flows$source)) %>%                           # Create an index of nodes which need to be connected
  mutate(id = c(6,2,5,8,7,4,3,1)) %>%                                    # Specify the order you want the compartments plotted in
  arrange(id)                                                               # Set the order

edges <- flows %>%                                                          # Create the connections between nodes
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id) %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = id) %>%
  select(from, to, weight) %>%
  mutate(weight = abs(weight))

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)     # Set file format for plotting

#### Plot network ####

cols <- c("Barents Sea\nInshore" = "Yellow", "Barents Sea\nOcean" = "lightblue2", "Barents Sea\nOffshore (S)" = "Yellow3", "Barents Sea\nOffshore (D)" = "Yellow3", 
          "Greenland\nInshore" = "Yellow", "Greenland\nOcean" = "lightblue2", "Greenland\nOffshore (S)" = "Yellow3", "Greenland\nOffshore (D)" = "Yellow3")    

ggraph(routes_tidy, layout = "linear") + 
  geom_edge_arc(aes(width = weight, colour = weight, ), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 4)) +
  #geom_node_label(aes(label = label, fill = label), size = 2) +
  geom_node_label(aes(label = label, fill = label)) +                       # Poster sized labels
  scale_fill_manual(values = cols) +
  geom_curve(aes(x = 4.1, y = .5, xend = 4.9, yend = .5), curvature = -0.5, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  geom_curve(aes(x = 4.9, y = -.5, xend = 4.1, yend = -.5), curvature = - 0.5, arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  labs(title = "Water exchange",
       subtitle = "NEMO-MEDUSA provides water velocities at the geographical boundaries of model compartments ", edge_colour = "Flow") +
  theme_graph() +
  guides(edge_width = FALSE, fill = FALSE) + 
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"),                           # Remove padding around plot
        legend.margin=margin(c(5,0,5,-8))) +                                # Reduce padding by legend
  NULL

ggsave("./Figures/flows/flows.png", plot = last_plot(), width = 17, height = 5, units = "cm", dpi = 500)
ggsave("./Figures/poster_flows.png", plot = last_plot(), width = 40, height = 12, units = "cm", dpi = 500) # Poster
