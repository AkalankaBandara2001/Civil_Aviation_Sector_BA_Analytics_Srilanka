##loading libraries and data

library(igraph)
library(readr)
library(ggraph)
library(tidygraph)
library(ggplot2)

Edges <- read_csv("Data/Question-(b)/SriLanka_Aviation_SNA_Dataset.csv")

head(Edges)
str(Edges)

##creating an un-directed graph

g <- graph_from_data_frame(Edges, directed = FALSE)

cat("Number of nodes (stakeholders):", vcount(g), "\n")
print(V(g)$name)

cat("Number of edges (relationships):", ecount(g), "\n")
cat("Network density:", round(edge_density(g), 3), "\n")
cat("Is the network fully connected?", is_connected(g), "\n")
cat("Average path length:", round(mean_distance(g), 2), "\n")
cat("Network diameter:", diameter(g), "\n")
cat("Average clustering coefficient:", round(transitivity(g, type = "average"), 3), "\n")


# here in the created unfirected graph as we can see that there are 15 nodes and they are 
# Bandaranaike International Airport (CMB),Mattala Rajapaksa International Airport (MRIA)
# ,Civil Aviation Authority of Sri Lanka (CAASL),Airport & Aviation Services SL (AASL),SriLankan Airlines,Mihin Lanka                                   
# Sri Lanka Air Force, Ground Handling Unit,Air Traffic Control (ATC),Fuel Supply Companies, Maintenance & Engineering                     
# Customs & Immigration, International Airlines, Tourism Authority Cargo Operators and they are connectd by 28 edges.
# resulting a fully connected network with density 0.267 which suggest that this is a sparse network. here the average path 
#length(distance between two particular nodes) is 2 and the length of the longest geodesic (the longest shortest path 
#between any pair of vertices) is 4.



##calculating centrality metrics

deg  <- degree(g)
betw <- betweenness(g, normalized = TRUE)
clos  <- closeness(g, normalized = TRUE)   
eigen_cent  <- eigen_centrality(g)$vector          

results <- data.frame(
  Node          = names(deg),
  Degree        = deg,
  Betweenness   = round(betw, 4),
  Closeness     = round(clos, 4),
  Eigenvector   = round(eigen_cent, 4)
)
results <- results[order(-results$Degree, -results$Betweenness), ]
print(results)


# here the degree centrality explains the raw count of direct connections which helps to identify busy hubs.
# betweenness explains how often a node sits on the shortest path between other pairs, simply it helps to
# identify bridges. Closeness helps to explain how quickly a node can reach all others. and finally,eigenvecor
# centrality helps to identify influence by association.

# here the cargo operators have the highest degree of 7 which is the most connected and busiest node and act as a
# bridge as well, and also this node has the highest closeness which explains that from cargo operators node we can
# reach to any other nodes easily. Customs & Immigration is the 2nd most connected node which also act as a between node 
# but not so often as cargo operators. and it also comparatively close to other nodes. then Fuel Supply Companies,
# Bandaranaike International Airport (CMB),Sri Lanka Air Force  has the same degree but only Fuel Supply Companies act as
# a between node compared to other two. and all 3 have around same closeness.


# Air Traffic Control (ATC) is a useful case for interpreting centrality beyond a simple ranking. Its degree centrality
# is only mid-range, tied with CAASL and MRIA, and its betweenness score, while present, is lower than that of the 
# network's two articulation points. Yet the assignment brief explicitly frames ATC as "the central coordinating entity" 
# during the high-pressure scenario, managing aircraft movements and communicating with both airlines and airport authorities. 
# This gap between ATC's numerical centrality and its described operational role illustrates an important distinction: a node's 
# importance to a network is not always proportional to how many connections it holds. ATC's significance lies in which stakeholders 
# it sits between — airlines, the regulator, and ground operations — rather than in the raw number of its ties. This suggests that 
# resilience planning should not treat centrality scores as the sole indicator of operational importance; some coordinating roles 
# carry criticality that a structural metric alone does not fully capture.


write_csv(results, "Generated Data/centrality_results.csv")


##identify the most influential nodes

top_degree <- head(results[order(-results$Degree), "Node"], 3)
top_betw   <- head(results[order(-results$Betweenness), "Node"], 3)

cat("\nTop 3 by Degree Centrality")
print(top_degree)
cat("\nTop 3 by Betweenness Centrality")
print(top_betw)


# as we can see here the most connected nodes are the, Cargo Operators ,Customs & Immigration,
# Fuel Supply Companies, Bandaranaike International Airport (CMB),Sri Lanka Air Force.
# and the top 5 critical between nodes are Cargo Operators,Fuel Supply Companies,Customs & Immigration,
# Bandaranaike International Airport (CMB)and Mattala Rajapaksa International Airport (MRIA).



##identify critical bridge nodes

cut_nodes <- articulation_points(g)
print(cut_nodes)

cut_edges <- bridges(g)
print(cut_edges)


# from the cut nodes and cut edges we can see that Fuel supply companies and Cargo Operators are cut nodes.
# Becuase Ground handling unit is only connect to to Fuel supply companies, if Fuel Supply Companies 
# fail, Ground Handling Unit becomes completely isolated from the rest of the aviation sector.
# And also AASL is only connected to Cargo Operators, so if there is a failure on Cargo Operators AASL will be
# cut off from the whole network.



##identify clusters of interaction

communities <- cluster_louvain(g, weights = E(g)$Weight)
cat("\nDetected communities (clusters):\n")
print(membership(communities))
cat("Modularity score:", round(modularity(communities), 3), "\n")

# as we can see, there are three clusters formed:
# 
# Cluster 1 : Bandaranaike International Airport (CMB),International Airlines,Fuel Supply Companies
#             Ground Handling Unit,Customs & Immigration
# and cluster 1 acts as a Ground Operations & Logistics Hub cluster.
# 
# Cluster 2: Mattala Rajapaksa International Airport (MRIA),SriLankan Airlines,Tourism Authority,Cargo Operators
#            Airport & Aviation Services SL (AASL) and cluster 2 act as a Commercial Aviation & Tourism Aviation cluster.
# 
# Cluster 3: Sri Lanka Air Force,Maintenance & Engineering, Civil Aviation Authority of Sri Lanka (CAASL)
#            Mihin Lanka.
#
# Cluster 4: Maintenance & Engineering 


#a Modularity score of of 0.331 indicates a moderately partitioned network with high cross-cluster interdependence.

##Visualizing



set.seed(42)


net_tidy <- as_tbl_graph(g) %>%
  activate(nodes) %>%
  mutate(
    Degree = centrality_degree(),
    IsCut = name %in% cut_nodes,
    NodeType = ifelse(IsCut, "Articulation Point", "Other Stakeholder")
  )

ggraph(net_tidy, layout = 'kk') +
  geom_edge_link(aes(color = Relationship, width = Weight), alpha = 0.7) +
  scale_edge_width_continuous(range = c(0.8, 2.5), name = "Relationship Weight") +
  scale_edge_color_manual(values = c("Commercial" = "steelblue", "Regulatory" = "darkorange", 
                                     "Operational" = "forestgreen", "Support" = "purple")) +
  
  geom_node_point(aes(size = Degree, fill = NodeType), shape = 21, color = "black", stroke = 0.8) +
  scale_size_continuous(range = c(4, 10), name = "Degree Centrality") +
  scale_fill_manual(values = c("Articulation Point" = "tomato", "Other Stakeholder" = "lightsteelblue"), name = "Node Type") +
  
  geom_node_text(aes(label = name), repel = TRUE, size = 3, fontface = "bold", max.overlaps = 20) +
  
  
  scale_x_continuous(expand = expansion(mult = 0.22)) +
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  
 
  coord_cartesian(clip = "off") +
  
  theme_void() +
  labs(
    title = "Sri Lanka Aviation Stakeholder Network",
    caption = "Red nodes represent structural cut-nodes (articulation points)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
    
    
    legend.position = "right",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    
  
    plot.margin = margin(t = 15, r = 50, b = 15, l = 50)
  )
