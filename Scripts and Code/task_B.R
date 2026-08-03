library(igraph)
library(readr)

Edges <- read_csv("Data/Question-(b)/SriLanka_Aviation_SNA_Dataset.csv")

head(Edges)
str(Edges)

#creating an un-directed graph

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
# resulting a fully connected network with density 0.267. here the average path length(distance between two particular nodes)
# is 2 and the length of the longest geodesic (the longest shortest path between any pair of vertices) is 4.



#calculating centrality metrics

degree  <- degree(g)
betweenness <- betweenness(g, normalized = TRUE)
closeness  <- closeness(g, normalized = TRUE)   
eigen_centrality  <- eigen_centrality(g)$vector          

results <- data.frame(
  Node          = names(degree),
  Degree        = degree,
  Betweenness   = round(betweenness, 4),
  Closeness     = round(closeness, 4),
  Eigenvector   = round(eigen_centrality, 4)
)
results <- results[order(-results$Degree, -results$Betweenness), ]
print(results)


write_csv(results, "Generated Data/centrality_results.csv")



top_degree <- head(results[order(-results$Degree), "Node"], 5)
top_betw   <- head(results[order(-results$Betweenness), "Node"], 5)

cat("\nTop 5 by Degree Centrality")
print(top_degree)
cat("\nTop 5 by Betweenness Centrality")
print(top_betw)
