rm(list=ls())  

## Load package
library(igraph)

dir_path <- "D:/MSBA/Fall2016/IDS564/labs/regular lab 2/"
setwd(dir_path)

#read csv and make it a graph
infile<-"SAPFull_SubGraph_EdgeList.csv"
el=read.csv(infile, header = TRUE, sep = ",")
g_acq=graph.data.frame(el, directed = TRUE, vertices= NULL)

# Edges
ecount(g_acq)
## Vertices
vcount(g_acq)

#Check for strong and weak connections
is_connected(g_acq,mode="strong")
is_connected(g_acq,mode="weak")


## Check whether Self_loops exist, as do multiple edges
is.simple(g_acq)

#Adjusting the weights 
E(g_acq)$weight 
g_acq_simpl<-simplify(g_acq)
E(g_acq_simpl)$weight 
# Will use the inverse of log weight for shortest path calculations
inv_weight<-1/log(E(g_acq_simpl)$weight  + 1)
num_weight<-E(g_acq_simpl)$weight 
length(inv_weight)

#finding the diameter
diameter(g_acq_simpl, weights=inv_weight)


#betweenness centrality 
a <- betweenness(g_acq_simpl, v = V(g_acq_simpl), directed = TRUE, weights = inv_weight,
                 nobigint = TRUE, normalized = TRUE)
c(a["511"], a["541"], a["518"], a["519"])


#closeness centrality out 
b <- closeness(g_acq_simpl, vids = V(g_acq_simpl), mode = c("out"),
               weights = num_weight, normalized = FALSE)
c(b["511"], b["541"], b["518"], b["519"])

#closeness centrality in 
c <- closeness(g_acq_simpl, vids = V(g_acq_simpl), mode = c("in"),
               weights = num_weight, normalized = FALSE)
c(c["511"], c["541"], c["518"], c["519"])


#global clustering coefficient 
d <- transitivity(g_acq_simpl, type = c("undirected", "global", "globalundirected",
                                        "localundirected", "local", "average", "localaverage",
                                        "localaverageundirected", "barrat", "weighted"), vids = NULL,
                  weights = NULL, isolates = c("NaN", "zero"))
d


#distance of the shortest path
e <- distances(g_acq_simpl, v = c("511","541","518","519"), to = "814", mode = c("out"), 
               weights = inv_weight, 
               algorithm = c("automatic", "unweighted",
                             "dijkstra", "bellman-ford", "johnson"))

#distance of the shortest path
f <- distances(g_acq_simpl, v = "711", to = c("511","541","518","519"), mode = c("out"), 
               weights = inv_weight, 
               algorithm = c("automatic", "unweighted",
                             "dijkstra", "bellman-ford", "johnson"))


#induced sub graph 
sub_net<-induced.subgraph(g_acq_simpl, v=c('511', '541',
                                           '518', '519', '517', '325', '423', '446', '512', '523',
                                           '561', '621', '115', '482', '485', '487', '491', '492',
                                           '521', '712' ))

#altering weights of subgraph
E(sub_net)$weight 
# Will use the inverse of log weight for shortest path calculations
sub_net_inv<-1/log(E(sub_net)$weight  + 1)


#diameter of subgraph 
diameter(sub_net, weights=sub_net_inv)


#Coloring nodes 
V(sub_net)$color <- ifelse(V(sub_net)$name  %in% c("511","541","518","519"), "lightblue", "yellow")

#change shape of nodes 
V(sub_net)$shape <- ifelse(V(sub_net)$name  %in% c("511","541","518","519"), "square", "sphere")

#Change edge's thickness
E(sub_net)$weight <- sub_net_inv

#change vertex size 
V(sub_net)$size <- closeness(sub_net, vids = V(sub_net), mode = c("in"),
                             weights = sub_net_inv, normalized = FALSE) 

set.seed(47)

plot(sub_net, layout=layout.fruchterman.reingold, edge.width=E(sub_net)$weight, 
     edge.arrow.size=0.2, vertex.size=V(sub_net)$size*300)

large <- component.largest(get.adjacency(g_SAPSub_simpl, sparse=FALSE))


large_graph <- graph.data.frame(large, directed = TRUE, vertices= NULL)

plot(large_graph, layout=layout.fruchterman.reingold, edge.width=E(large_graph)$weight, 
     edge.arrow.size=0.2, vertex.size=V(large_graph)$size)