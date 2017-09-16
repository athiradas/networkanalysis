rm(list=ls())  

## Load package
library(igraph)

dir_path <- "D:/MSBA/Fall2016/IDS564/labs/adv lab 2/"
setwd(dir_path)

infile_sub<-"SAPFull_SubGraph_EdgeList.csv"

library(igraph)
el=read.csv(infile_sub, header = TRUE, sep = ",")
class(el)

# Create the directed graph object
g_SAPSub=graph.data.frame(el, directed = TRUE, vertices= NULL)


# Create edge weights
E(g_SAPSub)$weight <-1
E(g_SAPSub)$weight 
g_SAPSub_simpl<-simplify(g_SAPSub, edge.attr.comb="sum")
is.simple(g_SAPSub_simpl)

# Edges
ecount(g_SAPSub_simpl)
## Vertices
vcount(g_SAPSub_simpl)

# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(g_SAPSub_simpl)$weight  + 1)
num_weight<-E(g_SAPSub_simpl)$weight 
length(inv_weight)
E(g_SAPSub_simpl)$weight <-inv_weight

E(g_SAPSub_simpl)$weight <- inv_weight

#The appoach over here is to indentify the central nodes and then further investigate the part of the network which is connected to the central node.

#View the trend based on degree centrality 
degree_total <- degree(g_SAPSub_simpl, mode = 'total')
degree_in <- degree(g_SAPSub_simpl, mode = 'in')
degree_out <- degree(g_SAPSub_simpl, mode = 'out')
hist_value_degree_total <- hist(degree_total, breaks = 454)
hist_value_degree_in <- hist(degree_in, breaks = 30)
hist_value_degree_out <- hist(degree_out, breaks = 454)
hist_value_degree_total$breaks
hist_value_degree_total$counts
V(g_SAPSub_simpl)$degreeout <- degree(g_SAPSub_simpl, mode = 'out')
which(V(g_SAPSub_simpl)$degreeout > 100)

neighbors(g_SAPSub_simpl, v=c('592540')) # the one with highest degree centrality 

#View the trend based on betweenness centrality 
btwn <- round(betweenness(g_SAPSub_simpl, v=V(g_SAPSub_simpl), directed = TRUE, nobigint =TRUE, normalized = FALSE))
hist_value_btwn <- hist(btwn[btwn != 0], breaks = 300)
hist_value_btwn$breaks
hist_value_btwn$counts
neighbors(g_SAPSub_simpl, v=c('592540'))

# Diameter with both kinds of weights - maximum distance between any two nodes 
diameter(g_SAPSub_simpl, weights= num_weight)
#26
diameter(g_SAPSub_simpl, weights= inv_weight)
#14.27 
# Avg. path length and diameter
average.path.length(g_SAPSub_simpl, directed=TRUE)
#3.982714
#paths of the lead node
V(g_SAPSub_simpl), mode = c("out", "all", "in"), weights = num_weight)

#View the trend based on closeness centrality 
close_total <- closeness(g_SAPSub_simpl, vids = V(g_SAPSub_simpl), mode = c("total"),
               weights = inv_weight, normalized = TRUE)
close_in <- closeness(g_SAPSub_simpl, vids = V(g_SAPSub_simpl), mode = c("in"),
               weights = inv_weight, normalized = TRUE)
close_out <- closeness(g_SAPSub_simpl, vids = V(g_SAPSub_simpl), mode = c("out"),
               weights = inv_weight, normalized = TRUE)			   
hist_value <- hist(close_total[close_total != 0], breaks = 300)
hist_value$breaks
hist_value$counts
closeness(g_SAPSub_simpl, v=c('592540'), normalized = TRUE)


reciprocity(g_SAPSub_simpl)
# 0.005825243
#Which means there are less dyadic relation ships between the nodes 
is.connected(g_SAPSub_simpl)
#FALSE
is.connected(g_SAPSub_simpl, mode="strong")
#FALSE
is.connected(g_SAPSub_simpl, mode="weak")
#FALSE
#The network is neither strong nor weak


# Clustering
transitivity(g_SAPSub_simpl, "global" ,weights = inv_weight)
#[1] 0.009985725
tranisivity_SAP <- (transitivity(g_SAPSub_simpl, "local" ,weights = inv_weight))
hist_value <- hist(tranisivity_SAP[tranisivity_SAP != 0], breaks = 100)
hist_value$breaks
hist_value$counts


#edge betweenness
E(g_SAPSub_simpl)$btwn <- edge.betweenness(g_SAPSub_simpl)
which(E(g_SAPSub_simpl)$btwn > 20000)
ends(g_SAPSub_simpl,2153)
#     [,1]      [,2]    
#[1,] "4183082" "592540"


# Summarize the graph structure
summary(g_SAPSub_simpl)
#IGRAPH DNW- 3415 4120 -- 
#+ attr: name (v/c), weight (e/n)


# Clique structure: 5 cliques of size 5, 39 cliques of size 4, 335 triangles
table(sapply(maximal.cliques(g_SAPSub_simpl), length))
#   2    3    4    5 
#3320  335   39    5 

#induced sub graph

sub_net <- 
  induced.subgraph(g_SAPSub, v= c ("592540",
                                   "131143",
                                   "821176",
                                   "3583224",
                                   "22328",
                                   "5420",
                                   "983891",
                                   "3621372",
                                   "653026",
                                   "3519026",
                                   "1675631",
                                   "2704623",
                                   "44034",
                                   "1195854",
                                   "3510478",
                                   "701187",
                                   "2711777",
                                   "3552437",
                                   "900"
  ))

  # Create edge weights
E(sub_net)$weight <-1
E(sub_net)$weight 
sub_net_simpl<-simplify(sub_net, edge.attr.comb="sum")
is.simple(sub_net_simpl)

# Edges
ecount(sub_net_simpl)
## Vertices
vcount(sub_net_simpl)

# Use the inverse of log weight for some of the network measure calculations
inv_weight_simpl<-1/log(E(sub_net_simpl)$weight  + 1)
num_weight_simpl<-E(sub_net_simpl)$weight 
length(inv_weight_simpl)
E(sub_net_simpl)$weight <-inv_weight_simpl

E(sub_net_simpl)$weight <- inv_weight_simpl


V(sub_net)$color <- ifelse(V(sub_net)$name  %in% c("592540","131143"), "lightblue", "yellow")
V(sub_net)$shape <- ifelse(V(sub_net)$name  %in% c("592540","131143"), "sphere", "circle")

plot(sub_net, layout=layout.kamada.kawai, edge.width=E(sub_net)$weight, 
     edge.arrow.size=0.02, vertex.size=V(sub_net)$size)

#induced sub graph 2 - for gephi 

sub_net<-induced.subgraph(g_SAPSub, v=neighbors(g_SAPSub, v=c('592540')))


# Create edge weights
E(sub_net)$weight <-1
E(sub_net)$weight 
sub_net_simpl<-simplify(sub_net, edge.attr.comb="sum")
is.simple(sub_net_simpl)

# Edges
ecount(sub_net_simpl)
## Vertices
vcount(sub_net_simpl)

# Use the inverse of log weight for some of the network measure calculations
inv_weight_simpl<-1/log(E(sub_net_simpl)$weight  + 1)
num_weight_simpl<-E(sub_net_simpl)$weight 
length(inv_weight_simpl)
E(sub_net_simpl)$weight <-inv_weight_simpl

E(sub_net_simpl)$weight <- inv_weight_simpl

