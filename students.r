rm(list=ls())  

## Load package
library(igraph)
library(sqldf)
library(poweRlaw)

dir_path <- "D:/MSBA/Fall2016/IDS564/labs/adv lab 3/"
setwd(dir_path)

nodes<-"NodesNetList_corrected_Feb19_2016 (1).csv"
edges<-"CollabNetEdgeListFilteredDec7_2012.csv"

node_frame=read.csv(nodes, header = TRUE, sep = ",")
edge_frame=read.csv(edges, header = TRUE, sep = ",")

graph_sap=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)
summary(graph_sap)
# Edges
ecount(graph_sap)
## Vertices
vcount(graph_sap)

# Create edge weights
E(graph_sap)$weight <-1
E(graph_sap)$weight 
graph_sap_simpl<-simplify(graph_sap, edge.attr.comb="sum")
is.simple(graph_sap_simpl)
# Edges
ecount(graph_sap_simpl)
## Vertices
vcount(graph_sap_simpl)
# Use the inverse of log weight for some of the network measure calculations
inv_weight<-1/log(E(graph_sap_simpl)$weight  + 1)
num_weight<-E(graph_sap_simpl)$weight 
length(inv_weight)
E(graph_sap_simpl)$weight <-inv_weight


#Remove isolated nodes form the structure 
sum(degree(graph_sap_simpl)==0)
graph_sap_simpl_conn <- induced_subgraph(graph_sap_simpl, vids=V(graph_sap_simpl) %in% 
                                          which(degree(graph_sap_simpl)!=0))
# Edges
ecount(graph_sap_simpl_conn)
## Vertices
vcount(graph_sap_simpl_conn)

#Community Detection 
sap_country <- get.vertex.attribute(graph_sap_simpl_conn, "country")
sap_ln_points<- get.vertex.attribute(graph_sap_simpl_conn, "ln_points")

#Sys.time()
#fastgreedy.community(graph_sap_simpl_conn, weights=E(graph_sap_simpl_conn)$weight)
#Sys.time()
#membership()

#Sys.time()
#walktrap.community(graph_sap_simpl_conn, weights=E(graph_sap_simpl_conn)$weight)
#Sys.time()
#3communities(walktrap.community(graph_sap_simpl_conn, weights=E(graph_sap_simpl_conn)$weight))

#Sys.time()
#label.propagation.community(graph_sap_simpl_conn, weights=E(graph_sap_simpl_conn)$weight)
#Sys.time()
#communities(label.propagation.community(graph_sap_simpl_conn, weights=E(graph_sap_simpl_conn)$weight))

set.seed(134)
sap_fg <- fastgreedy.community(graph_sap_simpl_conn, weights=E(graph_sap_simpl_conn)$weight)


#Find the community features 
sap_fg_cm <- membership(sap_fg)
sap_fg_cc <- communities(sap_fg)
#convert the data frame to tables to get insights 
sap_country_count <- as.data.frame(table(sap_fg_cm, sap_country, useNA = c("no")))
#find the total number of communities formed
sqldf("select count(distinct sap_fg_cm) from sap_country_count")
#Filterr the non onnuring entries from the table object
sap_country_count_rel <- sap_country_count[sap_country_count$Freq > 0,]
#Filter the countries with null value for country
sap_country_count_rel <- sap_country_count_rel[sap_country_count_rel$sap_country!="",]
#Find the number of distinct countries in each community
nodeacount_comm <- sqldf("select sap_fg_cm, count(distinct sap_country) as count from sap_country_count_rel 
      group by sap_fg_cm")
#Find the communities where only one country is involved
sqldf("select count(distinct sap_fg_cm) from  nodeacount_comm where count = 1")
#Find communities at lease 5 more than 5 countries are involved
sap_pop_comm <- sqldf("select * from  nodeacount_comm where count >= 5")
#Find the countries which participates in different communities
sqldf("select sap_country, count(distinct sap_fg_cm) from sap_country_count_rel 
      group by sap_country order by count(distinct sap_fg_cm) desc")
#The community with with most number of participating countries 
sqldf('select sap_fg_cm, count from  nodeacount_comm order by count desc limit 1 ')
#Number of nodes in community 2
length(sap_fg_cc$"2")
#Find number of nodes in each community 
comm_node_count <- lapply(sap_fg_cc, length)
comm_node_count[33]

#Find the points generated in each community
sap_points_count <- as.data.frame(table(sap_fg_cm, sap_ln_points, useNA = c("no")))
sqldf("select sap_fg_cm, sum(sap_ln_points*Freq) 
    from sap_points_count group by sap_fg_cm order by sum(sap_ln_points*Freq) desc limit 25")
sqldf("select * from  nodeacount_comm where count > 5 order by count desc limit 25")
sqldf("select sap_fg_cm, count(sap_country) as count from sap_country_count_rel 
      group by sap_fg_cm order by count(sap_country) desc limit 10")

##Create a displ object
m = displ$new(sap_fg_cm)
##Estimate the cut-off
estimate_xmin(m)
m$setXmin(105); m$setPars(1.644)

##Plot the data and the PL line
plot(m)
lines(m, col=2)

#Assign Community to nodes. 
V(graph_sap_simpl_conn)$community <- membership(sap_fg)

#Get few communities and plot
graph_sap_comm <- induced.subgraph(graph_sap_simpl_conn, 
                                   which(V(graph_sap_simpl_conn)$community %in% c(44,55,66,77)))
graph_sap_commlarge <- induced.subgraph(graph_sap_simpl_conn, 
                                   which(V(graph_sap_simpl_conn)$community == 2))

V(graph_sap_comm)$color <- ifelse( (V(graph_sap_comm)$country == "India" 
                                    | V(graph_sap_comm)$country == "United States"),
                                   "lightblue", "red")
V(graph_sap_commlarge)$color <- ifelse( (V(graph_sap_commlarge)$country == "India" 
                                    | V(graph_sap_commlarge)$country == "United States"),
                                   "lightblue", "red")
  

#plot the graph 
plot(graph_sap_comm,mark.groups = 
     communities(fastgreedy.community(graph_sap_comm, weights=E(graph_sap_simpl_conn)$weight)),
     layout=layout.fruchterman.reingold, edge.width=E(graph_sap_comm)$weight, 
     edge.arrow.size=0.02, vertex.size=V(graph_sap_comm)$size,
     vertex.label=V(graph_sap_comm)$country)

plot(graph_sap_commlarge,
     mark.groups = 
       communities(fastgreedy.community(graph_sap_commlarge, weights=E(graph_sap_simpl_conn)$weight)),
     layout=layout.fruchterman.reingold, edge.width=E(graph_sap_commlarge)$weight, 
     edge.arrow.size=0.02, vertex.size=V(graph_sap_commlarge)$size,
     vertex.label=NA)


#################################################################################
community.significance.test(graph_sap_simpl, which(V(graph_sap_simpl_conn)$community == 2))

community.significance.test <- function(graph, vs, ...) {
  if (is.directed(graph)) stop("This method requires an undirected graph")
  subgraph <- induced.subgraph(graph, vs)
  in.degrees <- degree(subgraph)
  out.degrees <- degree(graph, vs) - in.degrees
  wilcox.test(in.degrees, out.degrees, ...)
}
