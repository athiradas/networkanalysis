getwd()
# Save the data file to a location on your hard drive and specify the path here 
#(Windows systems use forward slashes)
dir_path <- 'D:/MSBA/Fall2016/IDS564/labs/regular lab 3/'
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  

par(mfrow=c(2,2))

# Load primary school data, contact data
infile_edges<-"Edges_sp_data_school_day_2.csv"
infile_nodes<-"Nodes_sp_data_school_day_2.csv"

## Load package
library(igraph)
edge_frame=read.csv(infile_edges, header = TRUE, sep = ",")
node_frame=read.csv(infile_nodes, header = TRUE, sep = ",")

g_primschool=graph.data.frame(edge_frame, directed = FALSE, vertices= node_frame)

stud.class <- get.vertex.attribute(g_primschool, "classname")
stud.gender<- get.vertex.attribute(g_primschool, "gender")

school_comm_fast <- fastgreedy.community(g_primschool, weights=E(g_primschool)$weight)

fg <- fastgreedy.community(g_primschool, merges=TRUE, modularity=TRUE,
                           weights=E(g_primschool)$weight)
fg.cm <- membership(fg)
table(fg.cm, stud.class, useNA = c("no"))
plot(fg,g_primschool, col = membership(fg),
     mark.groups = communities(fg), vertex.label= V(g_primschool)$classname)





wt <- walktrap.community(g_primschool, merges=TRUE, modularity=TRUE,
                         weights=E(g_primschool)$weight)
wt.cm <- membership(wt)
table(wt.cm, stud.class, useNA = c("no"))

plot(wt,g_primschool, col = membership(wt),
     mark.groups = communities(wt), vertex.label= V(g_primschool)$classname)



sg <- spinglass.community(g_primschool,
                          weights=E(g_primschool)$weight)
sg.cm <- membership(sg)
table(sg.cm, stud.class, useNA = c("no"))
plot(sg,g_primschool, col = membership(sg),
     mark.groups = communities(sg), vertex.label= V(g_primschool)$classname)

lpc <- label.propagation.community(g_primschool,
                                   weights=E(g_primschool)$weight)
lpc.cm <- membership(lpc)
table(lpc.cm, stud.class, useNA = c("no"))
plot(lpc,g_primschool, col = membership(lpc),
     mark.groups = communities(lpc), vertex.label= V(g_primschool)$classname)






g <- graph.formula(A-B,B-C,C-D, D-E, E-A)

E(g)$sign<-c(+1,-1, -1, +1, 1)

g <- graph.formula(A-B,A-C,A-D, B-C, B-D, C-D )

E(g)$sign<-c(+1,1, -1, 1, -1, 1)

plot(g)



gn <- cluster_edge_betweenness(g_primschool, weights = E(g_primschool)$weight, directed = TRUE,
                                edge.betweenness = TRUE, merges = TRUE, bridges = TRUE,
                                modularity = TRUE, membership = TRUE)
gn.cm <- membership(gn)
table(gn.cm, stud.class, useNA = c("no"))

plot(gn,g_primschool, col = membership(gn),
     mark.groups = communities(gn), vertex.label= V(g_primschool)$classname)



v_grade1students<-V(g_primschool)[V(g_primschool)$classname=="1B" | V(g_primschool)$classname=="1A"]
v_grade5students<-V(g_primschool)[V(g_primschool)$classname=="5B" | V(g_primschool)$classname=="5A"]

subgraph_grade1<-induced_subgraph(g_primschool, v_grade1students)
subgraph_grade5<-induced_subgraph(g_primschool, v_grade5students)

fg1 <- fastgreedy.community(subgraph_grade1, merges=TRUE, modularity=TRUE,
                           weights=E(subgraph_grade1)$weight)
fg5 <- fastgreedy.community(subgraph_grade5, merges=TRUE, modularity=TRUE,
                            weights=E(subgraph_grade5)$weight)
fg1.cm <- membership(fg1)
table(fg1.cm, stud.gender, useNA = c("no"))
fg5.cm  <- membership(fg5)
table(gn.cm, stud.gender, useNA = c("no"))



