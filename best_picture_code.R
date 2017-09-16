
library(jsonlite)
library(dplyr)
library(igraph)

# set my working directory to folder with json file
# change this to your folder with the json file 
setwd('D:/MSBA/Fall2016/IDS564/projects/oscar_graph-master/')

# set the seed - change only here to update how the graphs are laid out
seeding <- 123

# import movie data
fullMovie <- fromJSON('nMovie_files.json')

# remove last movie since it was nominated in 1999
#fullMovie <- fullMovie[1:99,]

# remove columns that we will not use
movs <- select(fullMovie,c(title, actors, director))

# add column for best picture winners
best_picture <- rep('L',108)
best_picture[c(1,9,18,27,36,45,55,65,70,75,80,85,90,95,101)] <- 'W' 

best_picture <- rep('L',108)
best_picture[c(1,9,18,27,36,45,55,65,70,75,80,85,90,95,101)] <- 'W'

# add best picture column to dataframe
movs$best_picture <- best_picture

# create vector of actors
actors <- unlist(movs$actors)

# since there are four rows of actors, we link 
# all actors in each row toegther (below)
n1 <- actors[seq(1,length(actors),4)]
n2 <- actors[seq(2,length(actors),4)]
n3 <- actors[seq(3,length(actors),4)]
n4 <- actors[seq(4,length(actors),4)]

# this creates 2 columns with actor names
t1 <- append(n1,c(n1,n1,n2,n2,n3))
t2 <- append(n2,c(n3,n4,n3,n4,n4))

# here is the dataframe with from & to (direction does not matter [undirected])
# dat1 is just actor to actor, we will add actor to director with dat2
dat1 <- data.frame(from = t1, to = t2)


# created two null variables, we will add actors to act1 and directors to dir1
# in a way that all combinations or actors to directors are met for each film
act1 <- NULL
dir1 <- NULL

# we loop through every movie,
# in act1 we multiply the number of actors by the amount of directors for the movie 
# in dir2 we multiply each director by 4 (since 4 actors listed per movie)
# append values to act1 and dir1
### combines all combinations of actors/directors who worked together on a movie

for(i in seq_along(movs$actors)) {
  dirLen <- length(movs$director[[i]]) 
  act1 <- append(act1,rep(movs$actors[[i]], times = dirLen))
  dir1 <- append(dir1,rep(movs$director[[i]],each = 4))
}

# last we do another loop to add director to director links for co-directors 
for(i in seq_along(movs$director)){
  if(length(movs$director[[i]]) > 1){
    act1 <- append(act1,movs$director[[i]][1])
    dir1 <- append(dir1,movs$director[[i]][2])
  }
}

# create a dataframe called dat2, containing the actors-directors edgelist
dat2 <- data.frame(from = act1, to = dir1)

# last we combine our earlier dataframe (dat1) with our actor-dir dataframe (dat2)
# this the finaldataframe! It is essentialy an edge list --- from in one column to in other
relations <- rbind.data.frame(dat1,dat2)

# Now we want to create an attribute dataframe

# we first get an array of all uniqiue actors and directors
# we create a repeating array of "actors" or "directors"  
unqAct <- unlist(movs$actors) %>%  unique()
role <- rep('actor',length(unqAct))
unqDir <- unlist(movs$director) %>%  unique()

# we combine the two list so that all names are one column and roles are another
unqAct <- append(unqAct,unqDir)
role <- append(role,rep('director',length(unqDir)))

# we make it a dataframe
filmAtt <- data.frame(name = unqAct,role, best_picture = "N")

# we have to make best_picture and name columns into chracters types to edit them (were factors)
filmAtt$best_picture <- as.character(filmAtt$best_picture)
filmAtt$role <- as.character(filmAtt$role)


#  get a subset of the winning movies
winSub <- movs[movs$best_picture == "W",]

# get an array of all director and actor names from winnning movies
winners <- unlist(winSub$actors) %>% unique()
winners <- unlist(winSub$director) %>%  unique() %>%  append(winners,.) 

# subset filmAtt by winners and change best_picture to "Y" for these rows
filmAtt[filmAtt$name %in% winners,3] <- "Y"

# there can not be two nodes with the same name in the attribute dataframe
# so we have to find persons who were both actors and directors
# delete their director row (name/atrribute) and then change their role to actor/dir

# find persons who were boths actors and directors
dualActDir <- filmAtt[duplicated(filmAtt$name),1]

# remove their duplicated row (directors)
filmAtt <- filmAtt[!duplicated(filmAtt$name),]

# change roles for persons in dualActrDIr set to actor/dir
filmAtt[filmAtt$name %in% dualActDir,2] <- "actor/dir"

# create a graph object, it is undirected, and we are using filmAtt for vertex attrs
g <- graph.data.frame(relations,directed = F, vertices = filmAtt)

#write.csv("test.csv", E(g))

# plotting the network based on roles
# actors gray circles
# directors are black squares
# actor/directors are black rectangles
set.seed(seeding)

V(g)[V(g)$role == "actor"]$shape <- "circle"
V(g)[V(g)$role == "director"]$shape <- "square"
V(g)[V(g)$role == "actor"]$color <- "gray"
V(g)[V(g)$role == "director"]$color <- "black"
V(g)[V(g)$role == "actor/dir"]$shape <- "rectangle"
V(g)[V(g)$role == "actor/dir"]$color <- "black"


# a plot without labels --- just testing different layouts for best visual representation

plot(g, layout = layout.fruchterman.reingold, vertex.size = 4, edge.arrow.size = .3, vertex.label = NA)

set.seed(seeding)
V(g)[V(g)$best_picture == "Y"]$color = "yellow"
plot(g, layout = layout.fruchterman.reingold, vertex.size = 4, edge.arrow.size = .3, vertex.label = NA)

#------------------------------------------------------------------

# Network Metrics

# size - nodes
vcount(g)

# edge count
ecount(g)

# Network density
graph.density(g)

# degree distributions
plot(degree.distribution(g), type = "b", xlab = "node degree", ylab = "Probability", col='blue', lwd = 5)

# number of triangles
n_triangles <- count_triangles(g)
hist(n_triangles, border='blue',  main = '')

#Get all components 
w.win <- get.vertex.attribute(g, "best_picture")
gn <- clusters(g)
gn.cm <- membership(gn)
table(gn.cm, w.win)

#------------------------------------------------------------------
# Setting up the Largest Component #

# giant component function
giant.component <- function(graph) {
  cl <- clusters(graph)
  induced.subgraph(graph, which(cl$membership == which.max(cl$csize)))
}

gcomp <- giant.component(g)

#------------------------------------------------------------------
# Centrality

# Count and distribution of components
comps <- components(g)

### DEGREE CENTRALITY ###

# get the degrees
degree_all <- degree(g)
degree_gcomp <-degree(gcomp)

# plot that shows degree centrality and winners
V(g)$size <- degree_all*.8
V(g)$shape <- "circle"
V(g)$color <- "gray"
V(g)[V(g)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(g, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)

V(gcomp)$size <- degree_gcomp*.6
V(gcomp)$shape <- "circle"
V(gcomp)$color <- "gray"
V(gcomp)[V(gcomp)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(gcomp, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)


### CLOSENESS CENTRALITY ###

# calculate closeness
closeness_all <- closeness(g)
closeness_gcomp <- closeness(gcomp)

# plot that shows closeness centrality and winners
V(g)$size <- closeness_all*500000
V(g)$shape <- "circle"
V(g)$color <- "gray"
V(g)[V(g)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(g, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)

V(gcomp)$size <- closeness_gcomp*5000
V(gcomp)$shape <- "circle"
V(gcomp)$color <- "gray"
V(gcomp)[V(gcomp)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(gcomp, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)


### BETWEENNESS CENTRALITY ###

# calculate betweeness
betweenness_all <- betweenness(g)
betweenness_gcomp <- betweenness(gcomp)


# plot that shows closeness centrality and winners
V(g)$size <- betweenness_all/500
V(g)$shape <- "circle"
V(g)$color <- "gray"
V(g)[V(g)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(g, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)


V(gcomp)$size <- betweenness_gcomp/500
V(gcomp)$shape <- "circle"
V(gcomp)$color <- "gray"
V(gcomp)[V(gcomp)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(gcomp, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)



### EIGENVECTOR CENTRALITY ###

#calculate eigenvector centrality
evector_all <- evcent(g)$vector
evector_gcomp <- evcent(gcomp)$vector

# plot that shows closeness centrality and winners
V(g)$size <- evector_all*15
V(g)$shape <- "circle"
V(g)$color <- "gray"
V(g)[V(g)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(g, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)

V(gcomp)$size <- evector_gcomp*20
V(gcomp)$shape <- "circle"
V(gcomp)$color <- "gray"
V(gcomp)[V(gcomp)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(gcomp, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)


### CLUSTERING COEFCICIENTS ###

# calculate clustering coefficient
clustcoeff_all <- transitivity(g, type = "local")
clustcoeff_gcomp <- transitivity(gcomp, type = "local")

# plot that shows closeness centrality and winners
V(g)$size <- clustcoeff_all*7
V(g)$shape <- "circle"
V(g)$color <- "gray"
V(g)[V(g)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(g, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)


V(gcomp)$size <- clustcoeff_gcomp*7
V(gcomp)$shape <- "circle"



V(gcomp)$color <- "gray"

V(gcomp)[V(gcomp)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)

plot(gcomp, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)


### EXPORT NETWORK CENTRALITY METRICS TO CSV ###

# create dataframe and export
node_stats_all <- cbind(filmAtt, 
                       comps$membership, 
                       degree_all,  
                       closeness_all, 
                       betweenness_all, 
                       evector_all, 
                       clustcoeff_all)

head(node_stats_all)
sqldf("select avg(degree_all), avg(closeness_all), avg(betweenness_all),
      avg(evector_all), avg(clustcoeff_all) from node_stats_all")

sqldf("select  avg(degree_all), avg(closeness_all), avg(betweenness_all),
      avg(evector_all), avg(clustcoeff_all) from node_stats_all where comps$membership = 1")

write.csv(node_stats_all, 'oscar_all_node_stats.csv')


### SHORTEST PATH ###

#shortest path - isn't helpful because it's INF for the whole network
sp_all <- shortest.paths(g)
sp_all_vec <- vector()
for (i in 1:vcount(g)) {
  sp_all_vec[i] <- mean(sp_all[i,])
}

# shortest path just for the giant component

sp_gcomp <- shortest.paths(gcomp)
sp_gcomp_vec <- vector()
for (i in 1:vcount(gcomp)) {
  sp_gcomp_vec[i] <- mean(sp_gcomp[i,])
}


V(gcomp)$size <- (sp_gcomp_vec-3)*3
V(gcomp)$shape <- "circle"
V(gcomp)$color <- "gray"
V(gcomp)[V(gcomp)$best_picture == "Y"]$color = "yellow"
set.seed(seeding)
plot(gcomp, layout=layout.fruchterman.reingold, edge.arrow.size = 2, vertex.label = NA)

# Get reachability
# This code isn't working for some reason
# Maybe because it's an undirected graph and I took this from a directed example?

reachability <- function(g, m) {
  reach_mat = matrix(nrow = vcount(g), 
                     ncol = vcount(g))
  for (i in 1:vcount(g)) {
    reach_mat[i,] = 0
    this_node_reach <- subcomponent(g, (i - 1), mode = m)
    
    for (j in 1:(length(this_node_reach))) {
      alter = this_node_reach[j] + 1
      reach_mat[i, alter] = 1
    }
  }
  return(reach_mat)
}

reach_all <- reachability(g,'all')

### Community Metrics ###

# reset gcomp size
V(gcomp)$size <- 4.5

# simplify gcomp
gcomp <- simplify(gcomp)

## Cliques

# find the total amount of cliques
# did not print cliques(gcomp) becuase of amount of cliques
length(cliques(gcomp))

# find the largest cliques 
largest.cliques(gcomp)


## kcores
# the results are all 4 or 5 core
# not much value and the tough to vuew labels 
kc <- coreness(gcomp, mode="all")
plot(gcomp, vertex.size=kc*1.2, vertex.label=kc)

## Idenitify Communites

# edge betweeness method
ceb <- cluster_edge_betweenness(gcomp)
plot(ceb,gcomp, vertex.label = NA, main = "Edge Betweneess Communities")

#Get all components 
w.gcom <- get.vertex.attribute(gcomp, "best_picture")
ceb.cm <- membership(ceb)
table(ceb.cm, w.gcom)

# since we cannot change colors when plotting clusters
# we will use squares to idenitfy winners
V(gcomp)[V(gcomp)$best_picture == "Y"]$shape = "square"
V(gcomp)[V(gcomp)$best_picture == "N"]$shape = "circle"
plot(ceb,gcomp, vertex.label = NA, main = "Edge Betweenness Communities")

# fast greedy method 
cfg <- cluster_fast_greedy(gcomp)
plot(ceb,gcomp, vertex.label = NA, layout =layout.fruchterman.reingold, main = "Fast Greedy Community")

## export community associations as a csv
vert_dat <- igraph::as_data_frame(gcomp, what ="vertices")
gcomp_dat <- data.frame(vert_dat[,c(1,2,3)], cluster_eb = ceb$membership,
                        cluster_cfg = cfg$membership)
# write to csv
write.csv(gcomp_dat, file = "communites.csv", row.names = FALSE)

### examine a promising community 
c9 <- induced.subgraph(gcomp, V(gcomp)$name[ceb$membership == 9])
V(c9)[V(c9)$role == "actor"]$shape <- "square"
V(c9)[V(c9)$role == "director"]$shape <- "circle"
V(c9)$color <- "grey"
V(c9)[V(c9)$role == "actor/dir"]$shape <- "rectangle"
V(c9)[V(c9)$best_picture == "Y"]$color = "gold"

plot(c9, layout = layout.davidson.harel, vertex.label.cex = .7, main = "Community 9", vertex.size = 8)

### homophily
# use assortactivity to find homophily of graph when best picture winners are a factor
assortativity.nominal(gcomp, as.factor(V(gcomp)$best_picture))



