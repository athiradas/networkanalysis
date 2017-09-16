rm(list=ls())


# CHUNK 1
library(sand)
fblog<-upgrade_graph(fblog)

nv <- vcount(fblog)
ncn <- numeric()
A <- get.adjacency(fblog)

# Find the number of common neighbors for each pair of nodes in the fblog network
for(i in (1:(nv-1))){
  ni <- neighborhood(fblog, 1, i)
  nj <- neighborhood(fblog, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}

# CHUNK 2
library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"))
title(ylab="Number of Common Neighbors")


library(huge)
set.seed(42)
huge.out <- huge(Ecoli.expr)

huge.opt <- huge.select(huge.out, criterion="ric")
summary(huge.opt$refit)

huge.opt <- huge.select(huge.out, criterion="stars")
g.huge <- graph.adjacency(huge.opt$refit, "undirected")
summary(g.huge)

nv <- vcount(g.huge)
ncn <- numeric()
A <- get.adjacency(g.huge)

# Find the number of common neighbors for each pair of nodes in the fblog network
for(i in (1:(nv-1))){
  ni <- neighborhood(g.huge, 1, i)
  nj <- neighborhood(g.huge, 1, (i+1):nv)
  nbhd.ij <- mapply(intersect, ni, nj, SIMPLIFY=FALSE)
  temp <- unlist(lapply(nbhd.ij, length)) - 
    2*A[i, (i+1):nv]
  ncn <- c(ncn, temp)
}


library(vioplot)
Avec <- A[lower.tri(A)]
vioplot(ncn[Avec==0], ncn[Avec==1], 
        names=c("No Edge", "Edge"))
title(ylab="Number of Common Neighbors")






# CHUNK 3
library(ROCR)
pred <- prediction(ncn, Avec)
perf <- performance(pred, "auc")
slot(perf, "y.values")
# ---
## [[1]]
## [1] 0.9275179
# ---

# CHUNK 4
rm(list=ls())
data(Ecoli.data)
ls()
# ---
## [1] "Ecoli.expr" "regDB.adj"
# ---

# CHUNK 5
heatmap(scale(Ecoli.expr), Rowv=NA)

# CHUNK 6
library(igraph)
g.regDB <- graph.adjacency(regDB.adj, "undirected")
summary(g.regDB)
# ---
## IGRAPH UN-- 153 209 -- 
## attr: name (v/c)
# ---

# CHUNK 7
plot(g.regDB, vertex.size=3, vertex.label=NA)


# Note the code of CHUNK 8 THROUGH CHUNK 13 uses the full correlation matrix to predict edges in the network
# CHUNK 8
mycorr <- cor(Ecoli.expr)

# CHUNK 9
# Fisher's transformation
z <- 0.5 * log((1 + mycorr) / (1 - mycorr))

# CHUNK 10
z.vec <- z[upper.tri(z)]
n <- dim(Ecoli.expr)[1]
corr.pvals <- 2 * pnorm(abs(z.vec), 0, 
                        sqrt(1 / (n-3)), lower.tail=FALSE)

# CHUNK 11
length(corr.pvals)
# ---
## [1] 11628
# ---

# CHUNK 12
# Benjamini-Hochberg adjustment to control for the false discovery rate
corr.pvals.adj <- p.adjust(corr.pvals, "BH")

# CHUNK 13
# Number of edges predicted: using statistical significance at the p < 0.05 threshold
length(corr.pvals.adj[corr.pvals.adj < 0.05])
# ---
## [1] 5227
# ---


# CHUNK 14
library(fdrtool)

# CHUNK 15
mycorr.vec <- mycorr[upper.tri(mycorr)]
fdr <- fdrtool(mycorr.vec, statistic="correlation")

# Note the code of CHUNK 16 through CHUNK 19 uses partial correlations to predict edges
# CHUNK 16
pcorr.pvals <- matrix(0, dim(mycorr)[1], 
                      dim(mycorr)[2])
for(i in seq(1, 153)){
  for(j in seq(1, 153)){
    rowi <- mycorr[i, -c(i, j)]
    rowj <- mycorr[j, -c(i, j)]
    tmp <- (mycorr[i, j] - 
              rowi*rowj)/sqrt((1-rowi^2) * (1-rowj^2))
    tmp.zvals <- (0.5) * log((1+tmp) / (1-tmp))
    tmp.s.zvals <- sqrt(n-4) * tmp.zvals
    tmp.pvals <- 2 * pnorm(abs(tmp.s.zvals), 
                           0, 1, lower.tail=FALSE)
    pcorr.pvals[i, j] <- max(tmp.pvals)
  }
}

# CHUNK 17
pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
# Benjamini-Hochberg adjustment to control for the false discovery rate
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

# CHUNK 18
pcorr.edges <- (pcorr.pvals.adj < 0.05)
length(pcorr.pvals.adj[pcorr.edges])
# ---
## [1] 25
# ---

# CHUNK 19
# Create the graph predicted by the statistically significant partial correlations
pcorr.A <- matrix(0, 153, 153)
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")

# CHUNK 20
# Find overlap between the graph predicted by partial correlations with the known gene linkages uncovered in prior research (from g.regDB)
str(graph.intersection(g.regDB, g.pcorr, byname=FALSE))

# CHUNK 21
# FDR tool can also be used to adjust for false discovery rate and predict new edges based on partial correlations
fdr <- fdrtool(pcorr.pvals.vec, statistic="pvalue", 
               plot=FALSE)
pcorr.edges.2 <- (fdr$qval < 0.05)
length(fdr$qval[pcorr.edges.2])


# HUGE (High-dimensional undirected graph estimation library) procedure for predicted links
# CHUNK 22
library(huge)
set.seed(42)
huge.out <- huge(Ecoli.expr)

# CHUNK 23
huge.opt <- huge.select(huge.out, criterion="ric")
summary(huge.opt$refit)

# CHUNK 24
huge.opt <- huge.select(huge.out, criterion="stars")
g.huge <- graph.adjacency(huge.opt$refit, "undirected")
summary(g.huge)
# ---
## IGRAPH U--- 153 759 --
# ---

# CHUNK 25
# Find overlap between the graph produced by the partial correlations with graph produced by HUGE library
str(graph.intersection(g.pcorr, g.huge))

# CHUNK 26
# Find overlap between the graph predicted by HUGE library with the known gene linkages uncovered in prior research.
str(graph.intersection(g.regDB, g.huge, byname=FALSE))


