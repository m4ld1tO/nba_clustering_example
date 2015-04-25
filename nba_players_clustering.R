library(ggplot2)
library(fpc)
library(NbClust)
library(ggdendro)
set.seed(1234)

#load file
setwd("~/coding/machine_learning/nba_player_clustering")
df <- read.table("nba_player_stats_2014-2015.txt",
                 sep="\t",
                 stringsAsFactors=F,
                 quote="",
                 header=T)

#remove names from df and make row name 
players <- df$player
df <- df[-c(1,2)]
rownames(df) <- players

#remove some columns
df <- df[,c("minutes_played", "points", "three_pointers_made", "steals", "blocks", "rebounds", "assists")]

#scale vars
dfs <- scale(df)

#within groups sum of squares
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

#number of clusters
wssplot(dfs)


#distance matrix
d <- dist(dfs)

#kmeans
km <- kmeans(dfs, 4)

#plotting cluster
plotcluster(dfs, km$cluster)

#facets, plotting all attributes in original data
with(df, pairs(dfs, col=c(1:7)[km$cluster]))

#showing players in same clusters
player_clusters <- as.data.frame(km$cluster)
names(player_clusters) <- "cluster"
player_clusters$players <- rownames(dfs) 
rownames(player_clusters) <- NULL
player_clusters[order(player_clusters$cluster), c(2,1)]


#plotting example of 2 dimensions
ggplot(df, aes(x=blocks, y=three_pointers_made, color=as.factor(km$cluster))) + geom_point(size=10)




#hierarchical clustering

#distance matrix
d <- dist(dfs, method = "euclidean")

#agglomerative clustering
hc <- hclust(d)

#make dendrogram
hcd <- as.dendrogram(hc)

#plot
ggdendrogram(hcd, rotate=T)

#cutting certain groups
ggdendrogram(cut(hcd, h=5)$lower[[1]], rotate=T, size=2)
