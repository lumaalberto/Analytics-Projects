
library(data.table)
library(magrittr)
library(fastcluster)
library(parallel)
library(amap)
library(ggplot2)
library(Matrix)

n.cores <- detectCores()

cat("Loading destination data...")
dest <- read.delim(file.choose(), header=T, sep=",") %>% as.matrix()
dest <- dest[sample(1:nrow(dest), 30000), ] # Not enough memory
cat("DONE\n")

gc()

cat("Dimensionality reduction...")
pca <- prcomp(dest, center = TRUE, scale. = TRUE) 
pc <- pca$x[, 1:2]
cat("DONE (take two main principal components)\n")

gc()

memory.size()
memory.size(TRUE)
memory.limit()

cat("Calculating distance matrix...")
distance.matrix <- Dist(pc, method = "euclidean", nbproc = n.cores - 1)
cat("DONE\n")


gc()

cat("Creating cluster tree...")
cluster.tree <- hclust(distance.matrix)
cat("DONE (see plot)\n")
rm(distance.matrix)
gc()

cat("Creating clusters...")
clusters <- cutree(cluster.tree, k = 15) # Chose the number of clusters
cat("DONE\n")
rm(cluster.tree)
gc()

data.plot <- data.table(PC1 = pc[,1], PC2 = pc[,2], cluster = as.factor(clusters))
ggplot(data = data.plot) + 
  geom_point(mapping = aes(x = PC1, y = PC2, color = cluster)) +
  xlab("Principal Component 1") +
  ylab("Principal Component 2") + 
  ggtitle("Destination clusters") +
  theme_bw()

