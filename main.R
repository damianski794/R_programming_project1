library("plot3D")
library('clusteringdatasets')
library(igraph)
#install.packages('dendextend')


source('spectral.R')
set.seed(5) 

n_centers = 4
n_features = 2
blobs <- make_blobs(n_samples = 200, n_features = n_features, centers = n_centers, cluster_std = 1)
blobs

plot(x = blobs$samples[,1], y = blobs$samples[,2], col = blobs$labels, cex=1, pch=16) #2D plot
for(i in 1: nrow(blobs$samples)){
  text(blobs$samples[i,1],y = blobs$samples[i,2],i, pos=3, cex=0.65)
}

#Mnn(blobs$samples,M = 3)
spectral_labels <-  spectral_clustering(X = blobs$samples, k = n_centers, M = 3)
dendextend::FM_index(blobs$labels, spectral_labels)

plot(x = blobs$samples[,1], y = blobs$samples[,2], col = spectral_labels, cex=1, pch=16)

#lines(c(blobs$samples[1,1],blobs$samples[1,2]),c(blobs$samples[2,1],blobs$samples[2,2]), type = 'l')
#print(blobs$samples[1,1])
#print(blobs$samples[1,2])

#print(blobs$samples[2,1])
#print(blobs$samples[2,2])

lines(c(blobs$samples[1,1],blobs$samples[2,1]),c(blobs$samples[1,2],blobs$samples[2,2]), type = 'l')
