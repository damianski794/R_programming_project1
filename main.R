library("plot3D")
library('clusteringdatasets')
library(igraph)

source('spectral.R')

set.seed(5) 

n_centers = 6
n_features = 2
blobs <- make_blobs(n_samples = 100, n_features = n_features, centers = n_centers, cluster_std = 1)
if (n_features == 3){
  scatter3D(x = blobs$samples[,1], y = blobs$samples[,2], z = blobs$samples[,3], colvar = blobs$labels, col=rainbow(n_centers))#3D
  spectral_labels <-  spectral_clustering(X = blobs$samples, k = n_centers, M = 10)
  
  scatter3D(x = blobs$samples[,1], y = blobs$samples[,2], z = blobs$samples[,3], colvar = spectral_labels, col=rainbow(n_centers))#3D
  dendextend::FM_index(blobs$labels, spectral_labels)
}


if(n_features == 2){
  plot(x = blobs$samples[,1], y = blobs$samples[,2], col = blobs$labels, cex=1, pch=16) #2D plot
  for(i in 1: nrow(blobs$samples)){
    text(blobs$samples[i,1],y = blobs$samples[i,2],i, pos=3, cex=0.65)
  }
  spectral_labels <-  spectral_clustering(X = blobs$samples, k = n_centers, M = 3)
  plot(x = blobs$samples[,1], y = blobs$samples[,2], col = spectral_labels, cex=1, pch=16) #2D plot
  
  dendextend::FM_index(blobs$labels, spectral_labels)
}


