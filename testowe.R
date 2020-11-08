#install.packages("remotes")
#remotes::install_github("elbamos/clusteringdatasets")

#install.packages("plot3D")
library("plot3D")


library('clusteringdatasets')
set.seed(5) #wyglada spoko dla 1d

n_centers = 3
n_features = 2
blobs <- make_blobs(n_samples = 20, n_features = n_features, centers = n_centers, cluster_std = 1)
plot(blobs$samples[,1])
#plot(blobs$samples, col=rainbow(n_centers)[blobs$labels])
plot(x = blobs$samples[,1], y= rep(0,length(blobs$samples[,1])), col = blobs$labels) # 1D
plot(x = blobs$samples[,1], y = blobs$samples[,2], col = blobs$labels) #2D
blobs


scatter3D(x = blobs$samples[,1], y = blobs$samples[,2], z = blobs$samples[,3], colvar = blobs$labels, col=rainbow(n_centers))#3D

class(blobs$samples)



xd = blobs
xd

