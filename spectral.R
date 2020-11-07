spectral_clustering(X, k, M){
  # X - macierz n x d
  # k - liczba roznych grup danych
  # M - liczba najbilizszych sasiadow
}

Mnn <- function(X, M = 3){
  M=3
  
  set.seed(5) #wyglada spoko dla 1d
  
  n_centers = 3
  n_features = 2
  blobs <- make_blobs(n_samples = 20, n_features = n_features, centers = n_centers, cluster_std = 1)
  blobs
  
  plot(x = blobs$samples[,1], y = blobs$samples[,2], col = blobs$labels) #2D plot
  for(i in 1: nrow(blobs$samples)){
    text(blobs$samples[i,1],y = blobs$samples[i,2],i, pos=3, cex=0.65)
  }
  
  samples <- blobs$samples
  samples
  
  X <- samples[,]
  X
  
  n <- nrow(X)
  d <- ncol(X)
  
  S = matrix(0.0, nrow = n, ncol = 1)
  
  X
  
  distances <- as.matrix(dist(X))
  distances
  
  S <- matrix(0.0,nrow = n, ncol = M)
  S
  #distances[2,]
  
  #order(distances[2,], decreasing = FALSE)
  #order(distances[2,], decreasing = FALSE)[2:(M+1)]
  
  #S[2,] <- order(distances[2,], decreasing = FALSE)[2:(M+1)]
  #S
  #n
  for (i in 1:n){
    #print(i)
    S[i,] <- order(distances[i,], decreasing = FALSE)[2:(M+1)]
  }
  S
}

