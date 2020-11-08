spectral_clustering <- function(X, k, M, print_graph = FALSE){
  # X - array n x d in which we want to find clusters
  # k - number of distinct groups (clusters)
  # M - number of nearest neighbours
  
  # returns labels of found clusters
  
  S <- Mnn(X, M)
  G <- Mnn_graph(S, X, print_graph)
  E <- Laplacian_eigen(G,k)

  labels <- kmeans(x = E, centers = k)$cluster

  return(labels)
}


Mnn <- function(X, M = 3){
  # X - array n x d
  # M - number of nearest neighbours
  
  # returns S array n x M with indicies of nearest neighbours of each point 
  
  n <- nrow(X)
  d <- ncol(X)
  
  S = matrix(0.0, nrow = n, ncol = 1)
  
  
  distances <- as.matrix(dist(X))
  
  S <- matrix(0.0,nrow = n, ncol = M)
  
  for (i in 1:n){
    S[i,] <- order(distances[i,], decreasing = FALSE)[2:(M+1)]
  }
  
  return(S)
}

Mnn_graph <- function(S, X, print_graph = FALSE){
  # creates adjecency matrix from connected graph
  # prints graph if print_graph = TRUE
  
  # S - adjecency list (array)
  # X - data with points positions
  
  #returns connected adjecency matrix
  
  S_n_rows = nrow(S)
  G <- matrix(0.0, nrow = S_n_rows, ncol = S_n_rows)
  
  for (i in 1:S_n_rows){
    for (j in 1:ncol(S)) {
      G[i,S[i,j]] <- 1;
      G[S[i,j],i] <- 1;
    }
  }
  if(print_graph){
    print_first_graph_lines(G,X)
  }
  
  graph1 <- graph_from_adjacency_matrix(adjmatrix = G, mode='undirected')

  
  n_connected_comp <- count_components(graph1)
  while(n_connected_comp != 1){
    rand_row <- sample(1:nrow(G),1)
    rand_col <- sample(1:nrow(G),1)
    if(G[rand_row,rand_col]==1){
      next
    }
    else{
      G[rand_row,rand_col]=1
      G[rand_col,rand_row]=1
      graph1 <- graph_from_adjacency_matrix(adjmatrix = G, mode='undirected')
      n_connected_comp <- count_components(graph1)
      
      if(print_graph){
        lines(c(blobs$samples[rand_row,1],blobs$samples[rand_col,1]),c(blobs$samples[rand_row,2],blobs$samples[rand_col,2]), type = 'l', col='red')
      }
    }
  }
  
  
  return(G)
}

Laplacian_eigen <- function(G,k){
  # G - adjencency matrix n x n
  # k - number of distinct groups/clusters
  
  # returns E - laplacian array n x k
  D <- apply(G,1, sum)
  D <- diag(D)
  L <- D - G

  ev <- eigen(L)$values
  smallest_lambas_indices <- order(ev, decreasing = FALSE)[2:(k+1)]
  
  E <- eigen(L)$vectors[,smallest_lambas_indices]
  
  return(E)
}

print_first_graph_lines <- function(G, X){
  # prints graph from adjececy matrix and points data
  
  # G - adjencency matrix n x n
  # X - data with 2D points
  
  for (i in 1:nrow(G)){
    for (j in 1:ncol(G)) {
      if (G[i,j] == 1){
        lines(c(X[i,1],X[j,1]),c(X[i,2],X[j,2]), type = 'l')
      }
    }
  }
}

