spectral_clustering <- function(X, k, M){
  # X - macierz n x d
  # k - liczba roznych grup danych
  # M - liczba najblizszych sasiadow
  
  # returns labels
  
  S <- Mnn(X, M)
  G <- Mnn_graph(S, X)
  E <- Laplacian_eigen(G,k)
  print(kmeans(x = E, centers = k))
  labels <- kmeans(x = E, centers = k)$cluster
  print('labels')
  print(labels)
  return(labels)
}


Mnn <- function(X, M = 3){
  
  n <- nrow(X)
  d <- ncol(X)
  
  S = matrix(0.0, nrow = n, ncol = 1)
  
  #X
  
  distances <- as.matrix(dist(X))
  #distances
  
  S <- matrix(0.0,nrow = n, ncol = M)
  S
  #distances[2,]
  
  #order(distances[2,], decreasing = FALSE)
  #order(distances[2,], decreasing = FALSE)[2:(M+1)]
  
  #S[2,] <- order(distances[2,], decreasing = FALSE)[2:(M+1)]
  #S
  #n
  for (i in 1:n){
    S[i,] <- order(distances[i,], decreasing = FALSE)[2:(M+1)]
  }
  
  return(S)
}

Mnn_graph <- function(S, X){
  S_n_rows = nrow(S)
  G <- matrix(0.0, nrow = S_n_rows, ncol = S_n_rows) #symetric matrix 20x20
  
  for (i in 1:S_n_rows){
    for (j in 1:ncol(S)) {
      G[i,S[i,j]] <- 1;
      G[S[i,j],i] <- 1;
    }
  }
  print_first_graph_lines(G,X)
  print('Przed uswpolnianiem')
  graph1 <- graph_from_adjacency_matrix(adjmatrix = G, mode='undirected')
  print('LICZBA KOMPONENTOW')
  print(count_components(graph1))
  
  #keep_going = TRUE
  
  print_first_graph_lines(G,X)
  
  n_connected_comp <- count_components(graph1)
  while(n_connected_comp != 1){
    rand_row <- sample(1:nrow(G),1)
    rand_col <- sample(1:nrow(G),1)
    print(rand_row)
    print(rand_col)
    if(G[rand_row,rand_col]==1){
      next
    }
    else{
      G[rand_row,rand_col]=1
      G[rand_col,rand_row]=1
      graph1 <- graph_from_adjacency_matrix(adjmatrix = G, mode='undirected')
      n_connected_comp <- count_components(graph1)
      lines(c(blobs$samples[rand_row,1],blobs$samples[rand_col,1]),c(blobs$samples[rand_row,2],blobs$samples[rand_col,2]), type = 'l', col='red')
      
    }
  }
  
  
  return(G)
}

Laplacian_eigen <- function(G,k){
  D <- apply(G,1, sum)
  print('D')
  print(D)
  D <- diag(D)
  print('d_diagonal')
  print(D)
  
  L <- D - G
  
  print('L')
  print(L)
  
  # E nxk
  #E <- matrix(0.0, nrow = nrow(G), ncol = k)
  #print('E')
  #print(E)
  
  print('eigen_values')
  ev <- eigen(L)$values
  print(ev)
  print('order of values')
  print(order(ev, decreasing = FALSE))
  smallest_lambas_indices <- order(ev, decreasing = FALSE)[2:(k+1)]
  print('smallest lambdas_idices')
  print(smallest_lambas_indices)
  
  print('eigen(L)$vectors')
  print(eigen(L)$vectors)
  
  print('eigen(L)$vectors[,smallest_lambas_indices]')
  print(eigen(L)$vectors[,smallest_lambas_indices])
  
  E <- eigen(L)$vectors[,smallest_lambas_indices]
  
  
  return(E)
}

print_first_graph_lines <- function(G, X){
  for (i in 1:nrow(G)){
    for (j in 1:ncol(G)) {
      if (G[i,j] == 1){
        lines(c(X[i,1],X[j,1]),c(X[i,2],X[j,2]), type = 'l')
      }
    }
  }
}
print_added_line <- function(G,X){
  #to be implemented
}

print_eigenvalues <- function(G){
  evalues <- eigen(G)$values
  print(evalues)
}
