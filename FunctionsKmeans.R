# Function that implements K-means algorithm. The default number of maximal iterations is 100.
# X - n by p matrix containing n data points to cluster
# K - integer specifying number of clusters
# M - (optional) K by p matrix of cluster centers
# numIter - number of maximal iterations for the algorithm, the default value is 100
MyKmeans <- function(X, K, M = NULL, numIter = 100){
 
  #convert X to matrix
  X = as.matrix(X)
  
  #numIter cannot be <= 0 or NA
  if(numIter <= 0 || is.na(numIter)){
    stop('numIter cannot be <= 0 or NA')
  }
  
  #K cannot be negative or NA
  if(K <= 0 || is.na(K)){
    stop('K cannot be <= 0 or NA')
  }
  
  # Check whether M is NULL or not. If NULL, initialize based on K randomly selected points from X. 
  # If not NULL, check for compatibility with X dimensions and K.
  if(is.null(M)){
    # randomly select K values from n 
    M = X[sample(nrow(X), K, replace = FALSE), , drop = F]
    #check if we end up with duplicated points
    if(any(duplicated(M))){
      uniquex = unique(X)
      urows = nrow(uniquex)
      #resample with unique x after checking we have at least X unique values
      if(K > urows){
        stop('Number of cluster centers is greater than number of unique data points')
      }
      M = uniquex[sample(urows, K, replace = FALSE), , drop = F]
    }
  }
  else{
    #convert M to matrix
    M = as.matrix(M)
    if(dim(M)[1] != K){
      stop(paste('M must have ', K, 'rows'))
    }
    if(dim(M)[2] != dim(X)[2]){
      stop('M and X must have the same number of columns')
    }
    #check duplicated centroids
    if(any(duplicated(M))){
      stop('Matrix M has duplicate cluster centers')
    }
    #check if K is larger than number of unique data points 
    ux = nrow(unique(X))
    if(K > ux){
      stop('Number of cluster centers is greater than number of unique data points')
    }
  }
  
  # Implement K-means algorithm. 
  # It should stop when either 
  # (i) the centroids don't change from one iteration to the next (exactly the same), or
  # (ii) the maximal number of iterations was reached, or
  # (iii) one of the clusters has disappeared after one of the iterations (in which case the error message is returned)
  
  Y = rep(0, nrow(X))

  for(i in 1:numIter){
    #store previous Y
    preY = Y
    #compute distances between each data point and centroid
    # we ignore the terms that do not need to be minimized which is the sum of squares of the observations after opening the parenthesis in the norm
    Msqr = M^2
    MsqrRowSum = .Internal(rowSums(Msqr, dim(Msqr)[1], dim(Msqr)[2], FALSE))
    innerexpr = sweep(-2*tcrossprod(X,M),2, MsqrRowSum, "+")
    Y = apply(innerexpr, 1, which.min)

    #check if the centroids changed
    if(identical(preY, Y)){
      #stop
      break
    }
    #check if a cluster has disappeared
    if(length(unique(Y)) < K){
      stop('Number of unique clusters ', length(unique(Y)), ' is less than K. Please check the M matrix.' )
    }
    
    #update cluster centers
    for(j in 1:K){
      Clusterj = X[Y==j, , drop=FALSE]
      #calling using .internal to avoid the is.dataframe check in colMeans
      M[j, ] = .Internal(colMeans(Clusterj, 
                         dim(Clusterj)[1], dim(Clusterj)[2], FALSE))
    }
  }
  
  # Return the vector of assignments Y
  return(Y)
}