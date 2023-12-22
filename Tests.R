# Use this file to create tests/debug your functions

# Source the functions
source("FunctionsKmeans.R")

#load required testing package
if (!require(testthat)) install.packages('testthat')

#test mismatches between M,X and K
test_inputmismatches_invalidinputs = test_that('Test for input mismatche or invalid inputs', {
  #mismatch between K and nrow(M)
  expect_error(MyKmeans(X, K-1, M))
  
  #create a fake X that doesn't have same cols as M
  X1 = matrix(c(1,3,4,7,0,1,9,5, 8,6), ncol = 5, nrow = 2, byrow = TRUE) # 2x5
  M1 = matrix(c(2.5, 1, 1.5, 1.5), ncol = 2, nrow = 2, byrow = TRUE) # 2x2
  expect_error(MyKmeans(X1, 2, M1))
  
  #test for K not equal to M
  X1 = matrix(c(1,3,4,7,0,1,9,5, 8,6), ncol = 2, nrow = 5, byrow = TRUE) # 2x5
  M1 = matrix(c(2.5, 1, 1.5, 1.5), ncol = 2, nrow = 2, byrow = TRUE) # 2x2
  expect_error(MyKmeans(X1, 3, M1))
  
  #ask for more clusters rows of M
  X1 = matrix(c(1,3,4,7,0,1,9,5, 8,6), ncol = 2, nrow = 5, byrow = TRUE) # 2x5
  M1 = matrix(c(2.5, 1, 1.5, 1.5, 1,2), ncol = 2, nrow = 3, byrow = TRUE) # 2x2
  expect_error(MyKmeans(X1, 2, M1))
  
  #pass non matrices
  X1 = c(1,3,4,7,0,1,9,5, 8,6)
  M1 = c(1,4,8)
  y = MyKmeans(X1, 3, M1)
  expect_equal(length(X1), length(y))
  expect_equal(3, length(unique(y)))
  
  #ask for more clusters than data points
  X1 = matrix(c(1,1,2,2,3,3,1,1), nrow=4, ncol=2, byrow = TRUE)
  M1 = matrix(c(2.5, 1, 1.5, 1.5), ncol = 2, nrow = 2, byrow = TRUE) # 2x2
  #ask for 4 clusters but we only have 3 unique points in X1 so should fail
  expect_error(MyKmeans(X1, K=4))
  
  #try with bad K values
  expect_error(MyKmeans(X1, K=NA))
  expect_error(MyKmeans(X1, K=-3))
  expect_error(MyKmeans(X1, K=0))
  
  #try with bad NA values
  expect_error(MyKmeans(X1, K=3, numIter = NA))
  expect_error(MyKmeans(X1, K=3, numIter = 0))
  expect_error(MyKmeans(X1, K=3, numIter = -2))
})

#test with simple examples
test_simplecasesforX = test_that('Test with simple cases for X - one and 2 dimensions',{
  skip_if_not_installed("fossil")
  
  x = matrix(c(rep(0,2), rep(10,2)), nrow = 4, ncol = 1, byrow = TRUE)
  m = matrix(c(0,10), nrow=2, ncol=1)
  y = MyKmeans(X=x,K=2, M=m)
  #we expect these to be split into 2 clusters with 2 points each
  clusters = c(1,1,2,2)
  expect_equal(length(y), nrow(x))
  expect_equal(fossil::rand.index(clusters,y), 1)
  
  x = matrix(c(1,2,3,9), nrow = 4, ncol = 1, byrow = TRUE)
  m = matrix(c(2,9), nrow=2, ncol=1)
  y = MyKmeans(X=x,K=2, M=m)
  #we expect these to be split into 2 clusters with 2 points each
  clusters = c(1,1,1,2)
  expect_equal(length(y), nrow(x))
  expect_equal(fossil::rand.index(clusters, y), 1)
  
  x = matrix(c(rep(1,2), rep(2,2)), nrow = 2, ncol = 2, byrow = TRUE)
  m = matrix(c(2,2), nrow=1, ncol=2)
  y = MyKmeans(X=x,K=1, M=m)
  #we expect these to be split into 1 cluster1 with 2 point
  clusters = c(1,1)
  expect_equal(length(y), nrow(x))
  expect_equal(fossil::rand.index(clusters, y), 1)
  
  #try with a diffent set of inputs, 2 dimesional
  x = matrix(c(1,1,2,1,3,2,3,3), nrow = 4, ncol = 2, byrow = TRUE)
  m = matrix(c(2,1, 3,2), nrow=2, ncol=2, byrow = TRUE)
  y = MyKmeans(X=x,K=2, M=m)
  clusters = c(1,1,2,2)
  expect_equal(fossil::rand.index(clusters, y), 1)
  
  #try another one
  x = matrix(c(1,1,3,1,3,2,3,3), nrow = 4, ncol = 2, byrow = TRUE)
  m = matrix(c(1,1,3,3), nrow=2, ncol=2, byrow = TRUE)
  y = MyKmeans(X=x,K=2, M=m)
  clusters = c(1,1,2,2)
  expect_equal(fossil::rand.index(clusters, y), 1)
})

#test with larger examples with random data
test_randomcasesforX = test_that('test with larger examples with random data',{
  skip_if_not_installed("fossil")
  #generate data from 2 univariate normal populations
  data1= rnorm(100, 0, 1)
  data2 = rnorm(50, 50,1)
  x = c(data1, data2)
  y = MyKmeans(X=x, K=2)
  clusters=c(rep(1,100), rep(2,50))
  #we expect 100 in one cluster and 50 in other one
  expect_equal(fossil::rand.index(clusters,y), 1)
})

#test for multiple clusters
test_fixeddatamultipleclusters = test_that('test using multiple clusters', {
  skip_if_not_installed("fossil")
  #generate data that can be assigned to 3 clusters
  X1 = matrix(c(2,10,2,5,8,4,5,8,7,5,6,4,1,2, 4,9), nrow = 8, ncol = 2, byrow = TRUE)
  M1 = matrix(c(2,10,5,8,1,2), nrow = 3, ncol = 2, byrow = TRUE)
  expect_equal(fossil::rand.index(c(1, 3 ,2, 2, 2, 2, 3, 2),
                          MyKmeans(X1, K=3, M1, numIter = 1)), 1 )
  
  #here everything goes into 1 cluster
  X1 = matrix(c(1,2,1,2,1,2,1,2), nrow=4, ncol=2, byrow = TRUE)
  expect_equal(fossil::rand.index(c(1,1,1,1),
                                  MyKmeans(X1, K=1)), 1 )
 })

#test for too many clusters
test_toomanyclusters = test_that('test using a large number of clusters',{
  skip_if_not_installed("fossil")
  #here everything goes into 1 cluster
  X1 = matrix(c(1,2,1,2,1,2,1,2), nrow=4, ncol=2, byrow = TRUE)
  #this should error out as we are asking for too many clusters
  expect_error(MyKmeans(X1, K=2))
  expect_error(MyKmeans(X1, K=200))
  
  #create a dataset with far away points
  X1 = matrix(c(1,2,101,202,301,302,401,402), nrow=4, ncol=2, byrow = TRUE)
  #this should work
  expect_equal(fossil::rand.index(c(1,2,3,4), MyKmeans(X1, K=4)), 1)
  #will fail as now we ask for too many clusters
  expect_error(MyKmeans(X1, K=5))
})

#test for higher dimensions
test_higherdimensions = test_that('test using higher number of dimensions', {
  skip_if_not_installed("fossil")
  #test with 3 dimensions
  X1 = matrix(c(1,1,2,2,2,1,6,4,6,7,3,8), nrow=4, ncol=3, byrow = TRUE)
  expect_equal(fossil::rand.index(c(2,2,1,1), MyKmeans(X1, K=2)), 1)
  expect_equal(fossil::rand.index(c(2,4,3,1), MyKmeans(X1, K=4)), 1)
})

#test for iris dataset
test_irisdataset = test_that('test using iris dataset', {
  skip_if_not_installed("fossil")
  #get the predictors from iris data
  X = iris[, c(1,2,3,4)]
  #compare with what kmeans gives us
  expect_equal(fossil::rand.index(kmeans(X, 3, iter.max = 100, algorithm = "Lloyd")$cluster, 
                          MyKmeans(X, K=3)), 1, tolerance=3e-1)
})