# Application of K-means algorithm to ZIPCODE data

# Rand Index Calculation example
require(fossil)
cluster1 <- c(2,2,1,3)
cluster2 <- c(3,3,2,1)
rand.index(cluster1, cluster2) # clusters match

# Load the ZIPCODE data
zipcode <- read.table("ZIPCODE.txt", header = F)

# Extract the true digits
Y <- zipcode[ , 1]

# Extract the data points
X <- zipcode[ , -1]

# [ToDo] Try K-means algorithm nRep times with different starting points on ZIPCODE data. Calculate Rand Index at each replication
nRep <- 50

randIndex = rep(0, nRep)

# [ToDo] Report mean Rand Index
for(i in 1:nRep){
  #call for 10 clusters and calculate rand index with the true Y
  randIndex[i] = rand.index(Y, MyKmeans(X = X, K = 10))
}

mean(randIndex)
#mean randIndex is 0.9128026

# [ToDo] Report mean run time for one replication on your machine
require(microbenchmark)
# do this separately to not count the rand.index calculation
microbenchmark(MyKmeans(X=X, K=10), times = nRep) 
#mean run time is 1355.376 milliseconds