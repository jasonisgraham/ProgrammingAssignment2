## create a structure that accepts a matrix
## and returns an object with a set of functions that allow accessing and setting that matrix
## and accessing and setting the identity (solve) of that matrix
## calling set(m) on the object resets the cached identity if it was defined
makeCacheMatrix <- function(matrix = matrix()) {
  solveVal <- NULL

  set <- function(m) {
    matrix <<- m
    solveVal <<- NULL
  }

  get <- function() { matrix }

  setSolve <- function(s) { solveVal <<- s}

  getSolve <- function() { solveVal }

  list(set = set,
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## Cache output of solve(x) given some x.
## If x isn't in cache, compute it and cache it
## if it is in cache, return it
cacheSolve <- function(cachedMatrix, ...) {
  cachedSolveVal <- cachedMatrix$getSolve()

  if (!is.null(cachedSolveVal)) {
    message("getting cached data")
    return (cachedSolveVal)
  }

  matrix <- cachedMatrix$get()
  solveVal <- solve(matrix, ...)
  cachedMatrix$setSolve(solveVal)
  solveVal
}

# > m<-matrix(sample(-100:100, 25), nrow=5, ncol=5)
# > cacheMatrix = makeCacheMatrix(m)
# > cacheMatrix$get()
# [,1] [,2] [,3] [,4] [,5]
# [1,]  -85   84  -12  -31  -23
# [2,]   82   95  -44   12   18
# [3,]  -35   50  -98  -13  -32
# [4,]  -89   -8   -6  -14  -51
# [5,]    4  -53    0  -76   96
# > cacheSolve(cacheMatrix)
# [,1]         [,2]         [,3]         [,4]         [,5]
# [1,] -0.0196411863  0.033528230 -0.015392042  0.044812034  0.007683468
# [2,]  0.0085580803  0.002466298 -0.001833083 -0.005261983 -0.001818514
# [3,] -0.0004993401  0.011381750 -0.016321662  0.017452990  0.001577635
# [4,]  0.0257452139 -0.056463955  0.027611789 -0.088413973 -0.021010878
# [5,]  0.0259247840 -0.044736038  0.021488653 -0.074766617 -0.007541061
# > cacheSolve(cacheMatrix)
# getting cached data
# [,1]         [,2]         [,3]         [,4]         [,5]
# [1,] -0.0196411863  0.033528230 -0.015392042  0.044812034  0.007683468
# [2,]  0.0085580803  0.002466298 -0.001833083 -0.005261983 -0.001818514
# [3,] -0.0004993401  0.011381750 -0.016321662  0.017452990  0.001577635
# [4,]  0.0257452139 -0.056463955  0.027611789 -0.088413973 -0.021010878
# [5,]  0.0259247840 -0.044736038  0.021488653 -0.074766617 -0.007541061
# > newMatrix <-matrix(sample(-100:100, 25), nrow=5, ncol=5)
# > cacheMatrix$set(newMatrix)
# > cacheSolve(cacheMatrix)
# [,1]         [,2]          [,3]          [,4]         [,5]
# [1,] -0.005372606 -0.006470239 -3.334619e-03  0.0121550510 -0.008017219
# [2,]  0.001851096  0.008080554 -1.916241e-02 -0.0293973115  0.018094671
# [3,] -0.000208658  0.002565236  3.257966e-05  0.0006874195 -0.011234573
# [4,] -0.006181438  0.007372734  1.459759e-02  0.0171622243 -0.009957472
# [5,] -0.007588916  0.003368941 -2.037936e-02 -0.0302916901  0.013251117
# > cacheSolve(cacheMatrix)
# getting cached data
# [,1]         [,2]          [,3]          [,4]         [,5]
# [1,] -0.005372606 -0.006470239 -3.334619e-03  0.0121550510 -0.008017219
# [2,]  0.001851096  0.008080554 -1.916241e-02 -0.0293973115  0.018094671
# [3,] -0.000208658  0.002565236  3.257966e-05  0.0006874195 -0.011234573
# [4,] -0.006181438  0.007372734  1.459759e-02  0.0171622243 -0.009957472
# [5,] -0.007588916  0.003368941 -2.037936e-02 -0.0302916901  0.013251117
