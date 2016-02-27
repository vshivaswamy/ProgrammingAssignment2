# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
# set and get the value of the matrix
# set and get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  
  get <- function() x
  
  set <- function(y) {                      
    x <<- y
    # reset inverse of the matrix if the matrix was changed.
    inversedMatrix <<- NULL              
  }
  
  getInversed <- function() inversedMatrix
  
  setInversed <- function(m){
    inversedMatrix <<- m
  }
  
  list(set = set, get = get,
       setInversed = setInversed,
       getInversed = getInversed)
}


# This function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed and cached. 
# If so, it gets the result from cache and skips the computation. 
# If not, it inverses, sets the value in the cache and returns
cacheSolve <- function(x, ...) {
  inversedMatrix <- x$getInversed()
  #if the inverse exists, just return it.
  if(!is.null(inversedMatrix)) {                 
    message("getting cached data - Inverse of the matrix")
    return(inversedMatrix)
  }
  #if the inverse matrix doesn't exist, then inverse using solve(), cache and return inverse matrix
  data <- x$get()                               
  inversedMatrix <- solve(data, ...)
  x$setInversed(inversedMatrix)
  
  ## Return a matrix that is the inverse of 'x'
  inversedMatrix
}


## To run this
#create a square matrix
mat <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
#call cacheSolve() like below
#cacheSolve(makeCacheMatrix(mat))
cache_mat <- makeCacheMatrix(mat)
#call cacheSolve() like below
#cacheSolve(cache_mat)

