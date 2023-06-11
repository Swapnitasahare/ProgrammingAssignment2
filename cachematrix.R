## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m) {
  # Initialize the cache
  cache <- NULL
  
  set <- function(matrix) {
    m <<- matrix
    cache$inv <<- NULL  # Invalidate the cached inverse
  }
  
  get <- function() {
    m
  }
  
  setInverse <- function(inv) {
    cache$inv <<- inv
  }
  
  getInverse <- function() {
    cache$inv
  }
  
  # Create the cache object
  cache <- list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
  
  # Set the initial matrix
  set(m)
  
  # Return the cache object
  cache
}

cacheSolve <- function(cacheMat) {
  # Check if the inverse is already cached
  inv <- cacheMat$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not, compute the inverse
  m <- cacheMat$get()
  inv <- solve(m)
  
  # Cache the inverse
  cacheMat$setInverse(inv)
  
  inv
}
m <- matrix(c(1, 2, 3, 4), nrow = 2)
cacheMat <- makeCacheMatrix(m)
inv <- cacheSolve(cacheMat)
print(inv)
cachedInv <- cacheSolve(cacheMat)
print(cachedInv)



