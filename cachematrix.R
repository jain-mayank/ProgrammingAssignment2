## makeCacheMatrix function creates a special matrix object which is used by
## cacheSolve function to retrieve inverse from cache or to compute it and save it in cache


## makeCacheMatrix function creates a special matrix object with 
## setters and getters for input matrix x and its inverted matrix inverseCacheMat

makeCacheMatrix <- function(x = matrix()) {
      
      # Initialize inverted matrix
      i <- NULL
      
      # setters for input matrix and its inversion matrix 
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      # getters for input matrix and its inversion matrix
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      
      # returned special matrix object
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve function takes special matrix object from makeCacheMatrix as input
## and checks if inverse is already computed and returns it from cache, if not
## matrix inverse is computed, saves it in cache, and returns the computed result
## assuming an invertible matrix is always provided 

cacheSolve <- function(x, ...) {
      
      # get inverted matrix from cache
      m <- x$getinverse()
      
      # Return inverted matrix from cache if available and skip computing inverse
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      # compute inverse of matrix if it is not available in cache
      data <- x$get()
      i <- solve(data, ...)
      
      # save the computed inverse in cache and return the inverse matrix
      x$setinverse(i)
      return(i)
}
