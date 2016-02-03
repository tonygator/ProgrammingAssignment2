## Coursera, Learning R
## Assignment 2 
## Tony Giordano
## 2016 FEB 02

## a function that allows us to store and retrieve the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
      
      # initialize properties
      inv <- NULL
      
      # getters/setters
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) {
            inv <<- inverse
      }
      getinverse <- function() inv

      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## use caching to avoid expensive computations. solving the matrix (finding its inverse)
## is only done once and the results are cached for use later
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      # call getter to retrieve the value of the inverse (or null if value has not been cached)
      inv <- x$getinverse()
      
      # if inverse was cached, then return it and we're done
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }

      # inverse was not in cache so we need to create it
      # first, we need the matrix data
      data <- x$get()

      # next, we compute the inverse of the matrix
      inv <- solve(data, ...)
      
      # now lets avoid having to compute the inverse again by saving our result in cache
      x$setinverse(inv)
      
      # return the inverse to the caller
      inv
}
