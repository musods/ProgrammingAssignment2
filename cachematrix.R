## Put comments here that give an overall description of what your
## functions do

## Here we have 2 functions  makeCacheMatrix and cacheSolve

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL       # Initialize the inverse as NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function(){x}   # A function to get the matrix x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, setinv = setinv, getinv =getinv)
      
}


## Write a short comment describing this function
## This is to get the cache data
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){             #Cheking if the inverse is null
            message("getting cached data")
            return(inv)        # Return the value of the inverse
}
      data <- x$get()
      inv <- solve(data, ...)   # Calculates the value of the inverse
      x$setinv(inv)
      inv

      
}
