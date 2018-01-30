## First function creates an environment with variable inv and x, and 
## list of functions to manipulate set and retrieve the values of x and inv 
## from the makeCacheMatrix environment


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      #set replaces the value of the matrix itself and erases any previous inverse matrices
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      #setinv utilizes superasignement to update inv variable held in the CacheMatrix environment
      setinv <- function(inverse_matrix) {inv <<- inverse_matrix}
      getinv <- function() inv
      list(set=set, get=get,
           setinv = setinv,
           getinv = getinv)
}

## The following function first checks whether cachematrix hold the inverse matrix in its 
## environment. If it does it returns the inverse matrix from the memory. If it does not, then
## the function computes the inverse and updates the inv variable in the cachematrix environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      ## if the cachematrix does not hold the value of inverse then 
      ## the function computes it and stores it within the cache matrix
      data <- x$get()
      inv <- solve(data,...)
      x$setinv(inv)
      inv
}
