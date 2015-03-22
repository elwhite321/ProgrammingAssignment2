## These funtions will take a matrix and cache the inverse value so it does not
## have to be constanty computed. They will use the cache value when it is 
## avaliable instead of recomputing the inverse. 

## This function creates a list of functions set up to store the value of the
## inverse of a matrix (inv). It initially sets the value (inv) as NULL. Takes a 
## matrix as its imput.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv<<- NULL
  }
  get <-function() x
  setinv<- function(inverse) inv <<- inverse
  getinv<- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function searches the global and child environments for the 
## inverse matrix value. If the value is not NULL (set by makeCacheMatrix), it simply returns it
##If the value is not in the environments, the function solves for the 
##inverse of the matrix and returns its values.It takes the output of the 
##makeCacheMatrix function as its input

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

