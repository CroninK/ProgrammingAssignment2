## This function will generate a list of functions to be called by the cacheSolve function and
## will also cache the value of an inverted matrix for future retrieval.  

makeCacheMatrix <- function(x) {
  m <- NULL                                      ## resets m
  d <-dim(x)                                     ## collects dimensions of input
  error<-"Error: Matrix must be a square and non-singluar"
  if(d[1]!=sqrt(length(x))){                     ## tests if input is non-square matrix
    print(error)
    return(error)
  }
  else if(det(x)==0){                            ## tests if input is a singular matrix
    print(error)
    return(error)
  }
  get <- function() x                            ## returns value of x
  setinverse <- function(inverse) m <<- inverse  ## sets global m to value of solve(m)
  getinverse <- function() m                     ## returns value of m
  list(get = get,                                ## creates "matrix" for evaluation
       setinverse = setinverse,
       getinverse = getinverse)
}

##  This function will retrieve a list of functions and the matrix passed to them via the 
## function makeCacheMatrix and return its inverse.  If inverse was already solved then it will
## return the cached value.

cacheSolve <- function(x) {
  if(is.character(x)){return(x)}   ## will return error message from makeCacheMatrix if that fxn is passed here
  m <- x$getinverse()              ## collects current value of x$getinverse() from list for testing
  if(!is.null(m)) {                ## if a value has not yet been cached this is NULL
    message("getting cached data")
    return(m)                      ## returns inverse matrix if it exists
  }
  data <- x$get()                  ## gets the original input from list
  m <- solve(data)                 ## solves for inverse of original input
  x$setinverse(m)                  ## sends the inverse matrix to a cached location
  m                                ## returns the inverse matrix
}
