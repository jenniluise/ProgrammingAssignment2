## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create the Matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                              
  set <- function(y) {                    
    x <<- y 
    ## search all the way up, assign Null to inv, if inv not exist, create inv and set to Null
    inv <<- NULL                        
    
  }
  ## define the get fucntion - returns value of the matrix argument
  get <- function() x                     
  ## assigns value of inv in parent environment
  setinverse <- function(inverse) inv <<- inverse 
  ## gets the value of inv where called
  getinverse <- function() inv                     
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)  

  
  
  
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## it first checks to see if the inverse has already been calculated. 
        ## If so, it gets the mean from the cache and skips the computation. 
        ## Otherwise, it calculates the inverse and sets the value 
        ## in the cache via the solve function.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
  
  
}
