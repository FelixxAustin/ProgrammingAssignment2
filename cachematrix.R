
# This function is able to get and store a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
        
}


## This function checks if the inverse has been computed and stored in the cache. It reutrns the chached inverse if the inverse is chached or it computs the inverse using solve() 
## if its not then caches this inverse using x$setInverse(i)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  if (!is.null(i)) {
    message("Getting cached inverse")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
