
## Functions sets/gets matrix and its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  ##sets new matrix
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  
  ##gets current matrix
  get <- function() x
  
  ## sets inverse matrix
  setinverse <- function(data) im <<- data
  
  ## gets iverse matrix
  getinverse <- function() im
  
  ## returns list of methods
  list(set=set,
       get=get,  
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  ##get inverse matrix
  m <- x$getinverse()
  ## if data is cached
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }  
  ## inverse matrix and save
  m <- solve(x$get())  
  x$setinverse(m)
  m
}

