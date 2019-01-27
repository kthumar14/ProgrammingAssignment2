## function sets value of matrix from list and then gets value of matrix and sets inverse ofmatrix or gets inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  sInv <- function(inverse) m<<- inverse
  gInv <- function() m
  list(set=set, get=get, sInv=sInv, gInv=gInv)
}


## calculates inverse of what's returned from last function if the inverse hasn't been calculated yet

cacheSolve <- function(x, ...) {
  m <- x$gInv()
  if(!is.null(m)) {
    message("getting cached data.")
    return(m)
  }
  
  data <- x$get
  m <- solve(data,...)
  x$sInv(m)
  m
}

