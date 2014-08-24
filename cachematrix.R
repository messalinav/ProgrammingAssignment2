## makeCacheMatrix
## cacheSolve

## Takes in a matrix of any size, initially stores an empty value "m"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve will check "m" to see if Null, if not null, returns "m"
## if Null, it will compute the inverse of the input

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get() 
  m<-solve(matrix, ...)
  x$setinv(m)
  m
}
