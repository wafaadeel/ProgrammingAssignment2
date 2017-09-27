## Matrix inversion and caching results

## Write a short comment describing this function

CacheMat <- function(x = matrix()) {
  X<- NULL
  set <- function(y){
    x<<-y
    X<<-NULL
  }
  get <- function()x
  setX <- function(XX) X<<-XX
  getX<- function() X
  list(set=set, get=get, setX=setX, getX=getX)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  X <- x$getX()
  if(!isnull(X)){
    message("getting cached data")
    return (X)
  }
  data <- x$get()
  XX <- solve(data, ...)
  x$setX(XX)
  XX
  
}