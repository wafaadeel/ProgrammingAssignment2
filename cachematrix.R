## Matrix inversion and caching results

## This function is to set/get matrix and inverse

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


## This function is to calculate or fetch from cache inverse

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