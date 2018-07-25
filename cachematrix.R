

## The function below is able to cache potentially time-consuming
#   computations of matrix inversion. 
## To avoid computing repeatedly it is better to cache the inverse
##  of a matrix.

## makeCacheMatrix: This function creates a special matrix 
##object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special matrix
##returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
