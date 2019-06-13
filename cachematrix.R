## Inverts a matrix x and stores the result in $getinverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## Returns the inverse of matrix x by using the x$getinverse function defined in makeCacheMatrix

cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
