makeCacheMatrix <- function(x = matrix()) {
 m<-NULL
 set <- function(y){
   x<<-y
   m<<-null
 }
 get<- function() x
 setInverse <- function(inv) m<<-inv
 getInverse <- function() m
 list(set=set, get=get, setInverse=setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message ("getting cached data")
    return (m)
  }
  data<-x$get()
  m<-solve(data)
  x$setInverse(m)
  m
}
