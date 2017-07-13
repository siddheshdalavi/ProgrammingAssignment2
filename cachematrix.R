## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function creates a matrix and store its inverse value in cache

makeCacheMatrix <- function(x = matrix()) {
  a<-NULL
  set<-function(y){
    x<<-y
    a<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) a<<-inverse
  getinverse<-function() a
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## function cacheSolve retuns the inverse of matrix that is calculated from function makeCacheMatrix 

cacheSolve <- function(x, ...) {
        a<-x$getinverse()
        if(!is.null(a)){
          print("fetching inverse matrix storeed in chache")
          return(a)
        }
        b<-x$get()
        a<-solve(b,...)
        x$setinverse(a)
        a
        ## Return a matrix that is the inverse of 'x'
}
