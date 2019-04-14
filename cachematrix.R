## Matrix inversion is a costly computation so the ability to cache the inverse of a matrix
##for the use in future calculations could save a lot of time. 

## the first function 'makeCacheMatrix' creates a special matrix by creating a list that 
## contains a function to 
## 1.Set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse
## This creates a matrix that can cache its own inverse for future computations

makeCacheMatrix <- function(x = matrix()) {
  iv<-NULL
  set<- function(y) {
    x<<-y
    iv<<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) iv<<-inverse
  getinverse<- function() iv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the special matrix from the makeCacheMatrix
## function. Once the inverse has been calculated a first time it will retrieve the inverse
## from the cache if the matrix has not changed when the calculation is repeated.

cacheSolve <- function(x, ...) {
  iv<- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data<- x$get()
  iv<- solve(data, ...)
  x$setinverse(iv)
  iv
}
