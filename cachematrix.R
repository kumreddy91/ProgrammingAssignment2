
#makeCacheMatrix(marix1) returns a list to set, get, setinverse and getinverse of a given matrix
#for example, makeCacheMatrix$setinverse sets inverse value to inv, a cache variable.
#cacheSove(m) returns inverse of matrix m, returns either cache value(if not NULL) or computed value

#

## This function returns a list,
#1. setting matrix 2.getting matrix 3. setting inverse into cache
#4.getting cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Initialized to Null
  set <- function(y) {
    x <<- y # caching values to x
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}





#This function checks for inverse in cache. If cache value "inv" is NULL then
#it computes and returs inverse. Otherwise returns cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv) #sending(or)setting computed inv to cache
    inv
}

#Desclaimer: I just modified prof. Peng's code of 'vector caching'.
#I replaced mean,m and mean() with inverse, inv and solve(). Thanks to previous students.
#This assignment is very informative in learning about caching.

