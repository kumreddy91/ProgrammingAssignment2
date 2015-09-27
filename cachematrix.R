## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix returns a list to set, get, setinverse and getinverse of a given matrix
#for example, makeCacheMatrix$setinverse sets inverse value to inv(cache)

## Write a short comment describing this function

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
#it computes inverse. Otherwise returns cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv) #sending(or)setting computer inv to cache
    inv
}

#Desclaimer: I just modified prof. Peng's code of 'vector caching'.
#I replaced mean,m and mean() with inverse, inv and solve(). Thanks to previous students
#This assignment is very informative in learning about caching.

