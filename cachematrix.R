## The below function cache the time consuming calculations and thus preventing R from recalculating the same thinf again and again.

## The function generates a special matrix whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
             inv<- NULL
             set<-function(y)   {
                   x <<- y
                  inv <<- NULL
              }
              get <- function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, 
             setinv=setinv, 
             getinv=getinv)
}


## This function returns inverse of the special matrix created by the above function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
}



