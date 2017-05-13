## The intention of these functions is to outline the role of lexical
## scoping in the context of cached data. These functions allow for
## computations to be stored in the memory of an object. This allows
## us to avoid repetive calculations, by instead referencing the
## value of the previous solution in the event that it was already
## determined.

## This function creates a special object that closely resembles a 
## matrix. It allows for the inverse of a matrix to be cached in the
## memory of the object - so that it does not have to be calculated
## every time it is retrieved.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function determines the inverse of the matrix component of the 
## 'makeCache' object that is passed to it. It checks to see if the inverse
## of the matrix has already been computed and cached. If it has been, then
## it will simply retrieve this cached value, foregoing any computations. If
## the inverse of the matrix has not been cached, then the function will compute
## the value, and cache it into the memory of the 'cacheMatrix' object.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
                message ("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
