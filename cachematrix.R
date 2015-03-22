## Inverse (inv) matrix calculation
## Matrix inversion is usually a costly computation and there may be some benefit to ##caching the inverse of a matrix rather than computing it repeatedly
## functions do
## set the matrix
## get the matrix
## set the inv
## get the inv

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## Calculates the inverse of the matrix created with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


