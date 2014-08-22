## This pair of functions cache the inverse of a matrix.
## Assume that the matrix supplied is always invertible.
## The first function's input should be a matrix.
## Please find the below example how to use these functions:
## A <- 'define an invertable matrix'
## inv <- makeCacheMatrix(A)
## cacheSolve(inv)



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
         x <<- y
         i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
         message("getting cached data")
         return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}

