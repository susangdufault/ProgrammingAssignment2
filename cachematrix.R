## This code contains two functions that together create a matrix that can
## cache its inverse and retrieve the cached data rather than recalculate
## the inverse for a matrix for which this has already been calculated.
## 
## makeCacheMatrix is a function that creates a list of 4 elements that are
## functions to set the value of the matrix (set), get the value of the
## matrix (get), set the value of the inverse of the matrix (setinverse), 
## and get the value of the inverse of the matrix (getinverse).

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix. It first checks to see
## if the inverse has already been calculated and cached by makeCacheMatrix
## with the getinverse function by setting its value to m. If yes, (i.e., m
## is not null) then it returns the cached value. Otherwise it gets the value
## of the matrix and calculates and returns its inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
