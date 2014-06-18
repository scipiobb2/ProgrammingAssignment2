## The functions below are meant to cache the potentially expensive calculation of matrix inverse.
## If the inverse (solve) command is called on a matrix that was already inversed, the cached data will 
## be used instead of a new calculation.
## For that to work, the matrix must be initialized using the "makeCacheMatrix" function.

## This function is creating a sort of a "class" that stores the matrix itself,
## and also the inverse of the matrix.
## The function provides a get and set functions for the matrix and the inverse matrix.

## The function takes a matrix as input.
## The function returns a list of "methods" (get, set, getinverse, setinverse).

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


## This function uses the "CacheMatrix class" defined in the "makeCacheMatrix" function,
## to calculate and store the inverse of the matrix stored inside the "class".
## If the computation was previously made,
## this function simply returns the inverse value stored in the "CacheMatrix class"

## The function takes an "object" previously defined by the "makeCacheMatrix" function.
## (It actually takes a list of "methods" of that "object" and assumes they exist)
## The function returns the inverse of the matrix initialized be the "makeCacheMatrix" function.

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
