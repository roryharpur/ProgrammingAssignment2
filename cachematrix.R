## This file contains 2 functions required for Programming Assignment 2 for RPROG-035
## The functions creates a special matrix object that caches the inverse matrix 
## The file also contains some code to test the functions



## This function takes a matrix as an argument and creates an internal variable that stores the inverse matrix
## It also creates the methods associated with getting and setting the underlying matrix and it's inverse
## and returns a list of these methods
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function takes an object created by the makeCacheMatrix function as an argument
## It will return the cached inverse of the underlying matrix if it exists in the cache,
## otherwise it will solve for the inverse, save it to cache and return it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}



## Code to test functions

matrix1 <- matrix(rnorm(100), 10, 10)           ## Create Starting Matrix
cacheMatrix <- makeCacheMatrix(matrix1)         ## Create Cache Matrix
cacheSolve(cacheMatrix)                         ## Solve using cacheSolve for the first time
cacheSolve(cacheMatrix)                         ## Solve again using cacheSolve to verify use of cache
solve(matrix1)                                  ## Solve original matrix to verify results are correct
