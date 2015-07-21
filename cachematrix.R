## R Programming Assignment 2
## Name: Trey Sinyard
## Date: 7/21/2015

## This file consists of 2 functions that work to solve and cache the inverse of a matrix

## The following function, makeCacheMatrix, takes the argument of a matrix and then...
## (1) sets the variable, i as NULL
## (2) creates set() to change the vector stored in the main function
## (3) creates get() to return the matrix provided as the argument of makeCacheMatrix()
## (4) creates setinverse() to establish the variable i as equal to the variable inverse
## (5) creates getinverse() to print the variable i
## (6) establishes all the functions created as part of a list

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## The following function, cacheSolve takes the argument x and then...
## (1) sets the variable i as the function x$getinverse()
## (2) evaluates i as "not NULL" and returns the value of i if not NULL
## (3) sets "data" as the value of the original matrix using x$get()
## (4) sets i as equal to the inverse of the original matrix
## (5) caches the inverse matrix as i
## (6) returns the inverse matrix i

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        return(i)
}
