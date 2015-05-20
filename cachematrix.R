## Coursera R Programming Assignment 2

## This file contains a pair of functions that store a special matrix object
## and compute, cache, and retrieve its inverse.

## Function makeCacheMatrix creates a special object that stores a matrix
## and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get<- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## Function cacheSolve computes the inverse of the matrix returned by 
## makeCacheMatrix and stores it in the cache. 

## If the inverse has already been calculated, cacheSolve retrieves
## the inverse matrix from the cache instead of calculating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("retrieving cached inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
