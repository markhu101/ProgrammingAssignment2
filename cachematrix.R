## Caching the Inverse of a Matrix
## -------------------------------
## This module provides two methods which caches the computed
## inverse matrix so that following queries on the same data will 
## return the cached value instead of computing it from scratch again.

## makeCacheMatrix()
## Returns a special matrix that contains a list of functions:
## 1. set( y ) - initialize with an input matrix y
## 2. get() - returns matrix defined by above set() function.
## 3. setInverse( newInverse ) - sets an inverse matrix
## 4. getInverse() - returns the inverse matrix.
makeCacheMatrix <- function( x = matrix() ) 
{
        inverse <- NULL
        set <- function( y )
        {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function( newInverse ) inverse <<- newInverse
        getInverse <- function() inverse
        list( set = set, get = get, setInverse = setInverse, getInverse = getInverse )
}


## cacheSolve( x, ... )
## Returns the inverse of a matrix. It also performs caching if needed:
## 1. Has the inverse been cached?
## 2. If so, then return the cached inverse matrix - DONE.
## 3. If not, then compute the inverse.
## 4. Cache the inverse.
## 5. Return the inverse.
## NOTE: This function assumes the matrix supplied is always invertible.
cacheSolve <- function( x, ... ) 
{
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if ( !is.null( inverse ) )
        {
                message( "getting cached data" )
                return( inverse )
        }
        data <- x$get()
        inverse <- solve( data )
        x$setInverse( inverse )
        inverse
}

##  Usage Example:
## > source("ProgrammingAssignment2/cacheMatrix.R")
## > reg_matrix = matrix( 1:4, 2, 2)
## > reg_matrix
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cache_matrix = makeCacheMatrix( reg_matrix )
## > cacheSolve( cache_matrix )     ## nothing cached, so performed solve
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve( cache_matrix )     ## cached, so returned cached value
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5