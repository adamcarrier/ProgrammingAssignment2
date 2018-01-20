## cachematrix.R
##
## This is an R stub file containing functions that allow you to store/cache a matrix and
## its inverse in a list variable. The cacheSolve function provides additional efficiency
## by computing and storing the inverse of your matrix for you via solve. If the inverse
## is already computed (i.e., stored via setinverse), computation is skipped.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## It does that by returning a list of named elements that are get and set functions for the
## stored matrix object and its cached inverse.
makeCacheMatrix <- function( x = matrix() ) {
        matrixinverse <- NULL
        
        set <- function( y ) {
                x <<- y
                matrixinverse <<- NULL ## changing the matrix invalidates the cached inverse 
        }
        
        get <- function() x
        
        setinverse <- function( mi ) matrixinverse <<- mi
        
        getinverse <- function() matrixinverse
        
        list( set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse )
}

## The cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function( x, ... ) {
        inv <- x$getinverse()
        
        if( !is.null( inv ) ) { ## if the inverse is already stored/computed...
                return( inv ) ## ...then return it
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv ## return the inverted matrix
}
