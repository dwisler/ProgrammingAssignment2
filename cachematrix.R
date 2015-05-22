## Coursera - R Programming - Assignment 2
##
## A pair of functions that provide a matrix with a cache of it's inverse
## in order to avoid re-calculation of the inverse.
##
## makeCacheMatrix - creates a "special" matrix object that caches it's inverse.
## Usage:      makeCacheMatrix( x=matrix() )
## Arguments:  x: an invertible matrix to be cached, default is empty matrix.
## Returns:    a list object that contains the following methods:
##               $get()       returns the original matrix
##               $solve(...)  returns the inverse of the matrix, passing
##                            any arguments to the R-base solve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function()   return( x )
    invert <- function(...) {
        if ( is.null(inv) ) {
	    message( "computing inverse and caching it" )
	    inv <<- solve(x,...)
	} else {
	    message( "returning inverted matrix from cache" )
	}
	inv
    }
    list( get=get, solve=invert )
}


## cacheSolve - returns the inverse of a "special" matrix returned
## from "makeCacheMatrix()".  That inverse matrix will be cached so that
## it only gets calculated on the first call to this function, and thereafter
## the cached solution will be returned without recalculating.
## Usage:      cacheSolve( x, ... )
## Arguments:  x:   a "special" matrix (actually a list object) returned by
##                  the makeCacheMatrix() function.
##             ...: other arguments are passed to the R-base solve()
## Returns:    the inverse of the matrix originally specified to
##             makeCacheMatrix()

cacheSolve <- function(x, ...) {
	# give a friendly error if the argument doesn't
	# seem to have come from makeCacheMatrix
	if ( !is.list(x) || !exists("solve",where=x) ) {
	    stop( "x argument was not returned by makeCacheMatrix" )
	}
	#  return the possibly cached inverse of the original matrix
	x$solve(...)
}
