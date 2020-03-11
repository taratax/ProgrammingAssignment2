## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#' Creates matrix which caches it's own inversion
#' @param x is a matrix
#' Returns a list with four functions inside.
#' One function for setting matrix, one for getting matrix.
#' One setting inverse and one for getting inverse.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL

 #function below sets value of matrix, clears caache as we do not need it because we are setting new matrix.
 set <- function(y) {
   x <<- y
   m <<- NULL
 }

get <- function() x # This function just returns the matrix

setInverse <- function(inverse) m <<- setInverse # stores inverse in m

getInverse <- function() m

# returns a list with 4 elements which are all above functions
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
#' This function returns inverse of matrix returned from makeCacheMatrix function.
#' Inverse is cached so that it returns data from cache if inverse was calculated.
#' @param x result from makeCacheMatrix
#' @return inverse of matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) {
          message("getting cached data")
          return(m)
        }

        data <- x$get()  # retrieve matrix
        m <- solve(data) # get inverse of matrix
        x$setInverse(m)  # store the result in cache
        m                # returns iversion of matrix
}
