## The following functions calculate and cache the inverse of a matrix in order to 
## reduce the computational effort needed if the calculation has to be repeated several times.

## This function creates a special vector (list) containing functions designed to:

## 1- set the value of the matrix passed in the argument of the function
## 2- get the value of the matrix passed in the argument of the function
## 3- set the value of the inverse of the matrix
## 4- get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) {
                 x <<- y
                 m <<- NULL
         }
         get <- function() x
         setinv <- function(inv) m <<- inv
         getinv <- function() m
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}


## The following function calculates the inverse of the matrix passed to the the above function. It
## will check first if the inverse has already been calculated. If that is the case, it will obtain
## the inverse of the matrix from the cache and skip the computation. Otherwise, it will calculate the 
## inverse of the matrix and set the value of the inverse in the cache by means of the "setinv" 
## function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getinv()
         if(!is.null(m)) {
                 message("getting cached data")
                 return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$setinv(m)
         m
}
