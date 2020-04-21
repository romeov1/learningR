## R programming, assignment2. Objective is to write 2 functions
## that generate a matrix object,calculate the inverse, cache it and retrieve it

## The first function, makeCashMatrix, generates a matrix object containing 4 functions.
## the first function, set-line13,sets the values of the matrix
## the second function, get-line17, get the values in the set matrix
## the third function, setinverse-line18, set the inverse values for the matrix
## the fourth function, getinverse-line19, get the values of the inversed matrix
## finally, data are stored in a list with each function as an element

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

## the second function, cacheSolve, tries to retrieve the values in makeCacheMatrix and eventually calculates the inverse.
## First the function tries to retrieve the cached values by looking into getinverse from makeCacheMatrix-line32.
## and checking if the value is NULL-line33. if the value is not NULL, then it will use the cached inv values-line 35.
## if the expression is FALSE, the value is NULL, thus the function will get new values-line 37,
## calculate the inverse of the matrix values-line38, 
## store the new values-line39
## and print the inversed matrix-line40

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
      if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
      }
  newinv <- x$get()
  inv <- solve(newinv, ...)
  x$setinverse(inv)
  inv
}
