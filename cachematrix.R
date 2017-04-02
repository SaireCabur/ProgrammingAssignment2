################################################################################
## These two functions are designed to compute the inverse of a square matrix
## and cache the result for later use. 
## The actual computation is thereby done in the "cacheSolve" function, the 
## result is then stored in the environment of the "makeCacheMatrix" function.
##
## The input values are as follows:
## "makeCacheMatrix": a square invertible matrix
## "cacheSolve": an object of type "myVector"
## 
################################################################################

## "inv" is the variable used to store the inverted matrix
## 
## Furthermore, "makeCachematrix" contains four functions to set and get the 
## value of the input matrix and to set and get the value of the inverted matrix
## The output is a list containing these functions, which were named so that
## they can be called by name via the $ sign
## 
## Note that the first "set" function is only needed if the input value should
## be changed after "makeCacheMatrix" has already been executed. If a value is
## stored in "inv", it is then also set to NULL. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## "cacheSolve" uses the functions defined in "makeCacheMatrix" to get the value
## of the original input matrix and compute the inverse. The result is then
## stored in the "inv" variable by using the "setinv" function and returned to
## the console. 
## 
## However, before the computation takes place, the function checks if the
## inverse has already been computed. If so, the cached value is retrieved and
## returned without computing it again. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
