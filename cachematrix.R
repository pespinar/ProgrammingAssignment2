## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix stores a list of functions. The set function will store a 
## matrix. The get function will retrieve the stored matrix. The setinverse
## function will set the inverse of a matrix and the getinverse will retrieve
## the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##cacheSolve is a function that looks for the stored inverse of a given matrix. If
## it has already been computed, it will return the answer, otherwise, it will
## compute the inverse and store it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv        
}
