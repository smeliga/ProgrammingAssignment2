## The following functions cache the potentially time-consuming computation of
## the inverse of a matrix. If the content of a matrix is not changing, the
## value of its inverse is looked up in the cache rather than recomputed.


## makeCacheMatrix generates a list containing the following functions:
## 1) set caches a matrix;
## 2) get gets the matrix from the cache;
## 3) setinv caches the inverse of the matrix,
## 4) getinv gets the matrix inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes a makeCacheMatrix object and returns the inverse of the
## matrix. If the inverse has already been calculated and cached, cacheSolve
## gets its value from the cache through the getinv function.
# Otherwise, cacheSolve
# 1) gets the matrix from the cache through the get function,
# 2) calculates the inverse using the function solve {base}, and
# 3) caches the inverse value using setinv.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    dim <- dim(data)[1] # calculate matrix dimension
    inv <- solve(data, diag(dim), ...) # diag(dim) builds the identity matrix
    x$setinv(inv)
    inv
}
