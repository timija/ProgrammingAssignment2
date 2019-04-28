# there are two functions: 
# 1. makeCacheMatrix which creates a list and caches it inverse
# 2. cacheSolve which caches the result of makeCacheMatrix 
#    and retrieves it if the matrix did not change

# makeCacheMatrix creates a list 
# that sets the value of the matrix, 
# gets the value of the matrix, 
# sets the value of inverse of the matrix 
# and gets the value of inverse of the matrix

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


# cacheSolve returns the inverse of the matrix, 
# if the inverse has already been computed and is stored in cache. 
# if it is stored, it will retrieve it
# if it isn't, it will calculate the inverse and sets it in the cache. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
