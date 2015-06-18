## Cachematrix allows for the creation of a special matrix with
## additional functions that enable it to calculate its inverse
## and to cache and retrieve itself and its inverse from cache.
## It is assumed that the matrix is invertible.

## MakeCacheMatrix takes a matrix and returns a list of functions
## to alter, store and retrieve the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## CacheSolve takes a special makeCacheMatrix-wrapped matrix 
## and returns its inverse. If the inverse is already cached,
## that value is returned. Otherwise, cacheSolve calls solve()
## to calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
