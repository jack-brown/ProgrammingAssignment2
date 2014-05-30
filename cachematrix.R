## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## If the inverse matrix has already been calculated and cached
    ## then return the cached version.
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If not already calculated, then do so by 
    ## (1) getting the matrix, 
    data <- x$get()
    
    ## (2) calculating its inverse matrix,
    i <- solve(data, ...)
    
    ## and (3) caching that inverse matrix.
    x$setinverse(i)
    
    ## Finally, return the calculated inverse matrix.
    i
}
