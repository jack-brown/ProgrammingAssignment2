## As mentioned in the assignment description, computing the inverse
## of a matrix is expensive computationally.  Thus, these functions
## calculate and cache the matrix inverse.  The cached version can, then,
## be quickly retrieved as needed.

## makeCacheMatrix is a function that contains a list of
## four elements that are analogous to member functions:
## - set: assign the matrix 'y' to 'x' in the parent environment
##        and set the inverse of 'y' to NULL in the parent environment
## - get: return 'x'
## - setinverse: assign 'inverse' to 'i' in the parent environment
## - getinverse: return 'i'

makeCacheMatrix <- function(x = matrix()) {
    ## set the local variable 'i' to NULL
    i <- NULL
    
    ## set is assigned a function that takes
    ## 'y' as an argument.
    set <- function(y) {
        
        ## 'y' is assigned to 'x' in the 
        ## parent environment
        x <<- y
        
        ## NULL is assigned to 'i' in the
        ## parent environment
        i <<- NULL
    }
    
    ## get is assigned a funtion that
    ## takes no arguments and returns 'x'
    get <- function() x
    
    ## setinverse is assigned a function that
    ## takes one argument 'inverse' and assigns
    ## 'inverse' to the variable 'i' which is in
    ## the parent environment.
    setinverse <- function(inverse) i <<- inverse
    
    ## getinverse is assigned a function that 
    ## takes no arguments and returns 'i'
    getinverse <- function() i
    
    ## return the list containing the elements:
    ## set, get, setinverse, and getinverse - all
    ## of which are defined above
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a list 'x' as input and returns
## 'i', the inverse of a matrix.  It first checks to see if
## 'i' has already been calculated.  If so, it returns 'i'.
## If not, it calculates, caches, and returns the inverse 
## matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' or NULL
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
