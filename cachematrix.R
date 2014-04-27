## Together these functions will create 1) a matrix and 2) list containing functions
## that will cache and return the inverse values of the matrix

## This function creates the matrix and puts the 4 functions in a list object
## One of the functions calculates the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function caches and returns the inverse matrix
## If no inverse values are found, then it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
