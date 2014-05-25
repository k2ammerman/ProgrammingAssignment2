## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set function
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    ## get function
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## cacheSolve receives a matrix, if the matrix object is empty
## the cached matrix id returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- rev(data, ...)
    x$setmatrix(m)
    m
}
