## Below are two functions that are used to create a special object 
## that stores a matrix and cache's its inverse. 

## Function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix - function set
## get the value of the matrix - function get
## set the value of the inverted matrix - function setinverse
## get the value of the inverted matrix - function getinverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Function calculates the inverse of matrix. 
## At first it checks if the inverse of matrix has already been calculated.
## If so, it gets the inverted matrix from the cache and skips the computation.
## Otherwise, it calculates the inverted matrix from data and sets its value in the cache 
## using the setinverse function

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
