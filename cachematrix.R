## This second programming assignment of R programming. The following 
## two functions are able to cache inverse of a matrix to avoid the repeating
## costy computation.


## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
## Vairable x is the original matrix.
## Variable s is the cached inverse matrix of x, init as NULL.

makeCacheMatrix <- function(x = matrix()) {
        #Inverse martix
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        ## Get inverse matrix using solve(), assuming that the matrix 
        ## supplied is always invertible
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
