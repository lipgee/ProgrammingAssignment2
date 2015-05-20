## The following two functions named makeCacheMatrix and cacheSolve
## is to help in matrix computation activity. Generally the operation
## to inverse a matrix is consider a resource intensive and potential
## time consuming activity.
## makeCacheMatrix function is to cache a matrix object
## Couple of features have been built around the matrix object,
## ie: set, get, setsolve, getsolve
## cacheSolve is a function that will inverse the matrix by using
## solve() function. If a particular matrix has been inverse before,
## it will take the inversed result directly from the cache and skip
## the processing. This helps in speed up the computation on same matrix
## data
## Listed below are the example steps to utilize these functions:
## Step 1: Create a matrix object and have it cached
##              a <- makeCacheMatrix(matrix(1:4,2,2)) 
## Step 2: Call cacheSolve function to inverse the matrix
##              cacheSolve(a)

## makeCacheMatrix function cache the matrix object
## this function takes an argument of matrix object type

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve inverse a matrix, if the matrix has been inversed before,
## the matrix inverse result will be taken directly from cache and
## computation will be skipped.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
