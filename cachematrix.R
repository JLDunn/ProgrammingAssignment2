# This function creates a special "matrix" object that can cache its inverse.
# The script will be committed on GitHub under JLDunn/ProgrammingAssignment2
#
# The first function, makeCacheMatrix creates a special "matrix", which will:
#
makeCacheMatrix <- function(x = matrix()) {
        # 1. set the value of the matrix
        matrixInverse <- NULL
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        # 2. get the value of the matrix
        get <- function() x
        # 3. set the value of the inverse
        setInverse <- function(inverse) matrixInverse <<- inverse
        # 4. get the value of the inverse
        getInverse <- function() matrixInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve should retrieve the
# inverse from the cache.
#
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        # Check to see if the inverse has already been calculated anc cached
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # ELSE calculate the matrix that is the inverse of 'x'
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
