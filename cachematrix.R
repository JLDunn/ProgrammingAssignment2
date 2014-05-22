# This function creates a special "matrix" object that can cache its inverse.
# The script will be committed on GitHub under JLDunn/ProgrammingAssignment2
#
# The first function, makeCacheMatrix creates a special "matrix", which will:
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
#
makeCacheMatrix <- function(x = matrix()) {
        matrixInverse <- NULL
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) matrixInverse <<- inverse
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
        #x Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
