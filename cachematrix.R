# Matrix inversion is usually a costly computation and there may be some benefit 
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The function makeCacheMatrix() and cacheSolve() let us deal with this aspect
# by speeding up our computation when the inverse of the matrix is already
# stored in our environment.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                # with this line we substitute our matrix
                x <<- y
                # restoring a null value to our inverse because of the new input
                inverse <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) inverse <<- solve
        get_inv <- function() inverse
        # storing our 4 functions in a list
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


# The cacheSolve() function let us compute the inverse of our matrix by checking
# whether it has been already computed and therefore stored in the cache.

cacheSolve <- function(x, ...) {
        # recalling the value for the inverse through x$get_inv()
        inverse <- x$get_inv()
        # if inverse is different from NULL then cacheSolve() returns the 
        # stored value
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # if we have no data in memory, then the function computes the inverse
        # by recalling our input [x$get()]. After that, we store our inverse
        # in our environment [x$set_inv(inverse)]
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inv(inverse)
        inverse
}

### Sample output

# > source("cachematrix.R")
# > z <- makeCacheMatrix(matrix(5:8, 2, 2))
## since we have no inverse stored in our data, the function simply computes
## the inverse of our input z
# > cacheSolve(z)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5

## if we run again our function, then the function recognizes that the value
## is already stored. Hence, we get also a message that points it out

# > cacheSolve(z)
# getting cached data
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5