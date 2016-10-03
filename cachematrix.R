## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
cache <- NULL
set <- function(y){
                x <<- y
                cache <<- NULL
        }

        get <- function() x
        # invert the matrix and store in cache
        setMatrix <- function(inverse) cache <<- inverse
        # get the inverted matrix from cache
        getInverse <- function() cache
        list(set = set, get = get,
             setMatrix = setMatrix,
             getInverse = getInverse)
}




## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
 cache <- x$getInverse()

        # return inverted matrix from cache if it exists
        # else create the matrix in working environment
        if (!is.null(cache)) {
                message("getting cached data")

                # display matrix in console
                return(cache)
        }

        # create matrix since it does not exist
        matrix <- x$get()

        # make sure matrix is square and invertible
        # if not, handle exception cleanly
        tryCatch( {
                # set and return inverse of matrix
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)

                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)

                return(NA)
        },
        finally = {
                # set inverted matrix in cache
                x$setMatrix(cache)
        } )

        # display matrix in console
        return (cache)
}
