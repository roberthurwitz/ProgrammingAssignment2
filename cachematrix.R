## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function () m
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by 
##      makeCacheMatrix above. If the inverse has already been calculated 
##      (and the matrix has not changed), then the cachesolve should retrieve 
##      the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached solve result")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


# Usage
#       > c=rbind(c(1, -1/4), c(-1/4, 1))
#       > cm <- makeCacheMatrix(c)
#       > cacheSolve(cm)
#       [,1]      [,2]
#       [1,] 1.0666667 0.2666667
#       [2,] 0.2666667 1.0666667
#       > cacheSolve(cm)
#       getting cached solve result
#       [,1]      [,2]
#       [1,] 1.0666667 0.2666667
#       [2,] 0.2666667 1.0666667