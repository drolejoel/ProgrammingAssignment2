## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setmean <- function(inverse) inv <<- inverse
        getmean <- function() inv
        list(set = set, 
             get = get,
             setmean = setmean,
             getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getmean()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)   # this line solves the matrix
        x$setmean(inv)
        inv
}

# ==========================================

# test both functions

x <- matrix(runif(36), nrow=6)           # create a matrix
solve(x)                                         # solve it

e <- makeCacheMatrix(x)
cacheSolve(e)                                    # first time calculates the matrix
cacheSolve(e)                                    # second time retrieves from cache

# compare the speed
system.time( cacheSolve(e) )                     # first time calculates the matrix
system.time( cacheSolve(e) )                     # second time retrieves from cache

