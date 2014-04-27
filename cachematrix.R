## Programming assignment - self assessment 
## These two functions implement a technique that allows the use of a class-like 
## set of functions. The objective to to either 1) calculate the inverse of a matrix from scratch,
## or 2) to pass the pre-calculated inverse. 

## This is the first function to be run. It defines four sub-functions (get(), set(), setmean(), getmean() ) 
## that get passed in the cacheSolve function.  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                              # initialize inverse to NULL
        set <- function(y) {                     # passes the called free variable "y" back to the calling function
                x <<- y
                inv <<- NULL                     # ...and sets the inverse matrix to NULL
        }
        get <- function() x
        setmean <- function(inverse) inv <<- inverse
        getmean <- function() inv
        list(set = set,                          # this defines the 4 sub-functions that are made vailable
             get = get,                          # in the "class"
             setmean = setmean,
             getmean = getmean)
}


## The cacheSolve function uses the four "sub-functions" defined above to determine if the inverse
## of the matrix has already been cached. If it has, then it uses the cached value. If it has not, 
## then it calculates the inverse matrix

cacheSolve <- function(x, ...) {
        inv <- x$getmean()
        if(!is.null(inv)) {                      # if the inverse has already been cached, then use it
                message("getting cached data")
                return(inv)
        }
        data <- x$get()                          # this line uses the get() function to use the original matrix
        inv <- solve(data, ...)                  # this line solves the matrix
        x$setmean(inv)                           # caches the calculated inverse as object "inv"
        inv
}

# ==========================================

# test both functions

set.seed(2604)
x <- matrix(runif(36), nrow=6)                   # create a matrix
solve(x)                                         # solve it -- this generates the expected answer manually

e <- makeCacheMatrix(x)
cacheSolve(e)                                    # first time calculates the matrix
cacheSolve(e)                                    # second time retrieves from cache

# repeat with a larger matrix to compare the speed.  
# First create a bigger matrix like this:

x <-  matrix(runif(1000000), nrow=1000)
e <- makeCacheMatrix(x)
system.time( cacheSolve(e) )                     # first time calculates the matrix
system.time( cacheSolve(e) )                     # second time retrieves from cache; much faster

