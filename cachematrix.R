## Matrix inversion is computationally expensive. We compute the inverted matrix ones, and 
## caches the result into variable "s". Upon subsequent uses we recall that object

## a list of four auxiliary function to set and get the matrix, and the inverted matrix. (setsolv,getsolv)

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolv <- function(solve) s <<- solve
        getsolv <- function() s

        ## returns the list of four auxiliary function
        list(set = set, get = get,
             setsolv = setsolv,
             getsolv = getsolv)
}


## compute or fetch the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## try to fetch the inverse into s
        s <- x$getsolv()

        ## if s is not empty, return it!
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## otherwise compute it by function "solve"
        data <- x$get()
        s <- solve(data, ...)
        x$setsolv(s)

        ## and return the computed object:
        s

}
