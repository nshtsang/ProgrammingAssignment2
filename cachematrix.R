## Function allows you cache the inverse of a matrix.
## cacheing the inverse matrix allows you to recall
## potentially time-consuming computation

## First function, makeCacheMatrix creates a special matrix object
## that caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y){
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinv <- function(solve) invm <<- solve
        getinv <- function() invm
        list(set = set, get =get, 
             setinv = setinv, getinv = getinv)
}


## Function generates the inverse of the matrix from
## the function makeCachematrix. If the inverse matrix
## already exist, the inverse matrix is retrieved for the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinv()
        if(!is.null(invm)){
                message("getting cached data")
                return(invm)
        } 
        dat <- x$get()
        invm <- solve(dat, ...)
        x$setinv(invm)
        invm
}