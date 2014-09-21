## The makeCacheMatrix function creates a special 'matrix' object
## that can cache its inverse by setting a global variable

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                ## set the global variables x and m
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## inline definition of the setinverse function calls the standard R function solve.
        ## solve returns the inverse of the matrix passed as a parameter. setinverse 
        ## calculates the inverse and pushes the value to global variable m
        setinverse <- function(solve) m <<- solve
        
        ## getinverse simply returns m 
        getinverse <- function() m
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the 'special' matrix returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' by calling the getinverse funtion defined above
        m <- x$getinverse()
        
        ## check if m is already populated. If m is not null, meaning m is populated from 
        ## previous calls to setsolve below, then return m
        if(!is.null(m)) {
                message("getting cached data for matrix inversion")
                return(m)
        }
        
        ## if m is null then
        ## pass the function parameter to variable 'data'.
        data <- x$get()
        
        ## call the solve function
        m <-solve(data,...)
        
        ## push the inverse matrix to global variable m by calling
        ## setinverse
        x$setinverse(m)
        
        ## return the value of m, which is the inverse matrix
        m
}
