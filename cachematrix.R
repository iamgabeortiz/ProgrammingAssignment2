## BEGIN Overall Explanation ##
## The first function, makeCacheMatrix creates a special "matrix", which is
## really a list containing a function to:

##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the inverse matrix

## The second function calculates the inverse of the special "matrix" created
## with the first function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setsolve function.
## END Overall Explanation ##

## BEGIN EXAMPLE ##                     
## > x <- matrix(rnorm(25), nrow = 5)   //Create matrix
## > s <- makeCacheMatrix(x)            //Create special matrix
## > s$get()                            //Return the matrix
## > cacheSolve(s)                      //Return the inverse
## > cacheSolve(s)                      //Second call will return
##                                      //the cached version
## END EXAMPLE ##

## BEGIN FUNCTIONS ##
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Instantiate variable to store inverse matrix
        s <- NULL
        
        ## Set method for the matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        ## Get method for the matrix
        get <- function() x
        
        ## Set method for the inverse matrix
        setsolve <- function(solve) s <<- solve
        
        ## Get method for the inverse matrix
        getsolve <- function() s
        
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Get the inverse of our matrix
        s <- x$getsolve()
        
        ## If the inverse matrix is calculated, return it
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        ## Otherwise, get the matrix
        data <- x$get()
        
        ## Calculate the inverse matrix
        s <- solve(data, ...)
        
        ## And store the inverse matrix
        x$setsolve(s)
        
        ## Return a matrix that is the inverse of 'x'
        s
}
## END FUNCTIONS ##