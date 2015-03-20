## This is a pair of functions that cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
        
        s <- NULL # inverse matrix
        set <- function(y) { # set the value of the matrix
                x <<- y 
                s <<- NULL 
        }
        get <- function() x # get the value of the matrix
        setsolve <- function(solve) s <<- solve # Set value of inverse
        getsolve <- function() s # Get value of inverse
        list(set = set, get = get, 
                setsolve = setsolve, 
                getsolve = getsolve) 
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) { # if s is not null
                message("getting cached data") # Show message
                return(s) # display inverse
        }
        data <- x$get() # if s is null
        s <- solve(data, ...) # solve for inverse
        x$setsolve(s) # cache solved inverse
        s # print inverse
}