makeCacheMatrix <- function(x = numeric()) {
        s <- NULL
        set <- function(y) {
                x << - y 
                s <<- NULL 
        }
        get <- function() x 
        setsolve <- function(solve) s <<- solve #Set value of matrix
        getsolve <- function() s #Get value of matrix
        list(set = set, get = get, 
                setsolve = setsolve, #Set value of inverse
                getsolve = getsolve) #Get value of inverse
}
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) { #if s is not null
                message("getting cached data") #Show message
                return(s) #display inverse
        }
        data <- x$get() #if s is null
        s <- solve(data, ...) #solve for inverse
        x$setsolve(s) #cache solved inverse
        s #print inverse
}