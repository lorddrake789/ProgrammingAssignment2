# This function creates a matrix x and also 
# calculates the inverse of it and stores in 
# cache
makeCacheMatrix <- function(x = matrix()) {
    # initial value of cache set to zero
    invcache <- NULL
    # setting the matrix x
    set <- function(newX) {
        x <<- newX
        # clearing the cache
        invcache <<- NULL
    }
    
    # returns the matrix x
    get <- function() {
        x
    }    
    
    # cache the inverse
    setInv <- function(solve) {
        invcache <<- solve
    }
    
    #get the cached Inverse
    getInv <- function() {
        invcache
    }
    
    #returns inverse of X
    invcache
}

# The following function returns the inverse of a 
#"special" matrix from cache created with makeCacheMatrix 
# and if not yet calculated, it calculates the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invcache <- x$getInv()
    ## checks if the cache contains inverse of x
    if(!is.null(invcache)) {
        message("getting cached data")
        return(invcache)
    }
    
    #else get the matrix from makeCacheMatrix,
    # calculate the inverse and store it in the cache
    data <- x$get()
    invcache <- solve(data, ...)
    x$setsolve(invcache)
    
    #return the inverse
    invcache
}

