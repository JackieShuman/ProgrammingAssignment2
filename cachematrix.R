## Compute inverse of square matrix
## or retrieve previously calculated inverse from the cache


## store matrix and inverse matrix values
makeCacheMatrix <- function(x = matrix()) {
    #initialize objects
    im<- NULL
    #set asigns input argument to x in parent environ
    #and assigns value of NULL to im bject in parent environ
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    #get retrieves data for x
    get <- function() x
    #setinvmatrix assigns input argument "im" in parent environ
    setinvmatrix <- function(invmatrix)im <<- invmatrix
    #getinvmatrix retrieves inverse value "im"
    getinvmatrix <- function () im
    #assign values to list and put into parent environ
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## Calclate inverse of matrix from makeCacheMatrix 
## or retrieve from cache if previously calculated
cacheSolve <- function(x, ...) {
    
    im <- x$getinvmatrix()
    #check if inverse matrix value exists in cache
    #if it does, print message and return value "im"
    if(!is.null(im)){
        message("getting cached data")
        return(im)
    }
    #if no "im" value in cache then calculate inverse
    #call get function to retrieve matrix data
    data <-x$get()
    ## Return a matrix that is the inverse of 'x'
    im <- solve(data, ...)
    #call setinvmatrix function to assign "im"value
    x$setinvmatrix(im)
    im
}
