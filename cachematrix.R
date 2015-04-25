##*******************************************************************************
## This the R programming Assignment #2.
## There are 2 functions.
## 1. makeCacheMatrix : to return a special matrix that has 4 functions in a list
## 2. cacheSolve: to return the inverse of the special matrix from either cache 
##    result or calculate it in real time.
##*******************************************************************************


## This function returns 4 functions in a list: set, get, setinverse, getinverse.
## e.g. x <- matrix(1:4, nrow = 2, ncol = 2),
##      f <- makeCacheMatrix(x)
##      f will have $set, $get, $setinverse, and $getinverse four functions.
makeCacheMatrix <- function(x = matrix()) {
        #inv is a local variable for saving inverse matrix of a special matrix
        inv <- NULL 
        #set function
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get function        
        get <- function() x
        
        #setinverse function        
        setinverse <- function(inverse) inv <<- inverse
        
        #getinverse function
        getinverse <- function() inv
        
        #Return a list of these 4 functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function returns 4 functions in a list: set, get, setinverse, getinverse.
## e.g. x <- matrix(1:4, nrow = 2, ncol = 2),
##      f <- makeCacheMatrix(x)
cacheSolve <- function(x, ...) {
        #inv is a local variable for saving inverse matrix of a special matrix
        inv <- x$getinverse()
        
        #If we can get the inverse matrix from cache, return it directly and print
        #out a message
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #data is a local variable for saving special matrix
        data <- x$get()
        #Since we get here only if we cannot get the cached data of inverse matrix,
        #we need to calculate the inverse matrix in run time using solve() function.
        inv <- solve(data, ...)
        #Set the inverse matrix into the specail matrix
        x$setinverse(inv)
        #return the inverse matrix as a result
        inv
}