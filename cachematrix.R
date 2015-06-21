## This functions allow you to cache the inverted matrix of a function to
## avoid running time consuming operations more than one.

## makeCacheMatrix allows you to create a matrix that can store its own inverse

makeCacheMatrix <- function(x = matrix()) {
        
                i <- NULL #inverse matrix variable
                
                set <- function(y) { #stores a new matrix
                        x <<- y
                        i <<- NULL
                }
                
                get <- function() x #returns the matrix stored
                
                setinv <- function(inv) i <<- inv #saves the inverted matrix
                
                getinv <- function() i #returns the inverted matrix
                
                list(set = set, get = get, #allows access to internal functions
                     setinv = setinv,
                     getinv = getinv)
}


## chacheSolve returns the inverse of a matrix, either from cache data or a new calculation

cacheSolve <- function(x, ...) {
     
        i <- x$getinv() #gets inverse from the matrix
        
        if(!is.null(i)) {#if inverse is in cache, return inverse
                message("getting cached data")
                return(i)
        }
        
        data <- x$get() #gets matrix value
        
        i <- solve(data, ...) #solves for inverse
        
        x$setinv(i) #stores inverse in matrix
        i #returns the inverse
        
}
