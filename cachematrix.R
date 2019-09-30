## Put comments here that give an overall description of what your
## functions do

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                            
    set <- function(y) {                    
        x <<- y                            
        inv <<- NULL                       
    }
    get <- function() x                    

    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)   
                                                                                 
}


## Function to compute inverse of the special "matrix" object returned by makeCacheMatrix
## If inverse has been calculated, and matrix not changed, then cacheSolve will retrieve inverse from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

