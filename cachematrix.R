#This is a pair of functions that cache the inverse of a matrix.



#makeCacheMatrix: This function creates a special "matrix"
# object that can cache its inverse.

  makeCacheMatrix <- function(x = matrix()) {
  #Perepare matrix intilized to NULL
    inv <- NULL
  
  #create another function whose output will be cached in the original one
    set <- function(y) {
    x <<- y
    
  #change the value of inverse of the matrix in case the matrix was changed.  
    inv <<- NULL
    }
  
  #Gets the value of the inverse
    get <- function() x
  
  #calculates the inverse of non-singular matrix via the solve function
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
    setinverse = setinverse,
  
    # gets the inverse     
    getinverse = getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve retrieves the inverse 
# from the cache.

  cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
  #if the inverse exists, it gets it.
    if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
    
  #if the inverse if not there, first it is calculated and then retrieved.
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
    }
