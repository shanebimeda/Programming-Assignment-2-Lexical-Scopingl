## My two functions here, named 'makeCacheMatrix' and 'cacheSolve, 
## try to cache the inverse of a matrix. 

## The function 'makeCacheMatrix', which creates a special "vector" 
## (in this case, a matrix), is really a list containing a function to:
## 1. set the value of the special vector;
## 2. get the value of the special vector;
## 3. set the value of the inverse of the vector; and
## 4. get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
             x <<- y
             inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The function 'cacheSolve' calculates the inverse of the special "vector"
## (in this case, a matrix) created by the above function. However,  
## it first checks to see if the inverse was already computed. If yes,
## it no longer calculates the inverse and just gets the result. If not, 
## it computes the inverse and sets the value in the cache via the 
## setinverse function.

## The function also assumes that the matrix is an invertible square matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

