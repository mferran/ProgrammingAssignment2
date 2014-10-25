# Overall : Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# Function: makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix
        i<- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        
        ## set the inverse of the matrix
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        ## get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the special matrix created with the above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data 
## and sets the value of the mean in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse of the matrix        
        i <- x$getinverse()
        
        ## check if there is the matrix   
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if not: get the inverse of the matrix   
        data <- x$get()
        i <- solve(data)
        ## set the inverse of the matrix 
        x$setinverse(i)
        i
}

