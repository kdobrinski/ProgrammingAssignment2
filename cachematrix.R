## 2 functions, first is defined to store the matrix data and store the inverse matrix in cache.
## it resets the cache whenever a matrix is stored.
##The second functions tries to pull the cached inverse matrix, if it is not found it extracts the 
##data from the first function, runs solve on the data, stores it in the cache. it then returns the inverse

## makeCacheMatrix takes in a matrix variable and stores it. It creates a variable i to store
## computed inverse of matrix x. It defines 4 functions to get/set the matrix and cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##takes in a makeCacheMatrix, tries to get the inverse matrix from it's cache, if none is found
##it computes the inverse and stores it in the makeCacheMatrix i variable
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

##test to run code
##A <- matrix( 
##   c(2, 4, 3, 1), 
##   nrow=2, 
##   ncol=2)
##B <- makeCacheMatrix(A)
##C <- cacheSolve(B)
##D <- cacheSolve(B)
