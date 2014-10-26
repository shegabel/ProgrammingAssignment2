## Given an invertible matrix x, create a special matrix object and compute 
## and return its inverse matrix if first time or return its cached inverse.


## makeCacheMatrix: creates a special matrix object with cacheable 
## inverse, assuming the supplied  x = matrix() is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## set matrix x to new matrix y & inverse inv to NULL 
    ## in the makeCacheMatrix() environment
    
    set <- function(y) {
        x <<- y 
        inv <<- NULL 
    }
    
    get <- function() { 
        x
    }
    
    ## set inverse inv to the function argument inverse 
    ## in the makeCacheMatrix() environment.
    
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    getinverse <- function() {
        inv
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: computes the inverse of the special matrix x returned 
## by makeCacheMatrix above. If the inverse is already computed and 
## the matrix has not changed, it returns the inverse from the cache.

cacheSolve <- function(x, ...) { 
    
    inv <- x$getinverse() 
    
    # If inv is not NULL, return the cached inverse inv.
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) 
    }
    
    ## If execution reaches here, inv was NULL & a new inv of matrix x 
    ## is computed using the built in solve() function of R & returned.
    
    data <- x$get()
    
    inv <- solve(data, ...) 
    
    x$setinverse(inv)
    
    inv  
    
}

##  Test
# > source("cachematrix.R")
# > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# > amatrix$get() # Returns original matrix
#      [,1] [,2]
# [1,]  1    3
# [2,]  2    4
# > cacheSolve(amatrix) # Computes, caches, and returns matrix inverse
# [,1] [,2]
# [1,] -2 1.5
# [2,] 1 -0.5
# > amatrix$getinverse() # Returns matrix inverse
# [,1] [,2]
# [1,] -2 1.5
# [2,] 1 -0.5
# > cacheSolve(amatrix) # Returns cached matrix inverse using previously computed matrix inverse
# getting cached data
# [,1] [,2]
# [1,] -2 1.5
# [2,] 1 -0.5
# test multiplying a matrix with its inverse gives an identity matrix
# > amatrix$get() %*% cacheSolve(amatrix)
# getting cached data
#        [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# > cacheSolve(amatrix) # Computes, caches, and returns new matrix inverse
# [,1] [,2]
# [1,] -0.13333333 0.2
# [2,] 0.01010101 0.0
# > amatrix$get() # Returns matrix
# [,1] [,2]
# [1,] 0 99
# [2,] 5 66
# > amatrix$getinverse() # Returns matrix inverse
# [,1] [,2]
# [1,] -0.13333333 0.2
# [2,] 0.01010101 0.0