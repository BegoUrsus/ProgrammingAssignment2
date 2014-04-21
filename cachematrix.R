# Assignment: Caching the Inverse of a Matrix
# 
# Matrix inversion is usually a costly computation 
# and their may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#         
#         makeCacheMatrix: This function creates a special "matrix" object 
#         that can cache its inverse.
# 
#         cacheSolve: This function computes the inverse of the special "matrix" 
#         returned by makeCacheMatrix above. 
#         If the inverse has already been calculated (and the matrix has not changed), 
#         then the cachesolve should retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# We assume that the matrix supplied is always invertible.
# 
# 

# makeCacheMatrix: This function creates a special "matrix" object 
#         that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        #inv will store the cache inverse matrix of x
        inv <- NULL 
        
        # set the value of the matrix x (not the inverse)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # get the value of the matrix x (not the inverse)
        get <- function() {
                x
        }
        
        # set the value of the inverse matrix (stored in inv)
        setinverse <- function(inverse) {
                inv <<- inverse
                
        }
        
        # get the value of the inverse matrix (stored in inv)
        getinverse <- function() {
                inv
        }
        
        # returns a list of all the above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


# The following function calculates the inverse of the special "matrix" 
# created with the above function (makeCacheMatrix). 
# It first checks to see if the inverse has already been calculated. 
#       If so, it gets the inverse from the cache and skips the computation. 
#       Otherwise, it calculates the inverse of the matrix and sets 
#               the value in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # check if the inverse already exists
        m <- x$getinverse()
        
        # if m is not null, the inverse already exists, 
        # so we get it from the cache and return the inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if it doesn't exist, we calculate the inverse matrix,
        # store it, and return the inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

##test cases
# m1 <- matrix(1:4, 2)
# cm1 <- makeCacheMatrix(m1)
# cm1$get()
# cacheSolve(cm1)
# cacheSolve(cm1)
# m2 <- matrix(5:8,2)
# cm2 <- makeCacheMatrix(m2)
# cm2$get()
# cacheSolve(cm2)
# cacheSolve(cm2)