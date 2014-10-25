## Assignemnt 2: R Programming
##
## Author: Juan Esteban Monsalve Tobon (juanestebanmt@gmail.com)
##
## Description: 
##
## Here in are a set of functions which support the creation of a cached matrix
## which stores its inverse after it has been needed for the first time. And in
## future reuse to use the cached value. 
##
## Functions:
##  makeCacheMatris(x): Handles the creation, storage, and access for a cached
##                      matrix. And re-use of the environment with a new matrix.
##
##  cacheSolve(x):      Returns the inverse of a cached matrix. re-uses the
##                      the cache if avaliable otherwise sets it.
##
##  tests():            Sets of test to validate the functions above.

makeCacheMatrix <- function(x = matrix()) {
    # A builder function which constructs an environment in which to store a 
    # cache metric and the methods by which to access the environment.
    #
    # Args:
    #   x:  A metrix to cache. If none is provided an empty metrix will be
    #       constructed
    #
    # Returns:
    #   A list of acessor methods by which to access (set/get) the internals
    #   of a cached metrix.
    
    inv <- NULL ## variable to store the inverse (inv)
    
    ## Set a new matrix and clear the cache.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # retrieving the metrix from the cache 
    get <- function() x
        
    # accessor methods for the inverse of the metrix
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    # return list with handles to the accessor functions of a cached matrix 
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}

cacheSolve <- function(x, DEBUG=FALSE, ...) {
    # Receives a cached metrix produced with the makeCachedMatrix builder
    # Function. It retrives the cached inverse of the matrix 'x'.  
    #
    # IF it is NULL it calculates the inverse of the matrix 'x' and sets it for 
    # future reference. 
    #
    # Else it returns the cached inverse
    #
    # Args:
    #   x:      a cached metrix, which can store its inverse
    #   DEBUG:  a boolean/logical which enables the wrapping of extra info when
    #           executin a value.
    #
    # Returns: 
    #   The inverse of the matrix 'x'. If DEBUG == FALSE
    #   A list containing the inverse of the matrix 'x' and a logical which 
    #   will be true if the cached is accessed else false. If DEBUG == TRUE
    
    myReturn <- function(inv, isCached) {
        # If in debug mode myReturn function return extra information in the
        # form of list, where the first item is the inverse matrix and the
        # second is a boolean letting me know if we have accedd the cached
        # inverse matrix.
        if(DEBUG == TRUE) {
            return(list(inv=inv, isCached=isCached))
        }
        return(inv)
    }
    
    # Check for cached result and return the cached inverse if available 
    inv <- x$getinverse() 
    if(!is.null(inv)) {   
        return(myReturn(inv, TRUE))
    }
    
    # Calculate the inverse and store it for future reference
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    
    # Return the inverse of 'x'
    return(myReturn(inv, FALSE))
}

tests <- function () {
    # A function to tests the interaction between the cachedMatrix and the
    # cacheSolve function. An NxN matrix is used as the test subject. We then
    # proceed to validate and verify the expected behaviour of these two 
    # functions.
    
    A <- matrix( c(2, 4, 3, 1, 5, 7, 2, 1, 3), nrow=3, ncol=3, byrow = TRUE)
    A_inverse = solve(A)
    A_cached = makeCacheMatrix(A)
    
    #function to pull the address of an object in R
    address = function(x) substring(capture.output(.Internal(inspect(x)))[1],2,17)
    
    message("Test: Matrix is stored in cachedMatrix:", 
            identical(A, A_cached$get()))
    
    message("\nTest: Inverse is not cached when we first set a matrix:",
            is.null(A_cached$getinverse()))
        
    message("\nTest on first call to cacheSolve:") 
    message("\tShould solve matrix:",
            identical(A_inverse, cacheSolve(A_cached)))
    message("\tShould store inverse to cache:",
            identical(A_inverse, A_cached$getinverse()))
    
    message("\nTest: on second call to cacheSolve:")
    result = cacheSolve(A_cached, DEBUG=TRUE)
    
    message("\tShould retreive inverse metrix from the cache:",
            result$isCached)
    
    message("\tInverse should be still logically the same:",
        identical(result$inv, A_inverse))
}