## Put comments here that give an overall description of what your
## functions do

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
    
    ## to store a matrix into cache
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

cacheSolve <- function(x, ...) {
    # Receives a cached metrix produced with the makeCachedMatrix builder
    # Function. It retrives the cached inverse of the matrix 'x'.  
    #
    # IF it is NULL it calculates the inverse of the matrix 'x' and sets it for 
    # future reference. 
    #
    # Else it returns the cached inverse
    #
    # Args:
    #   x:  a cached metrix, which can store its inverse
    #
    # Returns: 
    #   The inverse of the matrix 'x'
    
    # Check for cached result and return the cached inverse if available 
    inv <- x$getinverse() 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Calculate the inverse and store it for future reference
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
   
    ## Return the inverse of 'x'
    inv
}


testAssignment2 <- function () {
    # A function to test the 
    A <- matrix( c(2, 4, 3, 1, 5, 7, 2, 1, 3), nrow=3, ncol=3, byrow = TRUE)
    A_inverse = solve(A)
    A_cached = makeCacheMatrix(A)
    
    message("Test: Matrix is stored in cachedMatrix:", 
            all(A == A_cached$get()))
    
    message("\nTest: Inverse is not cached when we first set a matrix:",
            all(is.null(A_cached$getinverse())))
        
    message("\nTest on first call to cacheSolve.") 
    message("\tShould solve matrix:",
            all(A_inverse == cacheSolve(A_cached)))
    message("\tShould store inverse to cache:",
            all(A_inverse == A_cached$getinverse()))
    
    message("\nTest: on second call to cacheSolve.")
    message("Should retreive inverse metrix from the cache.")
    ls()
    #ls(environment(A_cached))
    #print(cacheSolve(A_cached))
}