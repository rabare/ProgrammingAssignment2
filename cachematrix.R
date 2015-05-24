## Programming Assignemt Two - Rick A
## This file consists of two functions
#
# The first function creates a 'object' (a la a more typical Object Oriented Language)
#  in the form of a list of four "constructor" type functions. The first two, set and get,
#  create and 'call' (return) a Matrix. The second two, are used within the second function, "cacheSolve",
#  to calculate and store the inverse of the matrix.
# 
# The second function takes an object of the first function as it's input arguement,
#  then calculates and returns the inverse, using the second two functions built by the
#  makeCacheMatrix function.

# IMPORTANT NOTE: As per assignment instructions, Matrices created / passed are assumed to be nonsingular 
#  so that the inverse does exist. Due to this (and for testing purposes), the CREATOR function SET is set up to take
#  an VECTOR as input. This length of this vector must be a square number. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Define the matrix inverse storage object, set to NULL
    matInvStore <- NULL
    
    # The SET function. Creates the actual matrix in the makeCacheMatrix object. 
    # IMPROTANT NOTE MUST pass it a vector of numbers
    #  BUT IT MUST BE OF SQUARE LENGTH (i.e. 4, 9, 16 etc..)
    #  Also NOTE this function goes byCol
    set <- function(y) { 
      dimSize <- as.integer(sqrt(length(y))) 
      x <<- matrix(y, nrow = dimSize)  # Assigns the matrix initialization arguement of set to the makeCacheMatrix object x         
      matInvStore <<- NULL # sets the inv storage object to NULL (for when any matrix after the first is initialized)
    }
    
    # The GET function. 'Returns' (gets) the Matrix part of makeCacheMatrix.
    get <- function() x
    
    # The setInverse function. Sets up the type of object that our inverse storage object will be after solving
    #  so the original makeCacheMatrix object can take the type of object that the solution is. 
    setInverse <- function(solve) matInvStore <<- solve
    
    # The getInverse function. Used for 'getting' (passing) the matrix inverse storage object to cacheSolve
    getInverse <- function() matInvStore
    
    # The actual definition of the list object created by makeCacheMatrix
    list(set = set, get = get,
         setInv = setInverse,
         getInv = getInverse)
}

## This function takes object created by makeCacheMatrix and calculates and returns its inverse or
##  returns back the cached inverse if one has already been calculated

cacheSolve <- function(x, ...) {
    # Returns a matrix that is the inverse of 'x'
    matInvStore <- x$getInv() # grabs and assigns the inverse object (inv matrix if it exists, NULL if not)
    
    # Logical test, to determne if the inverse matrix we just tried to retrieve is not NULL, 
    #  this is what indicates the cached matrix has been retreived
    if(!is.null(matInvStore)) {
        message("getting cached data ;)") # with some flair
        return(matInvStore)
    }
    
    # If the inverse is not already cached, the following code is executed
    data <- x$get() # stores the created matrix in a temporary object
    matInvStore <- solve(data, ...) # Solves the matrix and assigns it's inverse to our temporary object
    x$setInv(matInvStore) # caches the inverse of the matrix into the created matrix object
    matInvStore # returns the inverse of the matrix to the console
}



