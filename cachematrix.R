## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix and assigns appropriate methods (set, get, setInv, getInv) 
## to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL              # set the inverse matrix to NULL when makeCacheMatrix is called
    # define the methods/functions that will be used by cacheSolve
    # set method is only for OOP completeness & is not used in this project
    set <- function(y) {                   # take the input matrix
        x <<- y                            # save the input matrix
        m <<- NULL                         # reset the inverse matrix to NULL 
    }
    get <- function() x                    # return the original matrix
    setInv <- function(solve) m <<- solve  # use superassignemnt to store the inverse matrix
    getInv <- function() m                 # return cached inverse matrix (if avaiable)
    list(set = set,                        # list of methods defined above 
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve solves for the inverse of a matrix, using a cached inverse matrix if already calculated
## uses getInv and setInv definded above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()                    # get the value of the inverse matrix from the object 'x'
    if(!is.null(m)) {                  # if the inverse matrix was already cached
        message("getting cached data") # ...send this message to the console
        return(m)                      # ...and return the cached matrix
    }
    # if the inverse matrix wasn't cached...
    data <- x$get()                    # get the matrix & assign to data
    m <- solve(data, ...)              # calculate the inverse matrix and assign to m
    x$setInv(m)                        # store the calcuated inverse matrix in x
    m                                  # return the inverse matrix
}