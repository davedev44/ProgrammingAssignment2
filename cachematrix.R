# Collaboration note: I took a look at the discussion forum post 
# "Testing the codes" as I was looking for some confirmation that I was on the
# right track for usage of these functions. It did confirm that I was on the
# right track. I did not use it for any code (as none was shared anyway).

## Pair of functions that allow for caching of the inverse
## of a matrix so that if we already calculated the inverse
## we can just retrieve it rather than calculating the
## inverse every time we need it

## makeCacheMatrix: create object that takes a matrix and allows for caching
## of its inverse
makeCacheMatrix <- function(x = matrix()) {
    # i - stores the cached inverse
    i <- NULL
    
    # function set the matrix into the object
    # and set inverse to null initially
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # get the matrix back
    get <- function() x
    
    # set the inverse of the matrix
    setinverse <- function(inv) i <<- inv
    
    # get the inverse of the matrix
    getinverse <- function() i
    
    # returned object - the list of the four above
    # functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the object x,
## which was created with a matrix
## by calling makeCacheMatrix;
## if its inverse has been cached,
## we get it and return it. If not,
## we compute its inverse and set
## into the object so that if we
## want the inverse again, it is cached.
cacheSolve <- function(x, ...) {
    # Try to get the inverse from the object 
    m <- x$getinverse()
    
    # If non-null, means the inverse has been cached
    # and we simply return the cached result
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Otherwise, get the matrix from the object
    mtx <- x$get()
    
    # Compute the inverse of that matrix
    i <- solve(mtx)
    
    # Set the inverse into the object
    x$setinverse(i)
    
    # Return the inverse
    i
}
