## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## Need to input an invertible matrix as x

makeCacheMatrix <- function(x = matrix()) {
        InverseMatrix <- NULL
        set <- function(y) {
                x <<- y # Substitute Matrix x with y (the input) in the main function (makeCacheMatrix)
                InverseMatrix <<- NULL  # restores to null the value of InverseMatrix
                                        # Old InverseMatrix is not needed anymore.
                                        # New InverseMatrix to be recalculated through cacheSolve.
        }
        get <- function() x # returns Matrix x stored in the main function
        setinverse <- function(inverse) InverseMatrix <<- inverse #  simply stores InverseMatrix value
        getinverse <- function() InverseMatrix # simply returns InverseMatrix value
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   # Store the 4 functions in the function makeCacheMatrix
                                        # function list() is required
}


##  cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        InverseMatrix <- x$getinverse()  # Read the previously stored value of the inverse of the matrix
        if(!is.null(InverseMatrix)) {    # If the value is not null then returns the cached inverse matrix
                message("getting cached data")
                return(InverseMatrix)
        }
        # Else get the inversible matrix and calculate inverse the matrix
        myMatrix <- x$get() # get the matrix
        InverseMatrix <- solve(myMatrix) # calculate the inverse of the matrix
        x$setinverse(InverseMatrix) # store the value of the inverse matrix in the cache
        InverseMatrix
}
