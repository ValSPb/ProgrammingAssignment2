## These two functions dedicate to cache the inverse of a matrix. 
## They are based on the functions given in the assigment's instructions


## This function creates a special "vector", which is
## really a list containing four functions:
## set, get, setInv, getInv to set/get matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        # initialize mInv (=inverse matrix) with NULL
        mInv <- NULL
        
        # assign y to the x
        # additionally assign NULL value to mInv
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        
        # return the x
        get <- function() x
        
        # assign mtrInv to the mInv
        setInv <- function(mtrInv) mInv <<- mtrInv
        
        # get the inversed matrix = return the value of mInv
        getInv <- function() mInv
        
        # return a list of these functions
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}



## This function returns the inverse of the special "vector"
## created with makeCacheMatrix.
## If the inverse has already been calculated,
## it gets it from the cash.
## Otherwise, it calculates the inverse of the 
## matrix and sets the value of the inverse in the cashe 
## via the `setInv` function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check what we already have in the cash for the inverse
        mInv <- x$getInv()
        if(!is.null(mInv)) {
                ## the invserse is already in the cash
                ## just return it
                message("getting cached data")
                return(mInv)
        }
        
        ## no inverse in the cash
        ## calculate the inverse of the input matrix
        data <- x$get()
        mInv <- solve(data, ...)
        x$setInv(mInv)
        mInv        
}
