## Given a matrix that is assumed to be invertible,
# create a special kind of matrix that allows you to cache its inverse. 
#
# Useful reading: 
# https://cran.r-project.org/doc/manuals/R-intro.html#Scope
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/matmult.html

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        # initialize inv
        inv <- NULL
        
        # define the set function
        set <- function(y) {
                # use this function to set a value to x
                x <<- y
                # initialize inv
                inv <<- NULL
        }
        
        # define the get function
        # return the value of x
        get <- function() x
        
        # define the setinv function
        setinv <- function(inverse) {
                # set inv to the value passed in
                inv <<- inverse
        } 
        
        # define the getinv function
        getinv <- function() inv
        
        # this is what this function returns:
        # a list of functions for this matrix
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


#  This function computes the inverse of the special "matrix" 
#  returned by makeCacheMatrix above. 
#  If the inverse has already been calculated (and the matrix has not changed), 
#  it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

        # attempt to get the inverse from the other function
        inv <- x$getinv()
        
        # if found, return it and exit
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        # otherwise, get the original matrix
        data <- x$get()
        
        # calculate its inverse
        m <- solve(data, ...)
        
        # set it
        x$setinv(m)
        
        #return it
        inv
}
