# In this example we introduce the <<- operator 
# which can be used to assign a value to an object 
# in an environment that is different from the current environment. 
# Below are two functions that are used to create a special object 
# that stores a numeric vector and cache's its mean.

# The first function, makeVector creates a special "vector", 
# which is really a list containing a function to
# 
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeVector <- function(x = numeric()) {
        
        # initialize m
        m <- NULL
        
        # define the set function
        set <- function(y) {
                # use this function to set a value to x
                x <<- y
                # initialize m
                m <<- NULL
        }
        
        # define the get function
        # return the value of x
        get <- function() x
        
        # define the setmean function
        setmean <- function(mean) {
                # set m to the value passed in
                m <<- mean
        } 
        
        # define the getmean function
        getmean <- function() m
        
        # this is what this function returns:
        # a list of functions for this vector
        list(set = set, 
             get = get,
             setmean = setmean,
             getmean = getmean)
}

# The following function calculates the mean of the special "vector" 
# created with the above function. 
# However, it first checks to see if the mean has already been calculated. 
# If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the 
# value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
        
        # attempt to get the mean from the other function
        m <- x$getmean()
        
        # if found, return it and exit
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # otherwise, get the original vector
        data <- x$get()
        
        # calculate its mean
        # how is "data" initialized??
        m <- mean(data, ...)
        
        # set it
        x$setmean(m)
        
        #return it
        m
}