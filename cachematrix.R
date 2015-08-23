# This function takes a User inputted value 'x' and creates
# a square matrix that is capable of being inverted.
# Invalid values of 'x' cause the code to end.
# The function then goes onto create a LIST of functions
# ie. 'set', 'get', 'setinv' and 'getinv' which can get called in 
# the function 'cacheSolve.R'

makeCacheMatrix <- function(x = numeric()) {
  l <- sqrt(x) # calcs the square root of 'x'
  if (!isTRUE(all.equal(l, as.integer(l)))) { # checks if 'l' is a whole number
    message("your matrix is NOT invertible. Try again") # code ends
  }
  else {
    rn <- rnorm(x) # vector of random numbers length(x)
    x <- matrix(rn, l, l) # coverts 'rn' to a square matrix
    mtxinv <- NULL # initialises variable 'mtxinv' to NULL
    
    # creating the functions 'set', 'get', 'setinv' and 'getinv'
    set <- function(y) {
      x <<- y
      mtxinv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) mtxinv <<- solve
    getinv <- function() mtxinv
    
    # creating the LIST of the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

  
  # This function calculates the INVERSE of the matrix created
  # in the function makeCacheMatrix.
  # First it checks the variable 'mtxinv' to see if this inverse has
  # already been calculated. If 'mtxinv' has a value it gets the calculation
  # from the cache and skips the rest of the code.
  # Otherwise, it computes the matrix INVERSE and sets this as the value of
  # 'mtxinv' in the cache via the setinv function.
  
  cacheSolve <- function(x, ...) {
    
    # check for inital INVERSE calculation
    mtxinv <- x$getinv() # gets 'mtxinv' data. Default = NULL
    if(!is.null(mtxinv)) { # checks if 'mtxinv is NULL
      message("getting cached data") # message that cache data is being collected
      return(mtxinv) # 'mtxinv' is returned and code ends
    }
    
    # Calculates INVERSE of 'mtxinv' = NULL and and use setinv function to set the
    # value of 'mtxinv' to this INVERSE in the cache
    data <- x$get() 
    mtxinv <- solve(data)
    x$setinv(mtxinv)
    mtxinv
  }
}
