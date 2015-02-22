## These two functions solve for the inverse of a matrices and cache the results
##

## makeCacheMatrix takes a matrix x as a parameter
## It then creates 4 functions to do operations on x caching and returning the inverse
## The returned vector has all the functions created within

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) { #Creates set function that sets matrix x as given y
    x <<- y
    i <<- NULL
  }
  get <- function() x #get function returns matrix x
  setinverse <- function(solve) i <<- solve #setinverse function caches the inverse of the matrix
  getinverse <- function() i #getinverse function returns the inverse i
  list(set = set, get = get, #return a vector of the created functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of another matrix
## It first tries to get the inverse i from the cache using getinverse and returns it if successful
## If it doesn't get back an i then it retrieves the matrix data, calculates the inverse, and caches that
## Returns i the inverse

cacheSolve <- function(x, ...) {
  i <- x$getinverse() ##Try to get inverse from cache
  if(!is.null(i)) { ##Return cached inverse if possible
    message("getting cached data")
    return(i)
  }
  data <- x$get() ##Get matrix data 
  i <- solve(data, ...) ##Solve for inverse of data
  x$setinverse(i) ##Cache inverse of data
  i ##Return inverse i of data
}
