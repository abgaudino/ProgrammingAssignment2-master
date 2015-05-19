## Put comments here that give an overall description of what your
## functions do
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
##invertible matrix used in cacheSolve()
## Write a short comment describing this function
##to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    ##introduce <<- to assign object
    x<<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  return(inv)
}

## The below "test run" is derived from masterr.org, with 
## different numbers entered into the test matrix
## Time difference of 0 seconds was observed

test <- function(mat){
  ## matrix is an invertible matrix
  
  temp <- makeCacheMatrix(mat)
  
  start.time <- Sys.time()
  cacheSolve
  dur = Sys.time() - start.time
  print(dur)

}

set.seed(1110305)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)