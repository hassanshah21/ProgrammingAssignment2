## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  Here is the funcation "makeChacheMatrix" which take one matrix which is suppose to be invertible matrix and put those matrix in cache,
##  after that solve funcation find the inverse of matrix and store in cache as well
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y      ##  storing matrix to the cache
    m <<- NULL  
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve   ##  assigning the solve to m  and storing in cache for further use
  getinverse <- function() m    ##  returning the resutl m by getinverse 
  list(set = set, get = get,            ##   return the list of function which is used by cacheSolve     
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##  this function cachesolve get the matrix and check its inverse is already solved then get the
##  from cache otherwise this function will find the inverse and stoe into cache for next time uses
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()     ##  get the inverse of matrix from cache
  if(!is.null(m)) {        ##  checking it wether inverse was already in cache or not
    message("getting cached data") ##   if its founded in cache the return the result
    return(m)
  }
  data <- x$get()       ##  getting the matrix
  m <- solve(data, ...)  ##   finding inverse using solve
  x$setinverse(m)       ##   stroing inverse in cache
  m      ##  return result(inverse)
}
