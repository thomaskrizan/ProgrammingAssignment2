## makeCacheMatrix makes a special matrix  object
## cacheSolve calculates the inverse of the special matrix object
## if the inverse is already solved it will find the result and return it from the cache
## instead of recalculating it

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y #set value of matrix
    m <<- NULL #clear old matrix
  }
  get <- function() x #get value of matrix
  setInverse <- function(inverse) m <<-inverse#set inverse if there is no cached inverse
  getInverse <- function() m #function to get the inverse
  #create and return a list with the set,get,setInverse, and getInverse functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse() #get cached inverse value
  if(!is.null(m)) { #if cache isn't empty just return it
    message("getting cached data")
    return(m) 
  }
  data <- x$get() #get matrix value
  m <- solve(data, ...) #calc inverse
  x$setInverse(m) #cache result
  m #Return a matrix that is the inverse of 'x'
}
