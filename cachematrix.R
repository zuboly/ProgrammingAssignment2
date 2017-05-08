## The first function, makeCacheMatrix() creates an object that stores a 
## matrix & its inverse. The second function, cacheSolve() either returns 
## the inverse  from the chached value stored in makeCacheMatrix(), or calculates 
## the inverse of the input matrix if it was not calculated previously (i.e. not
## stored in the cache).

## The makeCacheMatrix() function first creates an empty matrix (X), and sets 
## the default value of i (=inverse) as NULL. Then with set() function it assigns
## value to x and default NULL to i in the parent environment (caching). Then the
## functions defines the setter and getter for both objects (x and i). The last
## sections builds a list by assigning each functions set(), get(), Setinverse()
## and getinverse() as an element of the list in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function starts with a single argument (x), then calls the 
## getinverse() function on the input matrix. It checks whether the value of i 
## is NULL. If it's not, the inverse is returned from the cache. If i ## is NULL,
## the solve () function calculates the inverse, then setinverse() set the inverse 
## in the input object, and returns the value of the inverse to the parent 
## environment by printing the i object.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
