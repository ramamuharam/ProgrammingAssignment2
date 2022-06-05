## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  return(list(set = set, get = get,
              set_inverse = set_inverse, 
              get_inverse = get_inverse))
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  return(inv)
}

