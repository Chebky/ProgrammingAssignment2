## These functions create an object that holds and retrieves a matrix and its 
##inverse (if it is invertible)

## makeCacheMatrix receives a matrix x and constructs a list of four functions 
## that allow one to set and retrieve the matrix and another object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) m <<- Inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve receives a list created by makeCacheMatrix and returns the inverse
## of the saved matrix inside. If the list contains the inverse it returns it
## without computing. Otherwise, computes and updates the list

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}