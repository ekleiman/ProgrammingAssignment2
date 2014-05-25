#  First define the function makeCacheMatrix which assigns inverted 
#  matrix value to m
#  Next, define the function cacheSolve which first screens to see if inverted matrix 
#  has already been calculated. If already calculated then it returns the inverted matrix
#  value from cached value.  If not already calculated then it computes the inverted
#  matrix value and stores it in the object passed to cacheSolve and returns the value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve(x)
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}

# Try out the functions with square matrix x below
x <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)

mat <- makeCacheMatrix(x) # define and populate the matrix
message("Running cacheSolve(mat) without caching")
cacheSolve(mat)
message("Running cacheSolve(mat) with caching")
cacheSolve(mat)