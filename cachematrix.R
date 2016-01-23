## Creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(matrix = matrix()) {
   inverse <- NULL

   ## define functions
   setMatrix <- function(x) {
      matrix <<- x
      inverse <<- NULL
   }
   getMatrix <- function() matrix
   setInverse <- function(i) inverse <<- i
   getInverse <- function() inverse
   list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the matrix object. If already solved, will retrieve
## inverse from the cache.

cacheSolve <- function(m, ...) {
   ## Return a matrix that is the inverse of 'x'
   inverse <- m$getInverse()
   if(!is.null(inverse)) {
      message("getting cached inverse")
      return(inverse)
   }
   matrix <- m$getMatrix()
   inverse <- solve(matrix)
   message("solving and caching inverse")
   m$setInverse(inverse)
}



##
## Testing the Functions

n <- 4
m <- matrix(rnorm(n*n),n,n)

cat("Original Matrix:\n")
m
matrix <- makeCacheMatrix(m)

cat("\ngetMatrix() function result:\n")
matrix$getMatrix()

cat("\ngetInverse() function before cacheSolve is called, should be NULL:\n")
matrix$getInverse()

cat("\nCalling cacheSolve the first time:\n")
cacheSolve(matrix)

cat("\nCalling cacheSolve the second time:\n")
cacheSolve(matrix)







