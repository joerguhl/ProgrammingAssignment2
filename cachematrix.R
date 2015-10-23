## Functions for caching the inverse of a matrix

## Function to create a special Object that can store a matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {

  inverseMatrix <- NULL
  set <- function(matrix) {
    x <<- matrix
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) inverseMatrix <<- inv
  getinv <- function() inverseMatrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}



## Function that computes the inverse of a special object returned by makeCacheMatrix. If the inverse is already computed it returnse the cached inverse. Otherwise it calculates the inverse and sets the result in the makeCacheMatrix object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix <- x$getinv()
    if(!is.null(inverseMatrix)) {
      return(inverseMatrix)
    }
    mat <- x$get()
    inverseMatrix <- solve(mat, ...)
    x$setinv(inverseMatrix)
    inverseMatrix
}