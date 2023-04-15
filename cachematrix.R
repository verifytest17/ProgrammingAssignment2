
## This function assigns the value of solve function ie inverse of matrix to 'inv' variable 
## setinv variable have the inverse matrix
## getinv variable retrieves the  value of variable 'inv'
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {              ##set the value of matrix
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve ##set the value of inverse matrix ie solve
      getinv <- function() inv ## get the value of variable inv which have the inverse matrix
      list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## gets the cashed data of inverse matrix if it exists else inverses the matrix using solve function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x' matrix
      inv <- x$getinv()
      if(!is.null(inv)) {             ## checking if cached data present
        message("getting cached data")
        return(inv)                   ## if cached data (inversed matrix) exist then return it
      }
      data <- x$get()
      inv <- solve(data, ...)         ## if cached dats doesnot exist then calculate inverse matrix
      x$setinv(inv)
      inv
  
  
}
