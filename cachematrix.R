
## This function will create a special type of matrix,
## in which its inverse is cached when calculated for the first time,
## and subsequence calculations will use the cached value instead of re-calculating it again

## usage:
##
## ...
## matrix <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 2)))
## matInv <- cacheSolve(matrix)
## ...
## 
## Inputs: 
##  x: input matrix
##
## Outputs:
##  list: list holind the methods to read and write the matrix data and its inverse
makeCacheMatrix <- function(x = matrix()) {
 
  # Stores the cached inverse
  inv <- NULL
  
  # Set's new values for the matrix
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  
  # Get's the martix values 
  get <- function() x
  
  # Set's the inverse of the matrix
  setinverse <- function(i = matrix) inv <<- i
    
  ## Get's the inverse of the matrix
  getinverse <- function() inv
  
  ## Returns a list with the methods to access the values of the matrix and its inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate a matrix inverse. If the inverse was already calculated, this function will return the cached inverse
## If the inverse was not calculated, it is and the results are cached for future reuse

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## Check if the inverse was already calculated
  inverse <- x$getinverse()
  
  ## First time inverse calculation ?
  
  if(!is.null(inverse)) { ## NO, return cache
      message("using cached inverse")
      return(inverse)
  }
  else {  ## YES, calculate and cache the inverse
    
      ## Get the matrix values
      mat <- x$get()
      
      ## Calculate its inverse
      matinv <- solve(mat)
    
      ## Cache the inverse
      x$setinverse(matinv)
    
      return(matinv)
  }
}
