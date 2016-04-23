## Object to cache the calculated inverse of a matrix along with the original 
## matrix. Template provided by course tutors @ Coursera R Programming

## function returning a list of set and get functions for either the original 
## matrix or the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) {
    inverse <<- inv
  }
  getinverse <- function() {
    inverse
  }
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse
       )
  
}


## Function that returns the cached inverse matrix, and calculates it when
## not cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  # If there's already an inverse matrix cached
  if(!is.null(inverse)){
    message("getting cached inverse matrix")
    # Return the cached inverse matrix to parent
    return(inverse)
  }
  
  # INVARIANT: inverse == NULL
  # Calculate the inverse matrix now
  matrix <- x$get()
  # ASSUMPTION: matrix is an invertible square matrix
  inverse <- solve(matrix)
  # Cache the calculated inverse matrix
  x$setinverse(inverse)
  # And return the calculated inverse matrix to parent
  return(inverse)
  
}
