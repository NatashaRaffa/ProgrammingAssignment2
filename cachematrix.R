## These functions allow the user to cache the inverse of a matrix so that it
## doesn't need to be computed every time; only when there is new data


## makeCacheMatrix makes a list that contains a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inversem) inv <<- inversem
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##The cacheSolve function calculates the inverse of the special matrix x.
#However, it first checks to see if the inverse has already been calculated.
#If so, it skips the calculation and gets the cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  ##check if the inverse of x has already been calculated. If so, return the value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #If not, calculate the inverse of x
  else
  {
    data <- x$get()
    inv <- solve(data)
    ##Set the inverse matrix in the cache using setinv
    x$setinverse(inv)
    return(inv)
  }
  
  
}
