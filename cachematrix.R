## The overall aim of function is to compute inverse of a matrix once and store for retrival as may be needed later


## makeCacheMatrix takes matrix as an input and converts it to a list of 4 matrixes
## First element of the list contains the matrix passed as argument to the makeCacheMatrix function
## This argument is also used to assign a new value of matrix to the function. Whenever its done, the inverse in cache
## gets initialised to zero
## Second element of the list enables looking at the current value of the matrix. This allows function to check the
## last value passed passed to makeCacheMatrix function
## Third element of the list is used by cacheSolve to refresh the value of the inverse as may be needed due to change in matrix
## fourth element of the function is used to access the inverse value stored in cache.


makeCacheMatrix <- function(x = matrix()){
  
  inver <- matrix(0,nrow = dim(x)[1],dim(x)[2])
  
  set <- function(y){
    x <<- y
    inver <<- matrix(0,nrow = dim(y)[1],dim(y)[2])
  }
  
  get <- function() x
  
  setinver <- function(value) inver <<- value
  
  getinver <- function() inver
  
  list(set = set, get = get, setinver = setinver, getinver = getinver)
  
}

## CacheSolve checks if the inverse value stored in MakeCacheMatrix is 0 and if so, computes the value of inverse
## and passes it to MakeCacheMatrix for storage in cache

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(all(inver)!=0){
    message("getting cached data")
    return(inver)
  }
  
  data <- x$get()
  inver <- solve(data)
  x$setinver(inver)
  
  inver
  ## Return a matrix that is the inverse of 'x'
}
