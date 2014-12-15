## There are two functions defined here, makeCacheMatrix() and cacheSolve(). 
## makeCacheMatrix() performs the basic operations of the matrix
## whereas cacheSolve() returns the inverse of the matrix

# makeCacheMatrix() does the following operations - 
# (1) Initialization of the inverse of the matrix to NULL
# (2) create a new matrix
# (3) print the matrix
# (4) setting the inverse of the matrix 
# (5) printing the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                     # (1)
  set <- function(y) {                          # (2) 
    x <<- y
    m <<- NULL
  }
  get <- function() { x }                       # (3) 
  setinverse <- function(mean) { m <<- mean }   # (4)
  getinverse <- function() { m }                # (5) 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve() does the following operations - 
# (1) Calls the getinverse() function from makeCacheMatrix() function
# (2) Checks if the called matrix is a NULL, if its a NULL then 
#    (a) prints the matrix from the memory else
#    (b) calculates the inverse of the matrix using the solve() function and then prints it

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                           # (1) 
  if(!is.null(m)) {                             # (2)
    message("getting cached data")
    return(m)                                   # (2a)        
  }
  data <- x$get()
  m <- solve(data, ...)                         # (2b)
  x$setinverse(m)
  m
}
