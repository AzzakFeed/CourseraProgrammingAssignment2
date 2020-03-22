
#The following two functions allow to get the inverse of a matrix from the cache if applicable, or calculate and cache it. 
#This is useful in order to save time and not repeatedly do a costly operation.

# makeCacheMatrix creates a special “matrix” object that can cache a matrix inverse. It creates a list containing a function to :
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function returns the inverse of a matrix, using the cache if applicable. If the inverse has already been calculated, then
# it takes the value fron the cache instead of calculating it. Otherwise it does the calculation and caches it with the setinverse 
# function.

#cacheSolve assumes that the matrix is inversible.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Sample run

# A <- matrix(c(1,2,3,4),2,2)
# A
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# Target result :
#  solve(A)
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# First run : calculation
# m = makeCacheMatrix(A)
# cacheSolve(m)
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Second run : getting cached data
#  cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5