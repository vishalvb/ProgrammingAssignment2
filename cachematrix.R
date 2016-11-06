## Put comments here that give an overall description of what your
## functions do
#There are two functions makeCacheMatrix and cacheSolve respectively. 
#These functions together describes the use of cache. When matrix or any variable is small then it is not that effective
#But if we have variable and its computations requires some value to be used frequently,then cache is very effective and it reduces overall computation time


## Write a short comment describing this function
#makeCacheMatrix function is creates a matrix. setinverse is used to set the value of inverse of matrix
#getinverse is used to return the value of inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(matrix_inverse) inverse_matrix <<- matrix_inverse
  getinverse <- function() inverse_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
  
}


## Write a short comment describing this function
#cacheSolve calculates inverse of matrix obtained by above function. If inverse 
#is already calculated, it returns the inverse from the cache. 
#Else it will do the computation, calculates the inverse and stores the value of inverse via setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data_matrix <- x$get()
  inverse_matrix <- solve(data_matrix, ...)
  x$setmean(inverse_matrix)
  inverse_matrix
  }
