## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function, makeCacheMatrix creates a special “matrix” that can cache its inverse
##set the value of the matrix
##get the value of the matrix
##set the inverse of the matrix
##get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get<- function()x
  set_inverse<- function (solve_matrix) inv<<- solve_matrix
  get_inverse<- function () inv
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function
##This function, cacheSolve, computes the inverse of the special “matrix” 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$get_inverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data)
  x$set_inverse(inv)
  inv
}
