## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it repeatedly 
## These functions demonstrate the way the inverse of a matrix can be cached.

#creates a special matrix object which can chache its matrix value
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL ## make inverse value null for freshly created matrix
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  

}


## calcuate the inverse of a matrix. If the inverse has already been calculated
## previously, it will return the cached value. Otherwise, a fresh value is computed and returned after being cached

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if (!is.null(inv)) { ## cached value found. return it
    message("Already computed. Returning cached value.")
    return(inv)
  }
  message("Returning freshly computed value.")  
  mat <- x$get()
  inv <- solve(mat, ...)  ## compute fresh inverse
  
  x$setInverse(inv)   ## cache the computed inverse
  inv

}


## test run
#> test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#> cacheSolve(test_matrix)
#Returning freshly computed value.
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(test_matrix)
#Already computed. Returning cached value.
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> 
