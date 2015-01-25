##Programming Assignment 2: Lexical Scoping
##This file includes to functions makeCacheMatrix and cacheSolve
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initializes inverse matrix in current enviroment
  set <- function(y){
    x <<- y #resets x in parent enviroment to value of y in current enviroment
    inv <<- NULL #initializes inverse matrix in parent env
  }
  #returns x (a matrix)
  get <- function() x
  #calculates inverse, and assigns to inv in parent enviroment
  setinv <- function(solve) inv <<- solve
  #returns the value of inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinv() #retrieve the inverse matrix from cache
  if (!is.null(inv)) { #check to see if it exists
    message("getting cached data")
    return(inv) #return value
  }
  data <- x$get() #retrieve the input matrix
  inv<-solve(data,...) #solve for the inverse
  x$setinv(inv) #add to list cache
  inv #return value
}

