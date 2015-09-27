## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                                   #sets the value of s to NULL --> provides a default if cacheSolve has not been used yet
  y <- NULL                                   #sets the value of y to NULL --> provides a default if cacheSolve has not been used yet
  setmatrix <- function(y) {                  #sets the value of the matrix
    x <<- y                                   #caches the matrix in order that cacheSolve can check whether it has changed
    s <<- NULL                                #sets the value of s to NULL
  }
  getmatrix <- function() x                   #defines the getmatrix function
  setinverse <- function(solve) s <<- solve   #defines inverse function solve
  getinverse<- function() s                   #defines get inverse function
  list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)  #creates a list to store the functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  s <- x$getinverse()                         #if an inverse has already been contucted this will get it    
  if(!is.null(s)) {                           #checks if cacheSolve has been run before
    message("getting cached data")            #if cacheSolve already run, it will return above the matrix output the text "getting cached data"
    return(s)                                 #returns the inverse
  }
  y<- x$getmatrix()                           #runs the getmatrix function to get the value of the input matrix
  x$setmatrix(y)                              #runs the setmatrix function to cache it
  s <- solve(y, ...)                          #computes the inverse value of the input matrix
  x$setinverse(s)                             #runs the setinverse function 
  s                                           #returns the inverse
}
