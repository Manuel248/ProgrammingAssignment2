## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



# This is a program to save a matrix x as part of a list of function calls,together with it's inverse 
# so it can be accessed from an external function. --> function makeCacheMatrix(x)





makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y){
    
    x <<- y   
    
    i <<- NULL 
  }
  get <- function() x       
  
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
  
  
}


## Write a short comment describing this function

# The second function cacheSolve(x) makes a call to check if an inverse matrix already exists.
# If the inverse doesn't exist already, it will compute the inverse, hand it over to the function setInverse(inverse) 
# available by the function makeCacheMatrix(x) to store the result  and then it returns 
# the inverse Matrix as output 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  inverse <- x$getInverse()
  if ( !is.null(inverse)){
    
    message("getting cached data")
    return(inverse)        
  }
 
  matrix <- x$get()
  
  #compute inverse  : 
  
  inverse <- solve(matrix,...)
  
  # send inverse to makeCacheMatrix() : 
  x$setInverse(inverse)
  
  inverse
  
  
}

