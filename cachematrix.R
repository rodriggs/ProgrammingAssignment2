## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Below are two functions that are used to create a special object that stores a matrix and 
## cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix
## 4. get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
## Initially assigning 'NULL' to inverse
  inverse <- NULL 					
  set_matrix <- function(y) {			
    
## Setting the matrix 'x'
    x <<- y 					
    inverse <<- NULL
  }

  ## Returning matrix 'x'
  get_matrix <- function() x 	
  
  ## Cache the value of the inverse
  set_inverse <- function(solve) inverse <<- solve 	 
  
  ## Returning inverse
  get_inverse <- function() inverse 			
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {		
  
  ## Assigning the inverse
  inverse <- x$get_inverse()				
  
  ## Checking if inverse already present.
  if(!is.null(inverse)) {					
    message("getting cached data")			
    return(inverse)
  }
  
  ## Get matrix
  data <- x$get_matrix()			
  
  ## Use solve() to compute inverse
  inverse <- solve(data, ...)				
  
  ## Caches the inverse
  x$set_inverse(inverse)					
  
  ## Returns inverse
  inverse 						
}