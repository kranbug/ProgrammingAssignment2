## This R file contains two functions 

## The funtion "makeCacheMatrix" is designed to create two global variables,
## variable "origMatrix" to cache a Matrix and variable "InvMatrix" to 
## cache the Inverse of the same Matrix. 
## This function returns a list of four functions, two of which are designed
## to set and get value of "origMatrix" variable and the other two are 
## desgined to set and get the value of "InvMatrix" variable

makeCacheMatrix <- function(x = matrix()) {
  
  ## Cache the matrix supplied to the function into "origMatrix" variable
  origMatrix <<- x 
  ## set "invMatrix" to NULL as Inverse is not computed at this time
  invMatrix <- NULL
  
  ## This function is designed to assign a matrix directly to "origMatrix"
  setOrigMatrix <- function(mat) {
    ## Cache the matrix supplied to the function into "origMatrix" variable
    origMatrix <<- mat
    ## set "invMatrix" to NULL as Inverse is not computed at this time
    invMatrix <<- NULL
  }
  
  ## This function is designed to return "origMatrix"
  getOrigMatrix <- function() origMatrix
  
  ## This function is designed to cache a supplied inverse matrix 
  ## directly into "invMatrix" variable
  setInvMatrix <- function(invMat) invMatrix <<- invMat
  
  ## This function is designed to return "invMatrix"
  getInvMatrix <- function() invMatrix
  
  ## Return the list containing four functions, two each to manipulate 
  ## "origMatrix" and "invMatrix" variables respectively
  list(setOrigMatrix = setOrigMatrix, getOrigMatrix = getOrigMatrix,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
  
} #End of makeCacheMatrix


## The function "cacheSolve" takes a "Cached Vector" and checks if it  
## contains the inverse of original matrix. 
## If the inverse exist (and the original matrix has not changed), 
## this function retrieves the inverse from the Cache.
## If the inverse does not exist, it computes and inserts the inverse
## back into the supplied "Cached Vector"

cacheSolve <- function(x, ...) {
  
  ## Get "invMatrix" variable of the supplied Cached vector
  invMatrix <- x$getInvMatrix()
  
  ## Check if "invMatrix" is not Null to verify if inverse is cached. 
  ## If it is cached then return the cached value.
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  
  ## If inverse does not exist then get the original matrix using 
  ## "getOrigMatrix" function
  origMatrix1 <- x$getOrigMatrix()
  ## Solve the original matrix to compute its inverse
  invMatrix <- solve(origMatrix1, ...)
  ## Set the computed inverse into the supplied Cached Vector
  x$setInvMatrix(invMatrix)
  ## Return the Inverted matrix
  invMatrix
  
} #End of cacheSolve
