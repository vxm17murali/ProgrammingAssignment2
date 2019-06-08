## Put comments here that give an overall description of what your
## functions do

# requires two functions first an instance object (makeCacheMatrix) then get inverse (cacheSolve) of a matrix
# makeCacheMatrix:  Create an instance object that holds the matrix and inverse
# cacheSolve: Solves the matrix with a new solution or an existing cache if input has not changed

## Write a short comment describing this function
# makeCacheMatrix:  Create an instance object that holds the matrix and inverse
# object properties: 
#    makeCacheMatrix$get()                      = get the matrix in this instance
#    makeCacheMatrix$set()                      = set the matrix in this instance
#    makeCacheMatrix$getinverse()               = get the inverse of the matrix
#    makeCacheMatrix$setinverse()               = set the inverse of the matrix

# Must first create an instance of a matrix mx by: 
# example matrix
#                    mx <- matrix (1:4,2,2)
# instance object called instanceM
#                   instanceM <-makeCacheMatrix(mx)

makeCacheMatrix <- function(x = matrix()) {

  # set inverse to null when object is instantized
  inversem <- NULL
  
  # when matrix is changed
  # assigns input argument to x in parent environment
  # resets inverse to null as it has not yet been calculated for the new matrix
  set <- function(y) {
    x <<- y
    inversem <<- NULL
  }
  
  # returns the matrix that is currently in this instance
  get <- function() x

  # to set the value of inverse in the instantized object
  setinverse <- function(meanmatrix) inversem <<- meanmatrix

  # to get the value of the inverse from this instance
  getinverse <- function() inversem
  
  # allow global environment access to instantized variables
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  

}


## Write a short comment describing this function

# cacheSolve: Solves the matrix with a new solution or an existing cache if input has not changed
# called by
#                cacheSolve(instanceM)
# where instanceM has been created as per above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


  ## Return a matrix that is the inverse of 'x'
  
  # CHECK IF MATRIX IS INVERSIBLE
  # not required for this project
  
  # See if inverse already exists in the cached instance
  inversem <- x$getinverse()
  
  # If inverse already exists, return it without having to recalculate
  if(!is.null(inversem)) {
    message("getting cached matrix inverse")
    return(inversem)
  }
  
  # Get the actual matrix stored in cache associated with this instance
  data <- x$get()
  
  # Calculate the inverse
  inversem <- solve(data)
  
  # Store the inverse in the cached instance
  x$setinverse(inversem)
  
  # return the inverse 
  inversem

}

