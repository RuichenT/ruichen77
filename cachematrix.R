## Put comments here that give an overall description of what your
## functions do
#This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
#can cache its own object.
## Write a short comment describing this function

#take the matrix as an input
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
 #set the value of the Matrix
  set <- function(y){   
  x <<- y
  j <<- NULL
  }
  get <- function()x  #get the value of the Matrix
  setInverse <- function(inverse) j <<- inverse  #set the value of the invertible matrix j
  getInverse <- function() j   #get the value of the invertible matrix j
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}


## Write a short comment describing this function
## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
#after running the code 1st time), it returns a message  "Getting Cached Invertible Matrix" 
#and the cached object
cacheSolve <- function(x, ...) {
#get the value of the invertible matrix from the makeCacheMatrix function
  j <- x$getInverse()
  if(!is.null(j)){ #if inverse matrix is not NULL
  message("Getting Cached Invertible Matrix")#Type message: Getting Cached Invertible Matrix 
  return(j) #return the invertible matrix
  }
  #if value of the invertible matrix is NULL then 
  mat <- x$get()  #get the original Matrix Data
  j <- solve(mat,...) #use solve function to inverse the matrix
  x$setInverse(j) #set the invertible matrix j
  j #return the invertible matrix j
}
