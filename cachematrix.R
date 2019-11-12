## Put comments here that give an overall description of what your
## functions do

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
cacheSolve <- function(x, ...) {
#get the value of the invertible matrix from the makeCacheMatrix function
  j <- x$getInverse()
  if(!is.null(j)){ #if inverse matrix is not NULL
  message("getting cached data")#Type message: Getting Cached Invertible Matrix 
  return(j) #return the invertible matrix
  }
  #if value of the invertible matrix is NULL then 
  mat <- x$get()  #get the original Matrix Data
  j <- solve(mat,...) #use solve function to inverse the matrix
  x$setInverse(j) #set the invertible matrix j
  j #return the invertible matrix j
}
