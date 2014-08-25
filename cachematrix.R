## Functions that enables caching the inverse of a matrix

## Creates a special matrix to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse of a matrix
  im<-NULL
  ## Set the matrix
  set<-function(y){
    x<<-y
    im<<-NULL
  }
  ## Get the  matrix
  get<-function() x
  ## Set the inverse of the matrix
  setmatrix<-function(solve) im<<- solve
  ## Get the inverse of the matrix
  getmatrix<-function() im
  ## List the methods 
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Returns the inverse of a matrix in an efficient manner by using cache.
cacheSolve <- function(x, ...) {
  ## Get the inverse matrix
  im<-x$getmatrix()
  ## If found then return the inverse of the matrix
  if(!is.null(im)){
    message("Getting data from cache")
    return(im)
  }
  ## Since there was no cache version of the inverse matrix, we first get the matrix
  matrix<-x$get()
  ## Calculate the inverse of the matrix
  im<-solve(matrix, ...)
  ## Set the inverse of the matrix for future use
  x$setmatrix(im)
  ## Return the inverse of the matrix
  im
}
## Test the solution
## A <- matrix(c(1,2,3,4), nr=2, nc=2)
## solve(A)
## A1 <- makeCacheMatrix(A)
## cacheSolve(A1)
