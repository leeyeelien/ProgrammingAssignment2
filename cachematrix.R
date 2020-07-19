## Put comments here that give an overall description of what your
## functions do

## Creates a matrix and caches its inverse

makeCacheMatrix <- function(mat = matrix()) {
  mat_inv <- NULL
  setMatrix <- function(m) {
    mat <<- m
    mat_inv <<- NULL
  }
  getMatrix <- function() mat
  
  setMatrixInv <- function(m_inv) mat_inv <<- m_inv
  getMatrixInv <- function() mat_inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setMatrixInv = setMatrixInv,
       getMatrixInv = getMatrixInv)

}


## Computes the inverse of a matrix and caches it using makeCacheMatrix function

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  mat_inv <- m$getMatrixInv()
  if(!is.null(mat_inv)) {
    message("getting cached inverse of matrix")
    return(mat_inv)
  }
  mat <- m$getMatrix()
  mat_inv <- solve(mat)
  m$setMatrixInv(mat_inv)
  mat_inv
}
