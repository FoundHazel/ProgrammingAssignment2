## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix shall create a matrix oject that can inverse itself

makeCacheMatrix <- function(x = matrix()) {
    invMatrix  <- NULL
    setMatrix <- function(y) {
        x<<-y
        invMatrix <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) invMatrix <<- inverse
    getInverse <- function() invMatrix 
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse,getInverse = getInverse)
}


##cachesolve shall check if the the inverse for the matrix has already been
# calculated if so then it shall return from cache
# else shall calculate the inverse using the solve function
# and sets it in the cache using setinv function

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'  
  invMatrix <- x$getInverse()
  
  # if the inverse has already been calculated
    if(!is.null(invMatrix)) {
        message("getting Cached Invertible Matrix")
        return(invMatrix)
    }
    
    MatrixData <- x$getMatrix()
    invMatrix <- solve(MatrixData, ...)
  
  # sets the value of the inverse in the cache
    x$setInverse(invMatrix)
  
    return(invMatrix)
}


##Testing
Test_Matrix<-matrix(1:4, 2, 2)
Test_Matrix

CacheMatrix_Test_1 <- makeCacheMatrix(Test_Matrix)
CacheMatrix_Test_1$getMatrix()
CacheMatrix_Test_1$getInverse()

cacheSolve(CacheMatrix_Test_1)
cacheSolve(CacheMatrix_Test_1)
