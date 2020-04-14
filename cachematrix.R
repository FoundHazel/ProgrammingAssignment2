## makeCacheMatrix gets a matrix as an input, set the value of the matrix,
#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object can cache its own object. 
## The functions outlined are for the completion of Assignment2 from course Coursera: R Programming  

## makeCacheMatrix will create a matrix as as an input (please see above for further description)


makeCacheMatrix <- function(x = matrix()) {     ##set argument to be makeCacheMatrix
   
    invMatrix  <- NULL                          ## set inv as NULL; will hold value of matrix inverse 
    setMatrix <- function(y) {                  ##set the value of the matrix
      
         x<<-y                                   ## this is the value of the matrix within the parent environment 
        
        invMatrix <<- NULL                      ## This states that if there is a new matrix, then the invMatrix will be set as NULL
    }
   
        getMatrix <- function() x                   ## this defines the get function, which returns the output of the argument function 
                                                        #from above 
   
        setInverse <- function(inverse) invMatrix <<- inverse       ##this assigns the value of the inverse in the parent environment 
    
        getInverse <- function() invMatrix                          ##this outputs the value of inverse where it is called
    
        list(setMatrix = setMatrix, getMatrix = getMatrix,          ##The last lines are reference to the functions with the $ operator 
         setInverse = setInverse,getInverse = getInverse)
}


## This fucntion returns the inverse of the matrix outlined above.
#NOTE: If the inverse has already been calculated and the matrix has not changed, the this function will retrieve the inverse from the
#cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is inverse of the makeCacheMatrix function from above) 
    invMatrix <- x$getInverse()
        
    if(!is.null(invMatrix)) {                                   ##if the inverse matrix is NOT NULL 
        message("getting Cached Invertible Matrix")
        return(invMatrix)                                       ##command to return the inverted matrix
    }
    
    MatrixData <- x$getMatrix()                                 ##command to retrieve original matrix data
        
    invMatrix <- solve(MatrixData, ...)
        
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
