
##This function "Function makeCacheMatrix" gets a matrix as an input, set the value of the matrix,
#get the value of the matrix and set the inverse Matrix and get the inverse Matrix.  

makeCacheMatrix <- function(x = matrix()) {
          invMatrix <- NULL
          
               
                  setMatrix <- function(y) {
                            x <<- y
                            invMatrix <<- NULL
                          }
                  
                          getMatrix <- function() x                              #Get the value of the Matrix
                          setInverse <- function(inverse) invMatrix <<- inverse  #Set the value of the invertible matrix
                          getInverse <- function() invMatrix                     #Get the value of the invertible matrix
                          list(setMatrix = setMatrix, getMatrix = getMatrix,
                          setInverse = setInverse, getInverse = getInverse)
                        
}


## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 
        # input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
        # In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
        # and set the invertible  matrix by using the solve function.

cacheSolve <- function(x, ...) {
          
                #Get the value of invertible matrix from the makeCacheMatrix function.
                          invMatrix <- x$getInverse()
                        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
                                 message("Getting Cached Invertible Matrix")   #Get Cached Invertible Matrix 
                                  return(invMatrix)                             
                                }
                          
                        #if value of the invertible matrix is NULL then  
                                MatrixData <- x$getMatrix()                     #Get the original Matrix Data 
                                invMatrix <- solve(MatrixData, ...)             #Solve function to inverse the matrix
                                x$setInverse(invMatrix)                         #Set the invertible matrix 
                                return(invMatrix)                               
                               
}


##Test above functionality by passing matrix in that.

Test1Matrix <- matrix(1:4,2,2)
Test1Matrix
CacheMatrix <- makeCacheMatrix(Test1Matrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()
cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)