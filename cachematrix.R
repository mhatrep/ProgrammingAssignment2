# Usage
# matInv = makeCacheMatrix(mat) where 'mat' is invertible matrix.
# cacheSolve(matInv)
# cacheSolve(matInv) (call again)

# Matrix inversion is CPU intensive calculation. Caching offers performance 
# boost for the inverse operation. makeCacheMatrix performs the steps below.
# 1) set value of matrix
# 2) get value of matrix
# 3) set value of inverse of  matrix
# 4) get value of inverse of  matrix

makeCacheMatrix <- function(inputMatrix = matrix()) 
{
    inverseMatrix <- NULL
    set <- function(input) 
	{
        inputMatrix <<- input
        inverseMatrix <<- NULL
    }
    get <- function() inputMatrix
    setinverse <- function(inverse) inverseMatrix <<- inverse
    getinverse <- function() inverseMatrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##################################################################################
# Note - cacheSolve assumes the matrix is invertible
# cacheSolve returns inverse of matrix. When operation is triggered, it checks if
# the inverse is already calculated. If yes, return the result. If not, it calculates 
# the inverse and stores the value in cache

cacheSolve <- function(inputMatrix, ...) 
{
    inverseMatrix <- inputMatrix$getinverse()
    if(!is.null(inverseMatrix)) 
	{
        message("Retrieving cached data .....")
        return(inverseMatrix)
    }
    tempMat <- inputMatrix$get()
    inverseMatrix <- solve(tempMat)
    inputMatrix$setinverse(inverseMatrix)
    inverseMatrix
}
