## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
    # 'private' data member representing the matrix inverse...
    inv <- NULL

    # methods to get and set the inverse for this matrix...
    getInv <- function()
    {
        inv    
    }
    setInv <- function(i)
    {
        inv <<- i
    }
        
    # methods to get and set the matrix...
    getMatrix <- function()
    {
        x
    }
    setMatrix <- function(y) 
    {
        x <<- y
        
        # clear the inv when we change the matrix...
        inv <<- NULL 
    }
    
    # build a list that consits of the methods that operate
    # on our new matrix type...
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
    # check to see if the matrix already has its inverse cached...
    inv <- x$getInv()
    
    # if the inverse is cached, return it...
    if (!is.null(inv))
    {
        message("inv already cached!")
        return (inv)
    }
    # otherwise calculate it, store it, and return it...
    else
    {        
        message("inv not cached, calculating...")
        
        mat <- x$getMatrix()
        inv <- solve(mat)
        x$setInv(inv)
        
        return(inv)
    }
}
