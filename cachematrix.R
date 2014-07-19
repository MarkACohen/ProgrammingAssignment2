## This file contains code that wraps a matrix around a set 
## of data and functions that will calculate the inverse
## of the matrix and then cache it.  Subsequent calls to
## calculate the inverse will return the cached inverse.
## If the matrix is set to a new matrix, the cache is cleared.
##
## To test these classes you can execute the following:
##  m<-cbind(0:1, 1:0)
##  superM<-makeCacheMatrix(m)
##  cacheSolve(superM)
##  cacheSolve(superM)

## Returns a list backed by the matrix argument (x).  The list 
## contains functions that allow you to get/set the matrix, and 
## get/set the inverse of the matrix.  Use cacheSolve to 
## retrieve the inverse of the matrix that is contained in 
## the returned list.
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


## This function assumes that the argument x is a 'CacheMatrix' 
## that was created using the makeCacheMatrix function.  This 
## function returns the inverse of the matrix contained in the 
## specified 'CacheMatrix'.  The inverse will be cached, so that
## it is only calculated once.  Subsequent calls to this method
## with the same 'CacheMatrix' object will return the cached
## inverse value.  This function assumes that the matrix is 
## non singular and therefore has a valid inverse.
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
