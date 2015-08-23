## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Heavily inspired on the makeVector example, the makeCacheMatrix
#take as an argument a matrix, computes the inverse. Then it returns a list
# with the different features of the matri and the computation.

makeCacheMatrix <- function(X = matrix()) {
  
      inv <- NULL # This variable will cache the invert
  
        set <- function(Y) {
            X <<- Y
            inv <<- NULL
        }
        
        get <- function() X
        setinv <- function(Inverse) inv <<- solve #Replace mean() with solve()
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        

}


## Write a short comment describing this function
#Again this function is heavily inspired by the sample, with very
#little differences except that m is replaced by inv.


cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
        inv <- X$getinv()
        
        if (!is.null(inv)) #Test if the inverse exists.          {
          
          message ("Getting cache data - inverse already previously computed")
          
          return (inv)
        }
        
        data <- X$get()
        inv <- solve(data, ...)
        X$setinv(inv)
        inv
}
