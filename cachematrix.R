## Objective: to cache the inverse of a matrix through 2 functions
  ## 1/ makeCacheMatrix will create a 'special vector' of functions all related
  ## to the considered matrix, to be kept in cache.
  ## 2/ cacheSolve will either calculate the inverse or retrieve it by using
  ## the 'special vector' of functions from makeCacheMatrix.

  ## All assignment is heavily inspired from the example for the general structure,
  ## with only an adaptation to the dimensions (i.e. matrix rather than vector)
  ## and solving the inverse.

## Status update from internal test: No crash when running the 2 functions
## as cacheSolve(makeCacheMatrix(cbind(c(1,2),c(3,4)))). Returned solutions is exactly
## the inverse obtained from solve().


## Comments on makeCacheMatrix:
  ## Heavily inspired on the makeVector example, the makeCacheMatrix
  ## take as an argument a matrix, computes the inverse. Then it returns a list
  ## with the different features of the matri and the computation.

makeCacheMatrix <- function(X = matrix()) {
  
      inv <- NULL # This variable will cache the inverse
  
        set <- function(Y) {
            X <<- Y ## Use of lexical scoping with <<- operator
            inv <<- NULL ##Idem
        }
        
        get <- function() X
        setinv <- function(Inverse) inv <<- solve #Replaced mean() with solve() for the purpose of the assingment
        getinv <- function() inv
        
      list(set = set, get = get, setinv = setinv, getinv = getinv) # Creation of the list of functions & data

}


## Comments on cacheSolve:
  ###Again this function is heavily inspired by the sample, with very
  ###little differences emYxcept that m is replaced by inv.


cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'
  
        inv <- X$getinv() ## Fetch the variable inv
        
        if (!is.null(inv))  {#Test if the inverse exists (i.e. not NULL or NA)        
          
            message ("Getting cache data - inverse already previously computed")
          
            return (inv) # Return the already calculated matrix's inverse
        }
        
        
        # The below lines handle matrix's inverse calculation as it was not cached
        data <- X$get() ## Fetch the matrix whose invers was not previously calculated.
        
        inv <- solve(data, ...) ## Calculating the inverse.
        
        X$setinv(inv)
        
        inv ## Return the inverse
        
}

