## This is my submission for the week3 data science submission.
## The assignment is pretty similar to the example, only with a different type of calculation.


## This first function is creating the matrix, which will be cashed. 
##Very similar to the example function, yet not creating a vector, but a matrix

makeCacheMatrix <- function(x = matrix()) {
                      
        minv <- NULL  ## This is where we store the inverse of the matrix (minv). 
                      ## We set it to NULL if not calculated, see 'message' later
        
                set <- function(y) {
                        x <<- y
                        minv <<- NULL ## just like in the example, we define set 
        }
        
        get <- function() x ## we go get the input matrix
        setminv <- function(inv) minv <<- inv ## we set the inversed matrix to inv
        getminv <- function() minv ## we return the inversed matrix
        
        list(set = set, get = get, setminv = setminv, getminv = getminv) 
        ## we create the list of the 4 functions defined above

}


## This function will calculate the inverse of the matrix, or fetch it from the cache.

cacheSolve <- function(x, ...) {
        
        f <- x$getminv() ## we get the inversed matrix
                 if(!is.null(f)) { ## if the inversion result is there (so not NULL)
                        message("using cached data")
                        return(f) ## use the cached inverse matrix
        }
        data <- x$get() ## if it's not calculated yet, we get the original matrix
        f <- solve(data) ## solve the matrix
        x$setminv(f)
        f ## print the output of the solved (inverse) matrix. 
        ## The previous 4 lines of code are skipped when the inverse matrix is in cache.
                
}

##the first time we solve the matrix, it will be calculated and stored to cache (no message)
##the second time the matrix is called to be solved, it will be fetched from cache. No calculations.