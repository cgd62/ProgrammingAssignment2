##
## The following two functions make it possible to cache matrix inversion
## results so that the computational workload for the matrix inversion is
## not repeaated on subsequent calls to cacheSolve()..
##

## makeCacheMatrix() creates a special "matrix" which is a list containing
## functions (returning matrix) to set the value of the matrix, get the
## value of the matrix, set the value of the cached matrix inverse,  
## and get the valued of the cached matrix inverse, inv.
##
## usage:  mySpecMatrix <- makeCacheMatrix(myMatrix)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(i) inv <<- i
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve takes a return value from makeCacheMatrix and first checks
## to see if a cached matrix inversion in inv can be returned.  If not
## then it checks to make sure an inverse for the matrix exists, by checking
## that the determinant is non-zero.  If so, it computes the inverse using 
## solve(), stores it in the cached inv, and returns the inverse matrix, inv.
## If the determinant is zero, then the cached inv is set to NULL and returned
## and a warning message is issued.
##
## usage with example of repeated invocations:
##         myInverse <- cacheSolve(mySpecMatrix)  # uses solve()
##         myInverse <- cacheSolve(mySpecMatrix)  # uses cached inv

cacheSolve <- function(x, ...) {
        mat <- x$get()
        inv <- x$getinv()

        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        if(det(mat) != 0) {
                inv <- solve(mat)
        } else {
                inv <- NULL
                message("inverse cannot be calculated, matrix not invertible")
        }
        x$setinv(inv)
        inv
}
