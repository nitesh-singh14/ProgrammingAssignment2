##This function creates a special matrix object that can 
##cache its inverse.It also gets and sets the value of the matrix
##and its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) inv <<- Inv
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

##This function computes the inverse of the special matrix returned by makeCacheMatrix. 
##If the inverse has already been calculated and the matrix has not changed,
##cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr, ...)
        x$setInv(inv)
        inv
}
