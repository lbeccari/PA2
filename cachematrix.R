## Put comments here that give an overall description of what your
## functions do

## this function generates a matrix, calculates the its inverse matrix and stores it in the cache.
## 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {  #generate the matrix in the cached encviroment, creates an empty inv matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function() inv <<- solve(x) #calculate the inv
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}

        
## This function verifies if the solved matrix of x is present in the cache. If YES 
## it retrieves it. If FALSE it calculates the inverse matrix of x


cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv) ##return the inv matrix stored in the cache 
        }
        mat <- x$get()
        inv <- solve(mat) ##calculate the inv matrix of x if it was not stored in the cache
        x$setInv(inv)
        inv
}