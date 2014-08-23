## functions to create matrix, and create and cache the inverse


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL  ## create variable to record inverse
        
        set <- function(y) { ## to set value of matrix and reset inverse
                x <<- matrix(y)
                m <<- NULL
        }
        
        get <- function() x ## to return value of matrix
        
        setInverse <- function(inverse) m <<- inverse ## to record the inverse matrix 
        
        getInverse<- function() m ## to return the inverse of matrix
        
        ## return all functions trhough list
        list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
        
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get the value of the inverse for the specified object
        m <- x$getInverse()
        ## test if it s null of it contains a value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## get the value of the matrix
        data <- x$get()
        ## inverse the matrix
        m <- solve(data, ...)
        
        x$setInverse(m)
        
        m
}
