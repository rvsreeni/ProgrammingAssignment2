## Programming assignment #2

## This function creates a Cache Matrix object, with methods (set, get,
## setinverse, getinverse), and the matrix, its inverse as properties

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m	
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks, if the inverse is already present, 
## and whether the matrix has changed. If the inverse is present,
## and the matrix has Not changed, then it gets the inverse from cache,
## else it computes the inverse. It uses the matequal function, 
## to determine if the matrix has changed or not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  matequal <- function(c, d)
		is.matrix(c) && is.matrix(d) && dim(c) == dim(d) && all(c == d)
        n <- x$getinverse()
	  data <- x$get()
	  datan <- solve(data)
        if (!is.null(n)) {
           if (matequal(n, datan)) { 
                message("getting cached data")
                return(n)	}
        }
        m <- solve(data, ...)
        x$setinverse(m)	
        m
}
