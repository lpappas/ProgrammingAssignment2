## The following 2 functions combined cache the inverse of a matrix.  This prevents unnecessary repetition of a costly 
## compuatation.


## The makeCacheMatrix function caches the inverse of a matrix.
## It creates a special "vector", which is really a list of the following functions: 
## 1. stores a matrix in cache
## 2. gets a matrix from cache
## 3. stores the inverse matrix in cache
## 4. get the inverse matrix from cache

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


## The cacheSolve function computes the inverse of a matrix and stores this inverse in cache.
## However, if this matrix is not new (has not changed from the cache value) and the inverse has already been calculated (the value is not NULL),
## then this function will retrieve the inverse from the cache and skip the computation.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, matrix_y, ...) {
        data <- x$get()                                   ## Retrieves the original matrix stored in cache
        m <- x$getinverse()                               ## Retrieves the inverse matrix stored in cache
        if (isTRUE(all.equal(data, matrix_y))){           ## IF the matrix supplied to this function matches the matrix stored in cache 
                if(!is.null(m)) {                         ## AND the inverse has already been calculated
                        message("getting cached data")    ## THEN simply return the cached inverse
                        return(m)
                }
        }else{
                message("new matrix")
                x$set(matrix_y)                           ## The matrix supplied to this function is new, so store it in cache
                data <- matrix_y
        }
        
        m <- solve(data, ...)                             ## Calcuate the inverse of the new or cached matrix
        x$setinverse(m)                                   ## Store the inverse matrix in cache
        m
}





