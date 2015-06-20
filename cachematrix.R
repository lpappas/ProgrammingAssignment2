## Lea Pappas
## Coursera - R Programming course
## June 19, 2015
## Purpose of the following 2 functions combined is to calculate and cache the inverse of a matrix.  This prevents unnecessary 
## repetition of the inverse matrix function which can be a costly computation.


## The makeCacheMatrix function provides the ability to cache a matrix and its inverse.
## It creates a special "vector", which is really a list of the following functions: 
## 1. stores a matrix in cache
## 2. gets a matrix from cache
## 3. stores the inverse matrix in cache
## 4. get the inverse matrix from cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {                                    ## 1. Stores a matrix in cache
                x <<- y
                m <<- NULL                                      ## Resets the inverse matrix value when a new matrix is cached
        }
        get <- function() x                                     ## 2. Gets a matrix from cache
        setinverse <- function(inverse) m <<- inverse           ## 3. Stores the inverse matrix in cache
        getinverse <- function() m                              ## 4. Gets the inverse matrix from cache   
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of a matrix and stores this inverse in cache.
## However, if the inverse has already been calculated (and the matrix has not changed),
## then this function will retrieve the inverse from the cache and skip the computation.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                               ## Retrieves the inverse matrix stored in cache
        if(!is.null(m)) {                                 ## If the value is not null, this means the original matrix 
                message("getting cached data")            ## has not changed and the inverse matrix is valid
                return(m)                                 ## Returns the cached inverse matrix
        }
 
        data <- x$get()                                   ## Retrieves the matrix stored in cache       
        m <- solve(data, ...)                             ## Calcuates the inverse of this matrix
        x$setinverse(m)                                   ## Stores the inverse matrix in cache
        m
}








