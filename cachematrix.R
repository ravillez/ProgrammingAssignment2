# This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        # Caches matrix x
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        # Calls matrix x
        get <- function() x
        
        # Caches the inverse of matrix x calculated with the 'CacheSolve' function
        setInverse <- function(inverse) inv_matrix <<- inverse
        
        # Calls the inverse of matrix x calculated with the 'CacheSolve' function
        getInverse <- function() inv_matrix
        
        # Lists the four functions part of 'makeCacheMatrix
        list(set = set,
             get = get,
             setInverse = setInverse, 
             getInverse = getInverse)
}

# This function computes the inverse of the special matrix returned by 'makeCacheMatrix' above.
# If the inverse has already been calculated, then 'cacheSolve' retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_matrix <- x$getInverse()
        if (!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        matrix1 <- x$get()
        inv_matrix <- solve(matrix1)
        x$setInverse(inv_matrix)
        inv_matrix
}

# Example of how to use these two functions:
# Start with a <- makeCacheMatrix()
# Then, create and cache a matrix, e.g., a$set(matrix(1:4, 2, 2))
# You can test that this worked by calling the cached matrix using a$get()
# At this point, note that if you do a$setInverse() you'll get an error; if you do a$getInverse(), you'll get NULL.
# Now, calculate the inverse of a by doing cacheSolve(a). This will cache the inverse of matrix a.
# Now, if you do a$getInverse, you'll get the inverse of a.
# If you run cacheSolve(a) again, you'll get the inverse of a and a message saying 'getting cached data'. 

