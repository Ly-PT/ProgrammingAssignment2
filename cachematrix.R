## A pair of functions that cache the inverse of a matrix.An example test call
## is included at the end to facilitate the testing. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set_matrix <- function(m) {
                x <<- m
                cache <<- NULL
        }
        get_matrix <- function() x
        
        set_inverse <- function(inverse) cache <<- inverse
        get_inverse <- function() cache
        
        list(set_matrix = set_matrix, get_matrix = get_matrix,
             set_inverse = set_inverse, get_inverse = get_inverse)
}


## cachSolve computes the inverse of the special "matrix" returned by the  
## makeCacheMatrix, either by retrieving cached data or by computing new and
## then set_inverse with the computed inverse. 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        cache <- x$get_inverse()
        
        ## Retrieve the inverse from the cache if the inverse has been 
        ## calculated (and the matrix has not changed). Otherwise, compute
        ## the new inverse, store the result to cache and set_inverse to cache.
        if(!is.null(cache)) {
                message("Get cached data!")
                
        } else {
                matrix <- x$get_matrix()
                cache <- solve(matrix, ...)
                x$set_inverse(cache)
                message("Compute the inverse of the matrix!")
        }
        cache
        
}

## Example test case. Note, a invertible matrix needs to be square.
## Generate a invertible matrix. Print the output.
matrix <- matrix(rnorm(9), 3, 3)
print(matrix)
## A special "matrix" object that can cache its inverse. Print the output. 
x <- makeCacheMatrix(matrix)
print(x)
## Compute the inverse of the matrix for the first time. Print the output
print(cacheSolve(x))
## Retrieve from cached data of the already computed inverse.Print the output
print(cacheSolve(x))
## Change the matrix and print the modified matrix.
x$set_matrix(matrix(rnorm(16), 4, 4))
print(x)
## Compute the inverse of the modified matrix.Print the output
print(cacheSolve(x))

