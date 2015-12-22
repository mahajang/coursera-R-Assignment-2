## Two functions are defined: makeCacheMatrix & cacheSolve. Example of two functions are given below:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

## Test the functions using dummy matrix created

##Create a matrix:
my_matrix <- makeCacheMatrix(matrix(6:9, 2, 2))

##Test the function with get() and getinverse() matrix
my_matrix$get() 
my_matrix$getInverse()

## Test the cacheSolve() function using the my_matrix data created
cacheSolve(my_matrix)

## Test the functions using another matrix with similar commands:
my_matrix$set(matrix(c(7, 8, 9, 10), 2, 2))
my_matrix$get()

my_matrix$getInverse()

cacheSolve(my_matrix)
