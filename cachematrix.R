## The below functions work together to calculate an inverse of a matrix or if
## the inverse is already calculated before will retrieve the cached inverse matrix.

## makeCacheMatrix results in a vector containing a list of 4 functions (set(),
## get(),setinverse(),getinverse()) and stores the input matrix (x) and contains 
## an object for the inverse matrix (inv).
## It will basically serve as a parent environment for the next function cacheSolve.
## To use this function give the definition of the matrix as input and write it 
## to an object: myMatrix <- makeCacheMatrix(matrix(1:4,2,2))

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix (x) cached in the environment of 
## makeCacheMatrix. This is the parent environment of the functions used in cacheSolve.
## First it checks whether the inverse has been cached and if so it will return 
## the cached inversed matrix.
## If there is no cached object the original matrix is loaded and the inverse is 
## calculated and it is set in the parent environment (to be used as a cached 
## object next time) and than the newly calculated inverse matrix is returned.
## Use the generated object from makeCacheMatrix as an input: cachSolve(myMatrix).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
