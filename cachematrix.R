###  Two functions that cache the inverse of a matrix to
###  avoid repetitive costly computations.



## Creates a special matrix object which can cache its inverse.
## Several closures (child functions) are defined.
## The "<<-" operator is used when we want to set a value to
## an object of the parent function (thus maintaining state,
## and specifically caching the inverse between calls)
makeCacheMatrix <- function( mat = matrix() ) {

    # Initialize the inverse
    # NULL indicates that the matrix is not yet computed and cached
    inv <- NULL


    # Set/Update the matrix
    # This function is useful when we want to change/update the matrix
    # It sets the inverse to NULL to be computed again when requested
    set <- function( mat_new ) {
        mat <<- mat_new
        inv <<- NULL
    }


    # Return the matrix
    get <- function() mat


    # Set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }


    # Return the inverse of the matrix
    getInverse <- function() {
        inv ## Return the inverse
    }


    # Return a list of the above methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}




## Calculate the inverse of the special matrix created by "makeCacheMatrix"
## and set the calculated inverse back to the special matrix
## If the inverse has already been calculated, then retrieve and return
## the inverse from the cache
## We use "matrix_x" as the argument name to show that it is a special matrix
cacheSolve <- function(matrix_x, ...) {

    # Get the inverse of 'matrix_x' - from 'matrix_x' cache (may be NULL)
    inv <- matrix_x$getInverse()

    # If already set (and thus not NULL) return that
    if( !is.null(inv) ) {
        message("getting cached inverse")
        return(inv)  # return the inverse and exit the function
    }

    # If we reach this point, it means the inverse has not been computed
    # We will get the original matrix using 'get' in order to compute it
    original <- matrix_x$get()

    # Calculate the inverse using the R solve() function
    # You may confirm that the result is correct by
    # confirming that : inv %*% original == I (identity matrix)
    inv <- solve(original, ...)

    # Set the inverse to the object
    matrix_x$setInverse(inv)

    # Return the inverse matrix
    inv
}






## A testing function to make sure that the above functions work as expected
## Run it without arguments : test_it()
test_it <- function() {

    # We create a big square matrix with 1000x1000 random values
    message("Creating a big square matrix with 1000x1000 RANDOM values")
    mat_square <- matrix(rnorm(1000000), 1000, 1000)
    str(mat_square)

    message("Calculating the inverse with the solve() function")
    # system.time() can measure the time it takes to run the enclosed function
    print(system.time(inverse1 <- solve(mat_square)))

    message("---")
    message("Creating the special matrix with makeCacheMatrix()")
    matx <- makeCacheMatrix(mat_square)
    message("Confirming the result is a list:")
    print(class(matx))

    message("Calculating the inverse with cacheSolve()")
    print(system.time(inverse2 <- cacheSolve(matx)))

    message("Calculating the inverse with cacheSolve() for a second time..")
    print(system.time(inverse2 <- cacheSolve(matx)))

    message("Testing if the two inverses are identical - the first from solve() and the second from cacheSolve()")
    message("Using: identical(inverse1, inverse2) ")
    print(identical(inverse1, inverse2))

    message("---")
    message("Resetting the cached matrices calling the set() closure of makeCacheMatrix")
    mat_square <- matrix(rnorm(1000000), 1000, 1000)
    matx$set(mat_square)  # update with the new random matrix

    message("Calculating the NEW inverse with cacheSolve()")
    print(system.time(inverse3 <- cacheSolve(matx)))

    message("Checking if the inverse changed - (It should)")

    if(!identical(inverse2, inverse3))
        message("Inverse updated!")
}
