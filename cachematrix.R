## The two functions are nested within each other and 
## together they allow the inverse of a matrix to be calculated and 
## kept in the memory. If the same matrix is input for calculation
## of the inverse, then the mean value will be pulled from the 
## memory instead of being recalculated - this saves time. 
## If a new/different matrix is input, then a new inverse
## is calculated. 

## This first function creates an R object of the type "list" 
## that stores a matrix and its inverse. 

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


## This second function takes as argument the list created
## with the first function. It retrieves the inverse matrix
## that is stored in the list's envitonment. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
## Return a matrix that is the inverse of 'x'
}

## You can test the two functions, after creating them, 
## by running these lines: 

maria <- matrix(rnorm(1:1000000), 1000, 1000)

## the next line assures the matrix has inverse
diag(maria) <- diag(maria) + 1

maria1 <- makeCacheMatrix(maria)

date(); o <- cacheSolve(maria1); date()
## worked 4 sec

date(); o <- cacheSolve(maria1); date()
## worked less than 1 sec

