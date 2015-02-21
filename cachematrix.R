##***************************************##
##******* Programming Assignment 2 ******##
##*** Caching the Inverse of a Matrix ***##
##***************************************##

##1. In the first function, i created the matrix to invert using the var call "inver"
makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y) {
        x <<- y
        matrixInv <<- NULL
    }
    get <- function() x
    setIn <- function(inver) matrixInv <<- inver
    getIn <- function() matrixInv
    list(set = set, get = get,
         setIn = setIn,
         getIn = getIn)
}

## 2. In the second function, i calculate the inverse with the function solve and 
##    always return a messages that indicate the origin of the data
cacheSolve <- function(x, ...) {
    matrix <- x$getIn()
    if(!is.null(matrix)) {
        message("result of cached data")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data, ...)
    x$setIn(matrix)
    message("result without cached data")
    return(matrix)
}

##Example
##>
##> xMatrix <- makeCacheMatrix(matrix(rnorm(4),2,2))
##> xMatrix$get()
##[,1]       [,2]
##[1,] -2.0884368 -0.4341949
##[2,]  0.1108414  0.2541775
##>
##> cacheSolve(xMatrix)
##result without cached data
##[,1]       [,2]
##[1,] -0.5265670 -0.8995002
##[2,]  0.2296246  4.3265117
##>
## again
##>
##> cacheSolve(xMatrix)
##result of cached data
##[,1]       [,2]
##[1,] -0.5265670 -0.8995002
##[2,]  0.2296246  4.3265117
##> 
