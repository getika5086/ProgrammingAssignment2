## The functions below help in commuting the inverse of a matrix

## the makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m_inv <<- solve
    getinv <- function() m_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the cacheSolve computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    m_inv <- x$getinv()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data)
    x$setinv(m_inv)
    m_inv
}
