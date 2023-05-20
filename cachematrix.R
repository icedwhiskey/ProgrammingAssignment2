## makeCacheMatrix() cache inverse matrix and cachesolve populate or 
## get inverse matrix from makeCacheMatrix()

## Cache inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_mtx <- NULL
        set <- function(y){
                x <<- y
                inv_mtx <<- NULL
        }
        get <- function() x
        setinv_mtx <- function(z) inv_mtx <<- z
        getinv_mtx <- function() inv_mtx
        list(set=set, get=get, setinv_mtx=setinv_mtx, getinv_mtx=getinv_mtx)
}


## Populate or get inverse from makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv_mtx <- x$getinv_mtx()
        if(!is.null(inv_mtx)) {
                message("getting cached data")
                return(inv_mtx)
        } else {
                data <- x$get()
                inv_mtx <- solve(data)
                x$setinv_mtx(inv_mtx)
                inv_mtx
}

}