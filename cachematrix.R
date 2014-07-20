## Matrix storage and inversion functions.

## This function creates a special "matrix" object that can cache its inverse.
## (It assumes matrices it is called with are invertible)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y ## <<- set the value of x so it's available in outer
                        ## functions environment i.e. so 'get' can return
                        ## what is set here
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set=set, get=get, setmean=setmean, getmean=getmean)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        
        if(!is.null(m)) {
            # if the inverse of the matrix is already cached return it
            # rather than the potentially computationally expensive 
            # task of re-computing it for each request.
            message("getting cached inverted matric value")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setmean(m)
        m
}

## To test that this works (note '>' represents the R prompt) :
## > mat<-1:4
## > dim(mat)<-c(2,2)
## > mat ## check made a 2 x 2 matrix
## > cm<-makeCacheMatrix(mat)
## > cm$getmean() ## this should return NULL
## > cacheSolve(cm) ## this should print out the inverse of matrix 'mat'
## > cm$getmean() ## now set with the inverse of matrix 'mat'
