##This package allows to input an matrix and gives back the inverse matrix
##It extracts the calculated inverse matrix directly instead re-running it. 

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        calculated <- NULL
        set <- function(y){
                x <<- y
                calculated <<- NULL
        }
        get <- function() x
        setInv <- function(inverse)calculated <<- inverse
        getInv <- function()calculated
        list(set = set, get = get, getInv = getInv, setInv = setInv)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated(and the matrix has not chnaged,)
##then the cacheSolve should retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        calculated <- x$getInv()
        if(!is.null(calculated)){
                calculated
        }
        m <- x$get()
        calculated <- solve(m, ...)
        x$setInv(calculated)
        calculated
}
