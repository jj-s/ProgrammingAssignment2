## The functions in this file create an object (mCacheVector) that stores a matrix (m) and caches its inverse (mInverse)

## makeCacheMatrix() takes a matrix as argument and returns a list of type mCacheVector which contains functions to 
## (1) set the value of the matrix - set
## (2) get the value of the matrix - get 
## (3) set the value of the inverse of the matrix - setInverse
## (4) get the value of the inverse of the matrix - getInverse

makeCacheMatrix <- function(m = matrix()) {
    mInverse <- NULL
    
    set <- function(mNew) {
        m <<- mNew
        mInverse <<- NULL
    }
    get <- function() m
    setInverse <- function(mInv) mInverse <<- mInv
    getInverse <- function() mInverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() checks to see if the inverse has already been calculated and cached
## If avaiable, it gets the inverse from the cache
## If not, it calculates the inverse of the matrix and caches the value of the inverse via the setInverse function

cacheSolve <- function(mCacheVector, ...) {
    ## Return a matrix that is the inverse of 'm'
    mInv <- mCacheVector$getInverse()
    if(!is.null(mInv)) {
        message("getting cached data")
        return(mInv)
    }
    m <- mCacheVector$get()
    
    ## function Checks if a matrix is invertible and returns FALSE if it is not
    ## Credit: https://stackoverflow.com/questions/24961983/how-to-check-if-a-matrix-has-an-inverse-in-the-r-language
    isMatrixInvertible <- function(m) class(try(solve(m),silent=T))=="matrix"
    if (isMatrixInvertible(m)) {
        mInv <- solve(m)
        mCacheVector$setInverse(mInv)
    }
    else {
        message("The matrix is not invertible")
    }
    
    mInv
}
