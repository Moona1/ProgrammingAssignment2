makeCacheMatrix <- function(x = matrix()) {
    
    cacheMatrix <- makeVector(x)    #calls the makeVector
    
    IA <- x$getInverse          # gets the inverse attribut from "get"
    
    if (!is.null(IA)) {
        message("getting cached inverseMatrix")
        return(IA)    #get inverse from the cache
    } else
        
    {
        inverseMatrix <- cacheSolve(x)   #if inverse is not available, cacheSolve will return inverse
    }
    
    setInverse <-function(cacheMatrix) cacheSolve(cacheMatrix)
    getInverse <-function(cacheMatrix) cacheMatrix
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    return(cacheMatrix)
}

cacheSolve <- function(x, ...) {
    inverseMatrix <- solve(x)  #returns inverse of matrix
    return(inverseMatrix)      #returns the value
}