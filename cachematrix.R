makeCacheMatrix <- function(x = matrix()) {
    
    cacheMatrix <- makeVector(x)    #calls the makeVector
    
    IA <- x$getInverse          # gets the inverse attribut from "get"
    
    if (!is.null(IA)) {
        message("getting cached inverseMatrix")
        return(IA)    #if inverse available, get inverse from the cache
    } else
        
    {
        inverseMatrix <- cacheSolve(x)   #if inverse is not available, cacheSolve will return inverse
    }
    
    setInverse <-function(cacheMatrix) cacheSolve(cacheMatrix) # sets inverse
    getInverse <-function(cacheMatrix) cacheMatrix       #gets inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    return(cacheMatrix)
}

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inverseMatrix <- solve(x)  #returns inverse of matrix
    return(inverseMatrix)      #returns the value
}