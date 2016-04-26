## Caching the inverse of a matrix


## Creating a list that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<-y
                inv <<- NULL
        }
        get <- function() x
        setinver <- function(inver) inv <<- inver
        getinver <- function() inv
        list(set=set, get=get, setinver= setinver, getinver=getinver)

}


## Computing the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinver()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinver(inv)
        inv
        
}
