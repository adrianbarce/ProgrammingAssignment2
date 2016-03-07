## The main objective of this functions is to 
## calculate the inversion of a Matrix. In order 
## to do it more efficiently, we will cach the inverse
## and not computing it again and again when available.

## The following function will: 
## set the value of the matrix
## get values of the matrix
## set values to the inverse of the matrix and
## get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## The function below gives back the inverse of the matrix created above
## It first checks if the inverse has been computed already. If it has it on cache
## it skips all the procedure of compute. If not it does it and sets the value in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
