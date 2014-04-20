# implement cache to solve inverse matrix
#
# ex: 
# my_matrix <- matrix(rnorm(16)), 4, 4)
# my_inverse_matrix <- cacheSolve(makeCacheMatrix(my_matrix))


## Creat cache matrix with a list of four functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # set the matrix value
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        # get matrix value
        get <- function() x
        
        # set inverse matrix value
        setInv <- function(im) m <<- im
        
        # get inverse matrix value
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## solve the inverse matirx from cache matrix

cacheSolve <- function(x, ...) {
        # obtain inverse matrix, if not yet computed, NULL will be returned
        m <- x$getInv()
        
        # if inverse matrix has been computed already, return the exisiting 
        # result and end the function
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        # get matrix value
        data <- x$get()
        
        # compute inverse matrix and return
        m <- solve(data, ...)
        x$setInv(m)
        m
}
