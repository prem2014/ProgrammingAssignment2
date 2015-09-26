## This R script  uses <<- operator which can be used to assign a value to an object 
## which in its' parent environment. Execute <?"<<-"> in R-console to get better idea.


## Return a list which is used to cache inverse of matrix.
makeCacheMatrix <- function(x = matrix()) {
        inverseMat <- NULL
        
        # set matrix and reset the it's inverse value accordingly
        set <- function(mat) {
          x <<- mat;
          inverseMat <<- NULL;
        }
        get <- function() x
        
        setInverseMat <- function(inv) inverseMat <<- inv
        getInverseMat <- function() inverseMat
         
        list( set = set,
              get = get,
              setInverseMat = setInverseMat,
              getInverseMat = getInverseMat
             )
}

## Takes <makeCacheMatrix> type in named argument
## Return the inverse of matrix from cache(if there) 
## else it computes the inverse, cahces it and then returns inverse. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMat <- x$getInverseMat()
        
        if(!is.null(inverseMat)) {
          message("getting inverse from cached")
          return(inverseMat)
        }
        
        data <- x$get()
        inverseMat <- solve(data, ...);
        
        # cache the computed inverse matrix
        x$setInverseMat(inverseMat)
        
        # return inverse matrix
        inverseMat
}









