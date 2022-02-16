## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

if (ncol(x)==nrow(x) && det(x)!=0) { #ensure that the matrix is invertible
    m <- NULL
    set <- function(y) { #set the matrix
            x <<- y
            m <<- NULL
    }
    get <- function() x #get the matrix
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set,
         get = get,
         getinverse = getinverse,
         setinverse = setinverse) # return a list of methods
}else{
     return(message("The matris is not invertible."))
}
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
m <- x$getinverse() ## Return a matrix that is the inverse of 'x'
   if (!is.null(m)) {
         return(m)
   }
   data <- x$get()
   m <- solve(data,...)
   x$setinverse(m)
   m # return the matrix
}
      
# testing
x <-makeCacheMatrix(matrix(c(1,0,-1,1,-1,1,0,1,0),3,3))
cacheSolve(x)
#     [,1] [,2] [,3]
#[1,]  0.5    0 -0.5
#[2,]  0.5    0  0.5
#[3,]  0.5    1  0.5

x <-makeCacheMatrix(matrix(c(1,2,3,4,5,6,7,8,9),ncol=3,nrow=3))
#The matris is not invertible.

