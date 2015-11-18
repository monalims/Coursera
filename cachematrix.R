##Program to create and cache the inverse of a matrix

##the makeCacheMatrix() creates a list of functions
##the cacheSolve() calculates the inverse for a new matrix.
## In case of a previously used matrix, it retrieves the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}



cacheSolve <- function(x=matrix(), ...) {
  m <- x$getSolve()
  
  if(!is.null(m)) {
    message("getting cached matrix data") ##changed this from the original statement
    return(m)
  }
  mat<- x$get()
  m <- solve(mat,...)
  x$setSolve(m)
  m
}

## to test the script, I used the following matrix below:
A<- matrix(c(1,2,3, 0,1,4,5,6,0), byrow=TRUE, nrow=3, ncol = 3) 
dat<-makeCacheMatrix(A)
cacheSolve(dat)

###On running the program for the fist time, this is the inverse of the matrix that I got

# cacheSolve(dat)
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

##On calling just the cacheSolve() function, the inverse is retrieved from the 
## cached data as is evident from the print message 'getting cached matrix data'

# cacheSolve(dat)
# getting cached matrix data
# [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1

