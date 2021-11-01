## This is the Programming Assignment 2: Lexical Scoping submital for Andrew. 
## To run an example of this enter this in consul
## > m <- makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(m)

## There is a very good forum post here explaining the example
## https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/jyJBRWmREeaeEw4INb6PhQ

## makeCacheMatrix works the same as makeVector in the example 
## it sets up the assinged matrix with the $set(), $get(), ... names. 
## The input needs to be in this form: (matrix(data, nrows, ncol)
## mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## The solve function we are asked to use will not calculate the inverse matrix
## if matrix is larger then 2x2 at least from my testing. 


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setmatrixinverse <- function(inverse) i <<- inverse
  getmatrixinverse <- function() i
  list(set = set, get = get,
       setmatrixinverse = setmatrixinverse,
       getmatrixinverse = getmatrixinverse)
}


## The cacheSolve function takes the vector you defined and solves for the 
## inverse matrix. The solve function automatically solves for inverse matrix.
## Matrix must be 2 x 2

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getmatrixinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setmatrixinverse(i)
    i
  }
