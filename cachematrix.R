## These functions find the inverse of matrix. Important point is that inverse is cached 
## and this cached data is returned if the matrix is not changing.

## This function returns a list of for functions which sets the matrix, gets the matrix, 
## sets the inverse of matrix, and gets the inverse of matirx.

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(m)
{
  x <<- m
  inverse <<- NULL
}
get <- function() x
setinverse <- function (inv)
{
  inverse <<- inv
}
getinverse <- function () inverse
list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function takes the list of functions returned by makeCacheMatrix function and 
## finds if the inverse is already cached. If YES, it gets the cached data (i.e. inverse)
## without calculating it again. Rather, if inverse is NULL, it finds the inverse, caches
## it, and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  temp <- x$getinverse()
  if (!is.null(temp))
  {
    message("Getting inverse of matrix which is cached")
    return(temp)
  }
  mat <- x$get()
  i <- solve(mat)
  x$setinverse(i)
  i
}
