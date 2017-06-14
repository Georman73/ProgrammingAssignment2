
##The first function, makeCacheMatrix creates a special "matrix" and cache its inverse

## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## The following function creates the inverse of the special "matrix" created with the above function. However,
## it first checks to see if the inverse has already been created. If so, it gets the inverse from the cache. 
## Otherwise, it creates the inversee of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

#Solution
matrix1<- makeCacheMatrix(matrix(51:54, 2, 2))
matrix1$get()
     [,1] [,2]
[1,]   51   53
[2,]   52   54
> cacheSolve(matrix1)
     [,1]  [,2]
[1,]  -27  26.5
[2,]   26 -25.5
> cacheSolve(matrix1)
getting cached data
     [,1]  [,2]
[1,]  -27  26.5
[2,]   26 -25.5

