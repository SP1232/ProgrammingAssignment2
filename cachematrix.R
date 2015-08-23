makeCacheMatrix <- function(x = matrix()) {
  # Code attempt by Sarfaraz Paracha / sarfaraz@inseyab.com
  
  #first and foremost, set Inverse to NULL
  inv <- NULL
  
  # set here is a function that takes an argument y and sets x to it, in the cache 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # get here is a function that simply gets the value of x
  get <- function() x
  
  # setInverse, taken as an an argument to a function, sets it for a 'x' in the cache
  setInverse <- function(setInverse) x <<- setInverse
  
  # getInverse simply returns 'inv'
  getInverse <- function() inv
  
  # what is returned in the end when using makeCacheMatrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheInv <- function(x, ...) {
  # getInverse function from the result of MakeCacheMatrix
  my_inv <- x$getInverse()
  
  if(!is.null(my_inv)) {
    message("getting your cached data")
    return(my_inv)
  }

  # get data from get function from the result of MakeCacheMatrix
  data <- x$get()

  # inverse the matrix using R's "solve()" function, passing above data as argument  
  my_inv <- solve(data)

  # set data from set function from the result of MakeCacheMatrix - pass my_inv as argument, that is the inverse matrix of x
  x$setInverse(my_inv)

  my_inv
}
