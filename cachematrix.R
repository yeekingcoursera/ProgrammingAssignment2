## Put comments here that give an overall description of what your
## functions do

## generate an object that stores a matrix and provides 
## cacheing functionality 
makeCacheMatrix <- function(x = matrix()) {
  solved <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(newSolved) solved <<- newSolved
  getsolve <- function() solved
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
}

# for reference... 
# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }


## calculate the inverse of the matrix returned by x$get()
## send additional options to solve using the ... arguments
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  solved <- x$getsolve()
  if(!is.null(solved)){
    message('getting cached data')
    return (solved)
  }
  # nothing cached - do the solve now
  data <- x$get()
  solved <- solve(data, ...)
  x$setsolve(solved)
  solved
}

# for reference
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }
