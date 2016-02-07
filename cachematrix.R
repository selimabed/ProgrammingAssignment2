## First take the example in the assignment,

  ##makeVector <- function(x = numeric()) {
  ##m <- NULL
  ##set <- function(y) {
    ##x <<- y
    ##m <<- NULL
  ##}
  ##get <- function() x
  ##setmean <- function(mean) m <<- mean
  ##getmean <- function() m
  ##list(set = set, get = get,
  ##setmean = setmean,
  ##getmean = getmean)
  ##}

## The only difference, we don't want the mean, but the inverse function. 

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Exactly the same process for the function "cacheSolve", with "cachemean"
## Here we're looking for the inverse instead of the mean

##cachemean <- function(x, ...) {
##m <- x$getmean()
##if(!is.null(m)) {
##message("getting cached data")
##return(m)
##}
##data <- x$get()
##m <- mean(data, ...)
##x$setmean(m)
##m
##}

cacheSolve <- function(x, ...) {
       
  inv = x$getinv()
  
   if (!is.null(inv)){
       message("getting cached data")
    return(inv)
  }
 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
   x$setinv(inv)
  
  return(inv)}

