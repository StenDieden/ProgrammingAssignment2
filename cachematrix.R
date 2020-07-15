## Given a matrix as its argument, the first function creates an object to 
## store the inverted argument, as well as a set of functions to place and 
## retrieve the inverted matrix’ in and from “storage”.
## The second function requires the first function’s output as arguments
## to either (i) retrieve the inverted matrix – if already stored  - from 
## storage (cache), or (ii) create and store the inverted matrix.  


##The makeCacheMatrix function defines six sibling objects in its environment;
## v, y and four "listed" functions. When run with a matrix as input the
##function creates a environment within the global environment, that hosts six
## objects accessible to the second function, 

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL  
    
    set <- function(y) {
      x <<- y
      v <<- NULL
    }
    
    get <- function() x
    setinv<- function(inv) v <<- inv
    getinv <- function() v
    list(set = set, get = get, setinv = setinv, getinv = getinv)
  }


## Taking output from the first function, the solveCacheMatrix function either
## (i) retrieves the inverted matrix from the "v" cache,if already stored, or 
## (ii) creates and stores the inverted matrix in v.

solveCacheMatrix <- function(x, ...) {

  v <- x$getinv()

## (i)
    if(!is.null(v)) {
    message("getting cached data")
    return(v)
    }

## (ii)  
  data <- y$get()
  v <- solve(data, ...)
  x$setinv(v)
  v
}