## Matrix versions of makeVector and cachemean
## Glen Leatherman

## create a function that packages get and set functions for a matrix and its inversion (solve).

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL					##  empty the local variable m, which will eventually hold a matrix function.
	
  set <- function(y) {				##  define the set function
      x <<- y					##  which assigns the outer matrix x and empties the matrix function m.
      m <<- NULL				##  this part is particularly tricky as
    }						##  x is the input parameter of the outer function.
						##  this is never called explicitly.  This is a common programming idiom
						##  but VERY confusing if you haven't seen it before.
  
  get <- function() x				##  define the get function, which simply returns the matrix x.
  setsolve <- function(solve) m <<- solve	##  define setsolve which puts the solve function into our matrix function variable m.
						##  NOTE: solve as used here is a variable name.  Any matrix function can be assigned to setsolve.
  getsolve <- function() m			##  define getsolve which returns the matrix function m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve) 
						##  return the functions as a list
}						##  NOTE: Nothing was actually done except defining functions (and one local variable m)
						##        The magic is the packaging of the input matrix with these functions (and m).


## either solve and cache or retrieve from cache the inversion of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()	##  get the matrix function from the passed-in object, which has to be
			##  a makeCacheMatrix object
    if(!is.null(m)) {   ##  if it has been cached, retrieve the cached copy
      message("getting cached data")    ##  this is the other tricky part because you have to know that
      return(m)				##  our object is not "solved" at definition.  we must check it.
    }
    data <- x$get()	  ##  on first run, this function will see that m is null (matrix inversion has NOT been solved yet.
    m <- solve(data, ...) ##  set m to the solution of the matrix
    x$setsolve(m)	  ##  set the solve function in x to our solve function m so that it won't have to solve again (cached).
    m			  ##  return the solution.
}
