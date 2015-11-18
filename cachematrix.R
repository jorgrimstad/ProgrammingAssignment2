## This pair of functions first creates a list 
## that can get and set a matrix and its inverse to 
## stored locations in memory, and then a second
## function to calculate the inverse of a matrix 
## (or retrieve the stored inverse from memory if
## it has already been calculated before).

# This function initializes a list and a set of functions
# that allows it to set a matrix in memory, retrieve it 
# from memory, set its inverse in memory, and retrieve its 
# inverse from memory. Each function in this larger function 
# "edits" an item in the list that is returned at the end of 
# the longer function.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) s <<- inverse
      getInverse <- function() s
      list(set = set, get = get,
           getInverse = getInverse,
           setInverse = setInverse)
}


# Function checks to see if matrix already has a stored
# inverse; if so, it retrieves the cached inverse from memory and returns
# it. If not, it gets the matrix from the list created by the makeCacheMatrix
# function, applies solve() (to get the matrix inverse), sets the cached 
# inverse in stored memory to the just-calculated inverse, and 
# returns the inverse.

cacheSolve <- function(x, ...) {
      s <- x$getInverse()
      if(!is.null(s)) {
            message("getting cached inverse")
            return(s)
      }
      mtx <- x$get()
      s <- solve(mtx)
      x$setInverse(s)
      s
}

# Resources used in developing this assignment:
# 1) Troubleshooting: http://stackoverflow.com/questions/6572119/r-solvesystem-is-exactly-singular
# 2) Information re: inverse of matrix: http://www.statmethods.net/advstats/matrix.html
# 3) "Caching the Mean of a Vector" example in course materials