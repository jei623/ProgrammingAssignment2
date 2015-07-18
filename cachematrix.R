## This pair of functions creates a matrix that can store its inverse in 
## cache memory and then return it quickly without having to recalculate
## it as long as it hasn't changed since the last time it was calculated

## This function creates a matrix along with some supporting functions to 
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL             #initializes m (inverse) to null
      set <- function(y) {  #transers the matrix values
            x <<- y
      m <<- NULL
      }
      get <- function() x  #function to get the original x matrix back
      setinv <- function(solve) m <<- solve  #sets the matrix inverse in to cache
      getinv <- function() m                 #gets the matrix inverse from cache
      list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinv() #gets the inverse value if there is one
      if(!is.null(m)) {  #if the inverse isn't null (an inverse was already calculated...)
            message("getting cached inverse")
            return(m)
      }
      
      #otherwise calculate the inverse of the matrix
      data <- x$get()   #gets the data from matrix x
      m <- solve(data, ...)  #calculates its inverse
      x$setinv(m)  #puts the inverse in cache for later
      m            #returns the inverse calculated
}
