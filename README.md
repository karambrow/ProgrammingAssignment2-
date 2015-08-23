# ProgrammingAssignment2-
# makeCacheMatrix creates a list containing a function to 
 # 1. set the value of the matrix 
 # 2. get the value of the matrix 
 # 3. set the value of inverse of the matrix 
 # 4. get the value of inverse of the matrix 
 
 
  #`makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.  
  
  makeCacheMatrix <- function(x = matrix()) { 
     i <- NULL                                 # sets default inverse of matrix value (i) to be undefined 
     
     set <- function(y) { 
         x <<- y 
         i <<- NULL                           # function that caches i to be undefined
     } 
     get <- function() x                  # creates object 'get' , a function that retrieves the input matrix
     
     setinverse <- function(inverse) i <<- inverse    # creates function to calculate i using inverse
     
     getinverse <- function() i      # creates a function that will retrieve the i value
     
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)#return value of makeCacheMatrix a list of functions
 }

 
  ##`cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the       ##inverse has already been calculated (and the matrix has not changed), then the`cachesolve` should retrieve the
  ## from the cache.  
 
 cacheSolve <- function(x, ...) {             # Return a matrix that is the inverse of 'x'
  
     i <- x$getinverse() 
     if(!is.null(i)) {                # if there is a defined value of i cached,( NOT null), function returns cached i 
     
         message("getting cached data.") 
         return(i) 
     } 
     data <- x$get()  # if no i value is found, sets the output matrix of get() function in makeCacheMatrix as 'data' 
     
     i <- solve(data) # calculates i value from output matrix 
     
     x$setinverse(i)  # setinverse function: function assumes that the matrix is always invertible. 
     
     i                # returns cached i
 } 
 
 
