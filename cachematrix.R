#Gary Warren
# R Programming Assignment #2


## This function create a matrix object that can cache it's inverse per the assignment instructions.
## This is to avoid recomputing the inverse-- just take it from the cache if you can.       
## The logic closely follows the example code given meaning it is just boilerplate.

#makeCacheMatrix gets and sets the value of the matrix and the inverse of the matrix
#initialize everything here
#using the <<- "super operator" is actually creatintg the cache for us 

makeCacheMatrix <- function(x = matrix()) 
  {

   inverseOfMatrix <- NULL
  
   set <- function(y)
   {
     x <<- y
     inverseOfMatrix <<- NULL
   }
  

  get <- function() x
  setInverse <- function(inverse) inverseOfMatrix <<- inverse
  getInverse <- function() inverseOfMatrix
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Computer the inverse of the matrix returned from makeCacheMatrix()

cacheSolve <- function(x, ...) 
{
  
  ## Return a matrix that is the inverse of 'x'
  
  inverseOfMatrix <- x$getInverse()
  
  # see if the computed inverse value is in the "cache"
  
  if( ! is.null(inverseOfMatrix))
    {
    
    # it is--- we are done -leave
    
      print("We are using cached data")
      return(inverseOfMatrix)
    }
  
  # else not in cache so we must recompute using solve() and put in "cache" for next round
  
       inverseOfMatrix <-solve(x$get())   # solve it
       x$setInverse(inverseOfMatrix)      # and cache it
       inverseOfMatrix  
  
}


# Here are some quick tests

#testMatrix <- makeCacheMatrix(matrix(1:6,2))
#testMatrix$get()
#testMatrix$set(matrix(7:10,2))
#testMatrix$get()
#cacheSolve(testMatrix)
#cacheSolve(testMatrix)    # should pull from cache here
#testMatrix$getInverse()
