## This function below calculates the inverse of a matrix,stores the value in a
## a cache memory,so that if the same inverse is called for later,it will save time
## by recalling directly the previous value from the cache

##We start by creating the cache for the inverse of the matrix, which would be called 
## up in future when required

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}  


## Then we create a function which,before calculationg the mean
## of the function, it first verifies if it is availlable in the cache above
## If it is in the cache it only recalls the answer,a nd only calculates a if it is the 
## first time that exact matrix is evaluating the inverse 
cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
## Return a matrix that is the inverse of 'x'
}
