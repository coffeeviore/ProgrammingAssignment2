## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  o<-NULL  
  set<-function(y){ #define set function
    x<<- y
    o<<-NULL
  }
  get<-function()x #create get function with x
  setInverse<-function(inverse)o<<-inverse #Define the set for inverse function.
  getInverse<-function()o #Define the get for inverse function
  list(set = set, get= get, # create list for the four functions
       setInverse= setInverse,
       getInverse= getInverse)

}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  o<-x$getInverse()
  if(!is.null(o)){
    message("getting cached data")
    return(o)
  }
  
  data<-o$get()  #get the the vector from  input object
  o<-solve(data, ...) #Calculates 
  o$setInverse(o) #set the result
  o #return the result
}
