#input an invertible matrix, x
#function will return a list of functions:
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse
#output: list that is then the input for the cacheSolve function.

makeCacheMatrix<-function(x=matrix()){
  inv<-NULL #the resulting inverse of the matrix
   set<-function(y){
     # use <<- operator to assign a value to an object in an environment that is different from the current environment. 
    x<<-y
    inv<<-NULL
  }
  get<-function()x #get value of matrix
  setinv<-function(invx) inv<<-invx #set value of matrix inverse
  getinv<-function() inv #get value of matrix inverse
  list(set=set,get=get,setinv=setinv, getinv=getinv)
}

#input: the output from the makeCacheMatrix function.
#return the inverse of the matrix x
cacheSolve<-function(x,...){
  inv<-x$getinv() #return inverse if it has been calculated already
  if(!is.null(inv)){
    #get it from cache and skip computation
    message("getting cached data")
    return(inv)
  }
  #else, calcuate the inverse
  data<-x$get()
  
  #sets the value of the inverse in cache by using the setinv function
  inv<-solve(data)
  x$setinv(inv)

## Return a matrix that is the inverse of 'x'
   inv
}
