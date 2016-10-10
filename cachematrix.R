##Since the task of inversing (taking the reciprocality) of a matrix is a commonly needed function, yet very costly in time, devising a strategy to cache the inverse (i.e. committing it to memory and enabling high-speed retrieval) would prove to be time-friendly (rather than computing it separately).
## This assignment demonstrates two sequential functions 1. makes a matrix object capable of caching the matrix's inverse 2.calculate the aforementioned cached inverse in the first function. 

## The first function creates a certain matrix object that is capable of caching the matrix's inverse
##First we set the matrix value
##Then we get the matrix value
#Then we do the same for the inverse of the matrix: first we set the value, then we get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv=NULL 
  set<-function(y){
    x<<-y 
    inv<<-NULL} get<-function()x 
  setInv<-function(inverse) inv<<-inverse 
    getInv<-function()inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)}


## After caching the matrix's inverse in the first function, the next one calculates this inverse. Since the matrix has not changed, the cacheSolve function below should extract the inverse
## ##to check the calculation of the inverse of the matrix
##if the calculation has been done, get it
## if not, then calculate and obtain the result

cacheSolve <- function(x, ...) {
        inv<-x$getInv() 
        if(!is.null(inv)){ 
          message("get cached data") return(inv)
        } 
        MTX<-x$get()
        inv<-solve(MTX)
        x$setInv(inv)
        inv}



