##Since the task of inversing (taking the reciprocality) of a matrix is a commonly needed function, yet very costly in time, devising a strategy to cache the inverse (i.e. committing it to memory and enabling high-speed retrieval) would prove to be time-friendly (rather than computing it separately).
## This assignment demonstrates two sequential functions 1. makes a matrix object capable of caching the matrix's inverse 2.calculate the aforementioned cached inverse in the first function. 

## The first function creates a certain matrix object that is capable of caching the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL set<-function(y){x<<-y inv<<-NULL} get<-function(x) 
  setInv<-function(inverse) inv<<-inverse 
    getInv<-function()inv
    list(set=set, get=get, setmean=setmean, getmean=getmean)}


## After caching the matrix's inverse in the first function, the next one calculates this inverse. Since the matrix has not changed, the cacheSolve function below should extract the inverse


cacheSolve <- function(x, ...) {
        inv<-x$getInv() if(!is.null(inv)){
          message("get cached data") return(inv)
        } MTX<-x$get()
        inv<-solve(MTX, ...)
        x$setInv(inv)
        inv}



