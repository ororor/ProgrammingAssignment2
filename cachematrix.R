## makeCacheMatrix creates a special matrix object, of four functions- set, get, set inverse and get inverse



makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setInverse<- function(solve) {m<<-solve}
    getInverse<- function() {m}
    list(set=set, get=get, setInverse= setInverse, getInverse=getInverse)
}

## cacheSolve check If the matrix inverse has already been calculated, it will instead 

##  find it in the cache and return it, and not calculate it again.

cacheSolve <- function(x, ...) {
    m<<-x$getInverse
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)   #Calculating the Inverse 
    x$setInverse(m)
    m
}
