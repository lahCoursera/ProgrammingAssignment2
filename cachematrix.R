## The purpose of this assignment is to demonstrate how you can cache the inverse of a 
## function. It will check to see if a matrix has already been cached.  If it has, 
## then it will use the prior cached value instead of recalculating. It will also 
## print the line "getting cached data" to show it was not calculated a second time.

## as with the makeCacheVector this is a list to 
##      set the matrix
##      get the matrix
##      set the inverse of the matrix
##      get the inverse of the matrix

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


## The function below will check to see if the inverse of the given matrix had
## already been calculated, if it has previously calculated the inverse, the function
## returns the prior matrix inverse with the statement "getting cached data".  
## otherwise, it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
