## I. Adopted from class example
## Input: matrix
## Output: list: 1) function to cache original matrix, 2) function to retrieve original matrix, 3) function to set inverse, 4) function to retrieve inverse 

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    x_Inverse<-NULL
    set<-function(y){
        x<<-y
        x_Inverse<<-NULL
    }
    get<-function() x
    setInverse<-function(Inverse)  x_Inverse<<-Inverse
    getInverse<-function() x_Inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## If inverse has already been calculated (and the matrix has not changed)
## retrieve the inverse from the cache using cachesolve 
## If haven't been computed 
## computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    x_Inverse<-x$getInverse()
    if(!is.null(x_Inverse)){
        message("getting cached data")
        return(x_Inverse)
    }else{
        M<-x$get()
        x_Inverse<-solve(M)
        x$setInverse(x_Inverse)
        ## Return a matrix that is the inverse of orginial matrix
        x_Inverse        
    }
}




## II. Changed from class example: cache and return the original matrix directly
## Input: matrix
## Output: list: 1) matrix, 2) function to set inverse, 3) function to retrieve inverse 

makeCacheMatrix <- function(x = matrix()) {
    M_Inverse<-NULL
    ##cache the value of the matrix
    M<<-x    
    ##cache the value of the inverse matrix
    setInverse<-function(Inverse)  M_Inverse<<-Inverse
    ##retrieve the value of the inverse matrix
    getInverse<-function() M_Inverse
    ##return a list consisted of the original matrix, the function to set inverse, and the function to get inverse
    list(M=M, setInverse=setInverse, getInverse=getInverse)
}


cacheSolve <- function(x, ...) {
    M_Inverse<-x$getInverse()
    ## If inverse has already been calculated (and the matrix has not changed), retrieve the inverse from the cache 
    if(!is.null(M_Inverse)){
        message("getting cached data")
        return(M_Inverse)
    }else{
        ##If no inverse was coached, get the matrix and solve its inverse
        M<-x$M
        M_Inverse<-solve(M)
        x$setInverse(M_Inverse)
        ## Return a matrix that is the inverse of 'x'
        M_Inverse
    }

}

