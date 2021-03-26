# Put comments here that give an overall description of what your
# functions do

#the following function creates a special "matrix" object that can cache its inverse#
#initialize objects x and inv#
makeCacheMatrix <- function(x = matrix()){
        
        inv <- NULL
        
        #set objects behaviors#
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x} #get values of the matrix#
        
        setInverse <- function(inverse) {inv <<- inverse} #set the inverse#
        getInverse <- function(){inv} #get the inverse#
        #name the functions#
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#this function computes the inverse of the special "matrix" returned by#
#makeCacheMatrix above. If the inverse has already been calculated (and# 
#the matrix has not changed), then the cachesolve should retrieve the#
#inverse from the cache.#

cacheSolve <- function(x, ...){
        #checks if the inverse was already been calculated#
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        mat <- x$get() #gets the matrix#
        inv <- solve(mat,...) #calculates the inverse of the matrix#
        x$setInverse(inv) #set the inverse of the matrix#
        inv #Return a matrix that is the inverse of 'x'
}
