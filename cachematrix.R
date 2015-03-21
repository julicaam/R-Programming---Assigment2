# The following two functions will cache the inverse of a matrix.
# This is very usefull because it will avoid computing the inverse
# of matrices repeatedly.
 
# This first function uses the input matrix and creates a list that contains 
# functions that will do the following:

#1 set the value of the matrix
#2 get the value of the input matrix
#3 set the value of the inverse of the input matrix
#4 get the value of the inverse of the input matrix
#5 returns a list with above functions as objects
 
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL    
    set_matrix <- function(y) {  #1 
         x <<- y
         inv_matrix <- NULL
    }
    get_matrix <- function() x  #2 
    set_inv <- function(inverse) inv_matrix <<- inverse  #3 
    get_inv <- function() inv_matrix  #4 
    list(set=set_matrix, get=get_matrix, setinverse=set_inv, getinverse=get_inv)  #5 
}
 
 
# This second function will return the inverse of a matrix.  It will call objects of the previously created list (or the 
# functions stored in the makeCacheMatrix) in the following order (see comments below):  

#1 Checks to see if the inverse of the matrix has been previously computed, then
#2 it will return the result and will not do any further computation of the inverse.
#3 If inverse of the matrix has not been previously computed, it will get the value of the input matrix,
#4 compute the inverse of the input matrix
#5 sets the value of the computed inverse in the cache matrix,
#6 and finally, it will return the result.

#This function always assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinverse()
    if(!is.null(inv_matrix)) { #1 
        message("Getting cached data...")
        return(inv_matrix) #2 
    }    
    new_data <- x$get() #3 
    inv_matrix <- solve(new_data) #4 
    x$setinverse(inv_matrix) #5 
    inv_matrix #6 
}
