## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates a special object that includes the initial matrix, as well as functions that can set values or retrieve values.


makeCacheMatrix <- function(initial_matrix = matrix()) {
        calculated_inverse <- NULL  #
        
        set <- function(y) {
                initial_matrix <<- initial_matrix ##makes the initial_matrix available in the parent environment
                calculated_inverse <<- NULL #sets the calculated_inverse to NULL and makes it available in the parent environment
        }
        
        get <- function(){ 
                initial_matrix
        }
        
        setsolve_int <- function(calculated_inverse){ 
                calculated_inverse <<- calculated_inverse ##calculated_inverse is now being accessible outside of makeCacheMatrix
        }
        
        getsolve_int <- function(){
                calculated_inverse
        }
        
        list(set = set, get = get,
             setsolve_ex = setsolve_int,
             getsolve_ex = getsolve_int)  #Creating an object here that is being returned so it can be used outside of makeCacheMatrix
        
}


## Write a short comment describing this function

cacheSolve <- function(initial_matrix, ...) {
        ## Return a matrix that is the inverse of 'initial_matrix'
        
        calculated_inverse <- initial_matrix$getsolve_ex()
        
        if(!is.null(calculated_inverse)) {
                message("getting cached inverse data")
                print(calculated_inverse) #printing the inverse
                return(calculated_inverse) #finishes here if condition met
        }
        
        data <- initial_matrix$get()
        calculated_inverse <- solve(data, ...)
        initial_matrix$setsolve_ex(calculated_inverse)
        message("Calculating New Inverse")
        print(calculated_inverse)
        calculated_inverse  #now it's accessible outside of cacheSolve. 
}
