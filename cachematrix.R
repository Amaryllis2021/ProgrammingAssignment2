
## Creates a pair of matrix object that can cache its inverse.

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## As argument we assume that the matrix we supply here is invertible. Null is a reserved word in 
## R and it represents null objects and was assigned to the variable inv.
## Then I set the value of another matrix using another function and using the double arrow assignment operator 
## which can be used to assign a value to an object in an environment that is different from the current environment. 
## Double arrow are capable of modifying variables in the parents levels.
## Set the value of trhe matrix and get the value of the matrix. Then set the value of inverse and get the value of the inverse.



## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
## Initialize the inverse property        
        inv <- Null
## Method to set the matrix        
        set <-function(y){
            x <<- y
            inv <<- NULL

        }
## Method the get the matrix       
        get <-function(y) {x}
## Method to set the inverse of the matrix       
        setInverse <- function(inverse) {inv <<- inverse}
## Method to get the inverse of the matrix        
        getInverse <- function () {inv}
## Return a list of the methods        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## R functions can encapsulate state information. When invoked, functions defines and returns functions
## in a list. 



cacheSolve <- function(x, ...) {
        inv <-x$getInverse()
## Return a matrix that is the inverse of 'x' but first check if the inverse has already beem calculated
        if (!is.null (inv)) {
## Just return the inverse if its already set               
                message ("getting cached data")
                return (inv)
 
        }
        mat <- x$get()
## Get the matrix from our object        
        inv <- solve(mat, ...)
## Set the inverse
        x$setInverse(inv)
## Return the matrix        
        inv
 }


###Lexical scoping in R has consequences beyond how free variables are looked up. In particular, it’s
###the reason that all objects must be stored in memory in R. This is because all functions must carry
###a pointer to their respective defining environments, which could be anywhere. Roger D. Peng in "R Programming for Data Science"

