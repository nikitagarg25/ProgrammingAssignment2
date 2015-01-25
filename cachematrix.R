#Answer1


#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(y = matrix()) 

{
    inverse <- NULL

    set <- function(z)
 {
        y <<- z
        

	  inverse <<- NULL
    }

    	get <- function() y


 	setinverse <- function(inverse) inverse <<- inverse

# setinverse overrides the previous value of inverse and assigns the argument to inverse    
	
	getinverse <- function() inverse

# getInverse returns the Inverse
    
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)


}

#Sample Output

# m <- makeCacheMatrix()
# m$set(matrix(c(1,3,7,9),2,2))
# m$get()
#     [,1] [,2]
# [1,]    1    7
# [2,]    3    9

 
#Answer2
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

	cacheSolve <- function(y, ...) {


# Retrives the most recent value for the inverse
    	
	inverse <- y$getinverse()
    		if(!is.null(inverse)) {
       		message("getting cached data.")     
	  return(inverse)
# If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value        
    }
    	data <- y$get()
    	
	inverse <- solve(data)
    
	y$setinverse(inverse)


    
	inverse
#Returns the new Inverse value
	}

# Sample output
# m$set(matrix(c(1,2,4,3),2,2))
# cacheSolve(m)
#      [,1] [,2]
#[1,] -0.75 0.58333333
#[2,]  0.25-0.83333333

