## R Programming course - Programming Assignment 2: "Lexical Scoping".
## 
## This file  provides two  functions that can be  used for caching  the time
## consuming operation of calculating the inverse of a matrix:
## 1. makeCacheMatrix: Create a special matrix that can cache its inverse.
## 2. cacheSolve:      Calculates the  inverse of  the matrix, and caches the 
##                     calculation so  that in any subsequent call it returns 
##                     the value from the cache  (providing that  the data of 
##                     the matrix object has not been changed)


## This function creates  a special matrix object that can cache its inverse.
## This special object provides the following functions.
##        set: 	set the value of the matrix  and "clears" the cached 
##              calculation of the inverse.
##        get: 	get the value of the matrix. 
## setinverse:	set the value of the inverse matrix.
## getinverse:	get the value of the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The  following  function  calculates  the  inverse  of  the special matrix 
## object generated with the makeCacheMatrix function above.
## Before performing the invertion the function checks if the calculation has
## been already done.  In this case  the inverse value  is returned  from the
## cache. 
## If the  calculation is not cached yet, the function calculates the inverse 
## of the special matrix  and caches the  result by calling "it's" setinverse 
## function.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)){
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
