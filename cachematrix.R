## Put comments here that give an overall description of what your
## functions do


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	
	inverse <-NULL	
	
	#returns the actual matrix	
	get <- function() x

	#returns the inverse of the matrix
	getInverse <-function() 
	{
		return(inverse)
	}

	#stores the inverse of the matrix
	setInverse<-function(i)
	{
		#stores inverse in the makeCacheMatrix scope
		#using <- would only store it in setInverse scope
		#so it wouldn't be available from cacheSolve
		inverse<<-i
	}
	list(get=get, getInverse=getInverse, setInverse=setInverse)
}


#This computes the inverse of the special "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated (and the matrix has not changed)
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      
	#get the inverse from the CacheMatrix object
	m_inverse<-x$getInverse()
	
	#if the inverse has already been calculated, return it
	if(!is.null(m_inverse)) 
	{
            message("getting cached data")
		return (m_inverse)
      }
	#Otherwise calculate the inverse
	else
	{		
		print("Calculating inverse")
		#get the actual matrix from the cache matrix object
		actual_matrix <-x$get()
		
		#calculate the inverse
		m_inverse <-solve(actual_matrix)

		#store the inverse
		x$setInverse(m_inverse)

		return (m_inverse)
	}
}

## Tests whether the program works
## Gets the inverse twice but only calculates the inverse once
## The second time it prints out get cached data
test <-function()
{
	##create matrix
	y<-cbind(1,2,3,4)
	m <- matrix(y,2,2)

	#create a special "matrix" object that can cache its own inverse	
	cache_matrix <- makeCacheMatrix(m)
 
	#calculate the inverse
	m_inverse <- cacheSolve(cache_matrix)

	##check that it is the inverse
	##that is matrix by its invers is equal to the identity matrix
	print(m %*% m_inverse)			

	#calculate the inverse again - this time it uses the cached
	#value of the matrix
	m_inverse <- cacheSolve(cache_matrix)
	
	##check equal to identity matrix
	print(m %*% m_inverse)			
}
