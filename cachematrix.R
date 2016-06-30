## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	mtx <- NULL

	set <- function(y) {
		x <<- y
		mtx <<- NULL
	}

	get <- function() {
		x
	}

	setMatrix <- function(m) {
		mtx <<- m
	}

	getMatrix <- function() {
		mtx
	}

	list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
	mtx <- x$getMatrix()

	if(!is.null(mtx)) {
		message("Getting cached data")
		return(mtx)
	}

	data <- x$get()

	mtx <- solve(data)

	x$setMatrix(mtx)

	mtx
        
}

## This function is to test the code above

test <- function() {
	##Time taken to inverse a matrix that has not been computed before

	x <- makeCacheMatrix(matrix(rnorm(1000000), nrow=1000, ncol=1000))

	message("Time taken to inverse a matrix that has not been computed before")
	message("================================================================")

	startDateTime <- Sys.time()
	result <- cacheSolve(x)
	endDateTime <- Sys.time()

	message(paste("Start time : ", startDateTime))
	message(paste("End time : ", endDateTime))
	message(paste("Time difference : ", endDateTime - startDateTime))
	message("\n")


	## Time taken to inverse a matrix that has been computed

	message("Time taken to inverse a matrix that has been computed")
	message("=====================================================")

	startDateTime <- Sys.time()
	result <- cacheSolve(x)  
	endDateTime <- Sys.time()

	message(paste("Start time : ", startDateTime))
	message(paste("End time : ", endDateTime))
	message(paste("Time difference : ", endDateTime - startDateTime))  
	message("\n")


	## Time taken to inverse a matrix that has been assigned with new value

	x$set(matrix(rnorm(1000000), nrow=1000, ncol=1000))

	message("Time taken to inverse a matrix that has been assigned with new value")
	message("====================================================================")

	startDateTime <- Sys.time()
	result <- cacheSolve(x)  
	endDateTime <- Sys.time()

	message(paste("Start time : ", startDateTime))
	message(paste("End time : ", endDateTime))
	message(paste("Time difference : ", endDateTime - startDateTime, "\n"))    
	message("\n")


	## Time taken to inverse a reassigned matrix that has been computed

	message("Time taken to inverse a reassigned matrix that has been computed")
	message("================================================================")

	startDateTime <- Sys.time()
	result <- cacheSolve(x)  
	endDateTime <- Sys.time()

	message(paste("Start time : ", startDateTime))
	message(paste("End time : ", endDateTime))
	message(paste("Time difference : ", endDateTime - startDateTime))    
}