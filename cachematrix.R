## This function--makeCacheMatrix--creates a special "matrix" object that can cache its inverse.
## That is, if the matrix has already been inverted, it stores the matrix so 
## that it may be retrieved by thecacheSolve function. makeCacheMatrix creates a list containing a
## function to set the value of the matrix, get the value of the matrix, set the value of the
## inverted matrix, and retrieve the inverted matrix   

makeCacheMatrix <- function(x = matrix()) {
 	  mat <- matrix()	# create matrix for inversion
	  mat <- NULL		# initialize inverse matrix
        set <- function(y) { # stores the input matrix and initializes the inverted matrix to NULL
                x <<- y		# To cache the input matrix
                mat <<- NULL    # To cache inverted matrix
        }
        get <- function() x	# to retrieve the input matrix
        setmat <- function(solve) mat <<- solve		# to store the inverted matrix
        getmat <- function() mat	# to retrieve the inverted matrix 
        list(set = set, get = get,	#return a list with required functions
             setmat = setmat,
             getmat = getmat)
}


## The following function calculates the inverse of the input matrix created with the above function.
## It first checks to see if the matrix has already been inverted. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it inverts the matrix and stores the inverse the cache
## via the setmat function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getmat()	#retrieve inverted matrix
        if(!is.null(mat)) {	# if inversion already performed
                message("getting cached inverted matrix")
                return(mat)
        }
        data <- x$getmat()		# retrieve input matrix from cache
        mat <- solve(data, ...)	        # invert
        x$setmat(mat)			# store inverted matrix in cache
        mat				#return inverted matrix
}
