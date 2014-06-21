### Overview of functions
## makeCacheMatrix is used to create the four functions given in its expressed list. 
## The list makeCacheMatrix output is used for the input of cacheSolve, after a matrix is set.
## makeCacheMatrix and cacheSolve work in tandem. 


### Description of makeCacheMatrix
## makeCachematrix creates and lists the four functions necessary to cache a matrix. 
## Its purpose is not to cache a matrix, but to provide the functions therefor.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        #       im represents inverted matrix
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        #       y represents the matrix inputted to the set function outside of makeCacheMatrix (see line 31)
        #       This is expressed by assigning the matrix y to object x in the global environment using <<-
        #       Similarly, a NULL value is assigned to im in the global environment
        get <- function() x
        setinv <- function(solve) im <<- solve
        getinv <- function() im
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# I use the above function by assigning the expressed list to another object, called l
# i.e. > l <- makeCacheMatrix()
# l$set(y) gives the matrix y to the set element of l
# y is then assigned to x, i.e. to the argument of makeCacheMatrix, which enables the get function (lines 19, 21)


### Description of cacheSolve
## cacheSolve checks if im for matrix y is already calculated.
## If no, it calculates im and prints it.
## If yes, it assigns the inverted matrix to im and prints the message (line 43) and returns im.

cacheSolve <- function(l, ...) {
        im <- l$getinv()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- l$get()
        #      When l is inputted to cacheSolve and im has not been calculated, 
        #      the matrix set by l$set (line 31) is returned by l$get
        im <- solve(data, ...)
        #      Allowing the inverted matrix to be calculated by solve
        l$setinv(im)
        #      And is set to im in the global environment (see line 21)
        im
}

## Return a matrix that is the inverse of x (line 32; i.e. the matrix that was originally called y).

## I hope to have illustrated the interplay between these two functions clearly.
## Good day!
