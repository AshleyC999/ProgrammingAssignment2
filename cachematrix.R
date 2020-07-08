##Cache Matrix
###Ashley Campbell
####11/7/2020
#####Description: Returns inverse of matrix x and caches for future use

### Set the matrix and the environment
makeCacheMatrix <- function(x = matrix()) {
        MI <- NULL
        set <- function(y){
                x <<-y      #set x in parent environment
                M <<- NULL
        }
        get <- function() x #get x in parent environment
        setMI <- function(solve) MI <<- solve
        getMI <- function() MI
        list(set = set, get = get, setMI = setMI, getMI = getMI) #allow easy ref.
}


###Solve the matrix 
cacheSolve <- function(x,...){
        MI <- x$getMI()
        if(!is.null(MI)){
                message("getting cached matrix")
                return(MI) #return matrix that is inverse of x if calc done before
        }
        data <- x$get()
        MI <- solve(data) #calculate inverse...
        x$setMI(MI) #...and populate setMI
        MI #return matrix that is inverse of x
}

###Example
mymatrix <- makeCacheMatrix(matrix(c(NUL),1,1))
mymatrix$set(matrix(c(1,2,3,4),2,2)) 
cacheSolve(mymatrix) 

