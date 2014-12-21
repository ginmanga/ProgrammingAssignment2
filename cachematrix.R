## This program contains two functions makeCacheMatrix and cacheSolve
## The functions calculates the inverse of a matrix and stores it in case
## we need to calculate the inverse again


## makeCacheMatrix creates a list which contains four functions
## The "set" and "get" allow us to set and retrieve the value of the matrix we want to invert
## and "setmatrix" and "getmatrix" allow us to cache the inverse and retrieve a cached inverse

makeCacheMatrix <- function(x = matrix()) {
        ix<-NULL
        set<-function(y){          #Sets the value of the matrix and erases cached inverse (If it was calcualted before)
                x<<-y
                ix<<-NULL
        }
        get<-function() x          #When this function is called it retrieves the value of the matrix
        setmatrix <- function(invmatrix) ix <<- invmatrix #Sets the value of the inverse to the argument "invmatrix"
        getmatrix <-function() ix  #Retrieves the value of ix
        
        list(set = set, get = get, #Creates a list containing the four functions
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## It also checks if the inverse has already been calculated and
## in which case it prints the stored matrix and avoids
## calculating the inverse again 

cacheSolve <- function(x, ...) {
        ix <- x$getmatrix()     #Retrieves the value of the inverse or NULL (From the makeCacheMatrix environment)
        if(!is.null(ix)) {      #The if function checks if the inverse has already been calculated
                message("getting cached data")
                return(ix)      #If ix is different from NULL it prints the inverse and exits the function
        }
        data <- x$get()         #Retrieves the value of the matrix we want to invert (From the makeCacheMatrix environment)
        ix <- solve(data, ...)  #Calculates the inverse
        x$setmatrix(ix)         #Stores the value of the newly calculated inverse
        ix
}
