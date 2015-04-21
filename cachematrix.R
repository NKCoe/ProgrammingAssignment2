

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x)
{
        #initialise the value of matrix inverse to NULL
        inverse <-NULL
        
        getmatrix <-function()
                {
                x
                }
        #create matrix, and set inverse back to NULL if new matrix is different to old matrix
        setmatrix <- function(m){
                if(!identical(x,m))
                        {
                        inverse<<-NULL     
                        }
                x<<-m     
        }
        #gets the inverse
        getinverse<-function()
                {
                inverse
                }
        #sets the inverse
        setinverse <-function(the_inverse)
                {
                inverse <<- the_inverse
                }
        
        list(getmatrix = getmatrix, setmatrix = setmatrix, 
             getinverse = getinverse, setinverse = setinverse)
       
       
}


## This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(m) 
{
        ## Return a matrix that is the inverse of 'x'
        inverse<-m$getinverse()
        data<-m$getmatrix()
        if(is.null(inverse))
        {
                message("I had no cache so built this for you")
                inverse = solve(data)
                m$setinverse(inverse)
        }
        inverse
}

myMatrix<- matrix (rnorm(9),ncol=3)
myMatrix
m1<-makeCacheMatrix(myMatrix)
cacheSolve(m1)
myMatrix <- setmatrix(matrix(rnorm(9),ncol=3))
myMatrix

m2<-makeCacheMatrix(myMatrix)
det(m2)
cacheSolve(m2)


