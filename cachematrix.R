
######   These are the general instructions to test the functions with two matrices supplied in
######   order to see how you can input a matrix,  compute the inverse of it, and then later
######  get the cached inversed matrix without recomputing the inverse.


#1
## first matrix to be used
## copy this a$set to the console then run the specific instructions to see results.
## (1) a$set(matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3,byrow=TRUE) )

        


#1
## second matrix to be used
## copy this a$set to the console then run the specific instructions to see results.
## (2) a$set(matrix(c(7,2,1,0,3,-1,-3,4,-2),nrow=3,ncol=3,byrow=TRUE) )


        ##1.1 Assign a$get to x:  (1) x<-a$get() or (2) x<-a$get()

#2 
#Use  (1)a$getinverse()  or  (2)a$getinverse().  You will obtain NULL, because inverse has not been obtained yet.

#3
# Use (1)cacheSolve(a) or (2) cacheSolve(a).  Your will obtain the Matrix Inverse of the Original given.
 
 #3.1 Assign cache solve to variable y. Like this:  (1)y<- cacheSolve(a)  or (2)y<-cacheSolve(a)

#4 
# Use (1)a$getinverse()  or  (2)a$getinverse().  This will show you that the inverse is stored, and do not
# affect anything.

#5 Use (1)cacheSolve(a)  or (2) cacheSolve(a).  This will show you the stored cached data. With the label of
# getting cached data.  The program is not recomputing. It is getting data from memory.

#6  You can test the result of the original matrix multiplied by its inverse matrix to obtain identity matrix.
# You can do this by doing this multiplication:   z<- x%*%y
# then :  print(z) or just type z.
#  If results are correct you will obtain a squared identity matrix with the same dimensions than the 
#  original used in the multiplication ( n x n), where n is the number of rows or columns.



##  The first function (makeCacheMatrix) sets original, gets original matrix, compute inverse and get inverse.
##  then all those results are stored in a list.

##  The second function assigns the inverse of the object selected to m.  Then it is evaluated
##  if this inverse is not equal to NULL. It that is correct if just retrieves the data stored in
##  memory. It that is not the case. The data in matrix is read, then the inverse is computed and
##  assgined to m.

##  the makeCacheMatrix do this things:
##   1.  It sets the original matrix ( set Function) to be used with the function named Set.  
##   It originally has the m value initialised to  NULL.  m stands for the Inverse of the Matrix.
##   2.  It gets the content of the Matrix established in the past set Function ( get Function)
##   3.  It computes the inverse of the matrix with the internal function Solve.  Then it assigns
##       the value of this inverse to the variable m,  this result is sent outside the scope of
##       the function.
##   4.  It gets the value of m.  When we are doing the calls, this will get the results after we have
##       called the function to solve the inverse matrix.






makeCacheMatrix <- function(x = matrix()) 
        {
                m<-NULL
                set<- function (y)
                {
                        x<<- y
                        m<<- NULL
                }
                get<- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list (set=set, get=get,
                      setinverse=setinverse,
                      getinverse=getinverse)
        }


## In this function what is done is the following:
## 1. The inverse of the object selected is computed and assigned to m.
## 2.  There is a logical evaluation to see is m is different to NULL.
##     If is different then:
##      Data is stored in memory: label "getting cached data" is print and inverse (m) is printed too.
##     If it is not different then:
##      Matrix is read and assigned to data, inverse in computed, and inverse is assiged through set function to
##      var x.

cacheSolve <- function(x, ...) 
        ## Return a matrix that is the inverse of 'x'
        {
        m<- x$getinverse()
        if (!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        data<- x$get()
        m<- solve(data,...)
        x$setinverse(m)
        m
                
        }
        
