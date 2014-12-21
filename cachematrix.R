makeCacheMatrix<-function(x=matrix()){   #function named according to instructions
  inverse<-NULL #for storing inverse when computed as cache
  setMatrix<-function(y){ #y:is a numeric matrix,to be saved into the variable 'x'
    inverse<<-NULL
    x<<-y
  }
  getMatrix<-function() x #prints or returns the saved matrix
  setInverse<-function(ix){ #saves the inverse 'ix' in cache variable 'inverse'
    inverse<<-ix
  }
  getInverse<-function() inverse #returns the inverse
  list(setM=setMatrix,getM=getMatrix,getIn=getInverse,setIn=setInverse)#list of function, standard way of returning multiple functions from a closure
}

cacheSolve<-function(matrixList,...){ #named according to instructions, takes previous function's return as argument
  inverse<-matrixList$getIn()#gets the saved inverse, if not computed yet, NULL is returned
  if(!is.null(inverse)){
    print("Inverse already in cache.Printing...")
    return(inverse)
  }
    matrix<-matrixList$getM()#get the matrix to compute the inverse
    inverse<-solve(matrix,...)
    matrixList$setIn(inverse)#saves the computed inverse to cache
    return(inverse)
}


#I have added a test case,just to make things easier.

temp<-list(12,23,34,45,65,65,45,23,43)
testmatrix<-matrix(unlist(temp),3,3)#unlist used because solve only works on numeric matrices 
m<-makeCacheMatrix(testmatrix)
cacheSolve(m)#will compute inverse
cacheSolve(m)#will simply return the already present inverse from the cache
