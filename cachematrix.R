##makeCacheMatrix: Create a cache space in the global environment to hold a matrix and it's inverse. Preset both with NULL.makeCacheMatrix returns a list which includes two intermediate objects set and get but also the original matrix and it's inverse
##cacheSolve: Checks if the inverse exists (i.e. non-NULL). 
##If NULL, it computes the inverse using solve and caches it for future iterations.
##If non-NULL, cacheSolve retrieves the inverse from the cache
##
makeCacheMatrix<-function(x=matrix()){
	m<-NULL					##preset m (inverse) to NULL
	set<-function(y){		##define function that... 
		x<<-y				##caches orig matrix ....
		m<<-NULL			##and the inverse
	}	
	get<-function() x		##define function that creates a list...
	setmatrix<-function(solve) m<<-solve	##with the cached matrix and inverse
	getmatrix<-function() m
	list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

cacheSolve<-function(x=matrix(),...){	##create function to...
	m<-x$getmatrix()					
	if(!is.null(m)){					##get inverse from cache IF NON-NULL
		message("getting cached data")
		return(m)						
			}
	matrix<-x$get()						##or, if Inverse NULL (first time)
	m<-solve(matrix,...)				##...then compute inverse using solve()
	x$setmatrix(m)						
	m									##..and return inverse
}