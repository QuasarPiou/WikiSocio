#' Divide a list into vectors of a fixed size
#' @export
#' 

divideList<-function(x,number) {
	if(length(x)>number) {
		rest<-length(x) %% number
		if(rest==0) {
			result<-matrix(x,ncol=number,byrow=TRUE)
			result<-split(result,row(result))
		} else {
			index<-length(x)-rest
			result<-matrix(x[1:index],ncol=number,byrow=TRUE)
			result<-split(result,row(result))
			result[[length(result)+1]]<-x[index+1:length(x)]
			length(result[[length(result)]])<-length(x)-index
		}
		return(result)
	} else {
		return (as.vector(x))
	}
}

#' Retourne un vecteur qui ne contient que les valeurs communes aux deux arguments.
#' @export

keepSimilar <-function(vector1,vector2) {

	fromXtoY<-sapply(vector1,function(x,y) {
		
		if(is.integer(match(x,y))) {
			x
		}
		
	},vector2)
	
	fromYtoX<-sapply(vector2,function(x,y) {
		
		if(is.integer(match(x,y))) {
			x
		}
		
	},vector1)
	
	return(as.vector(rbind(fromXtoY,fromYtoX)))

}