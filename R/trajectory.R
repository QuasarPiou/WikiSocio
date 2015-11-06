#' Download a list of all the contributions of an user
#' 
#' @import httr
#' @export
#' 
user<-function(user,namespace="0",domaine="fr",traminer=FALSE) {
  arvcontinue<-NULL
  result<-matrix(ncol=2,nrow=1) 
  repeat {
	if(is.null(arvcontinue)) {
		query=list(
			action="query",
			list="allrevisions",
			format="json",
			arvnamespace=namespace,
			arvlimit="max",
			arvuser=user)
	} else {
		query=list(
			action="query",
			list="allrevisions",
			format="json",
			arvnamespace=namespace,
			arvlimit="max",
			arvuser=user,
			arvcontinue=arvcontinue)	
	}
	
	exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
  
	content<-content(exec,"parsed")
	arvcontinue<-content$continue$arvcontinue
	data<-content$query$allrevisions
	
	# Sélection des données
	pages<-sapply(data,function(x) {x["title"]})
	names(pages)<-NULL
	pages<-unlist(pages)
	revisions<-sapply(data,function(x) {x["revisions"]})
	names(revisions)<-NULL
	
	# Conception du tableau vide
	nbRevisions<-sapply(revisions,function(x){
		length(x)
	})
	table<-matrix(nrow=sum(nbRevisions),ncol=2)
	
	# Calcul de la première colonne
	table[,1]<-unlist(lapply(pages,function(x){
		rep(x,nbRevisions[[match(x,pages)]])
		}))
		
	# Calcul de la seconde
	table[,2]<-unlist(lapply(revisions,function(x) {
	
		sapply(x,function(y) {
			
			y$timestamp
			
		})
	
	}))
	
	result<-rbind(result,table)
	
	if(is.null(arvcontinue)){
		break
	}
  }
  result<-result[-1,]
  names(result)<-c("Article","Date")
  return (result)  
}

