#' export

anonUser <- function(page,domaine="fr") {

    # Mise en place de la requête
    query=list(
       action="query",
       prop="contributors",
       format="json",
       titles=page)
	
	exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
    
	content<-content(exec,"parsed")
	
	# Sélection des données
    result<-content[[2]][[1]][[1]][[4]]
    return(result)
    
}

#' export

nbLinks <- function(page,domaine="fr",namespace="0") {
    
    list<-links(page)
    result<-length(list)
    return(result)

}