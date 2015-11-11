#' @export

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

#' @export

nbLinks <- function(page,domaine="fr",namespace="0") {
    
    list<-links(page)
    result<-length(list)
    return(result)

}

#' @export

selectRandom <- function(nb,domaine="fr",namespace="0") {
    
    # Découpe du nombre en multiple de 500 (division euclidienne)
    quotient<-nb%/%500
    reste<-nb%%500
    # Requête
    result<-vector()

    ## On boucle pour prendre les multiples de 500
    if(quotient>0) {
    for(i in 1:quotient) {
        query<-list(
            action="query",
            list="random",
            format="json",
            rnlimit="max",
            rnnamespace=namespace)
  
    exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
    content<-content(exec,"parsed")
    content<-content[[4]][[1]]
    content<-sapply(content,function(x){x$title})
    result<-c(result,content)
    }
    }
    ## On prend les restes
    if(reste!=0) {
        query<-list(
            action="query",
            list="random",
            format="json",
            rnlimit=reste,
            rnnamespace=namespace)
  
        exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
        content<-content(exec,"parsed")
        content<-content[["query"]][[1]]
        content<-sapply(content,function(x){x$title})
        result<-c(result,content)
  
    # Il faut aussi filtrer pour voir si l'on a des doublons
    result<-unique(result)
        if(length(result)!=nb) {
            print(paste("Le vecteur est de longueur inférieure à",nb,". Nombre de pages vraiment sélectionnées :",length(result),sep=" "))
        }
    }
    return(result)
}