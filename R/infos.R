#' Retourne le pourcentage de contribution anonymes d'un article
#'
#' @param list Une liste de contribution construite avec revisionsPage
#'
#' @return Un pourcentage de contribution
#' @export 

percentAnon <- function(list) {
  list<-as.data.frame(list)
	result<-(length(list[list[,4]==TRUE,4])/length(list[,4]))*100
	return(result)
}

#' Range les contributeurs en fonction du poids de leur contribution
#' @param list Une liste de révision produite par revisionsPage
#' @param threesold Un seuil exprimé en pourcentage de contribution dans le poids de l'article pour garder le contributeur dans la liste
#' @param keepAnon Une valeur booléenne indiquant si oui ou non on doit conserver les contributeurs anonymes dans le classement
#' @return Une liste comprennant 1) un vecteur classant les contributeurs entre eux. 2) le poids des contributeurs présent dans le vecteur, en % de contribution à la page
#' @export
 
rankingContrib <- function(list,threesold,keepAnon=TRUE) {
  if(!keepAnon) {
    list<-as.data.frame(list[as.data.frame(list[,4])==FALSE,])
  }
  # On reformate le dF pour en garder que les deux bonnes colonnes bien formatées
  list<-as.data.frame(list)
  data<-tryCatch(data.frame(name=list[,1],weight=abs(as.numeric(list[,5]))),error=function(e) NULL)
  if(!is.null(data)) {
    total<-sum(data[,2])
    
    # On le partitionne en fonction de la colonne names et on somme
    data<-ddply(data,~ name,function(x) {
      sum(x[,2])
    })
    
    # On le classe et on retourne les individus correspiondant au seuil
    numberContrib<-as.integer((threesold*length(data[,2]))/100)
    data<-data[order(data[,2],decreasing=TRUE),]
    result<-list(
      contrib=data[1:numberContrib,1],
      weight=sum((data[1:numberContrib,2]/total)*100)
    )
  } else {
    result<-FALSE
  }
  return(result)
}

#' Nombre de liens d'une page
#' @param page Le titre de la page
#' @param namespace Les namespace vers lesquels doivent pointer la page. Par défaut 0, celui des articles.
#' @return Un vecteur numérique de dimension 1
#' @export

nbLinks <- function(page,domaine="fr",namespace="0") {
    
    list<-links(page)
    result<-length(list)
    return(result)

}

#' Obtenir la première révision d'une page
#'
#' @param page le titre de la page
#' @param domaine le domaine où est localisé le wiki
#'
#' @return un data-frame à une ligne contenant la date, l'utilisateur et la taille de la révision
#' 
#' @import httr
#' @export
#'

startPage <- function(page,domaine="fr") {
  
  query<-list(
    action="query",
    prop="revisions",
    format="json",
    rvprop="timestamp|user|size",
    rvlimit="1",
    rvdir="newer",
    titles=page)
  
  exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
  content<-tryCatch(content(exec,"parsed")[[2]][[1]][[1]]$revisions[[1]],error=function(e) NULL)
  if(!is.null(content)) {
    
    if("anon" %in% names(content)) anon<-TRUE
    else anon<-FALSE
    
    return(data.frame(user=content$user,anon=anon,date=content$timestamp,size=content$size))
  } else {
    return(NULL)
  }
}