#' Liste des contributions d'un utilisateur
#' 
#' @param user Le pseudonyme de l'utilisateur
#' @param namespace L'espace de nom dont on veut lister les pages, par défaut 0 correspondant à tous les articles
#' @param domaine Le domaine du wiki
#' @param date Une valeur booléenne qui indique si oui ou non on veut les dates des contributions
#' @param weight Une valeur booléenne qui indique si oui ou non on veut le poids de la contribution (ralonge beaucoup le temps de calcul)
#' @return Un data-frame qui contient les données spécifiées par les paramètres
#' @import httr
#' @import pbapply
#' @export
#' 
user<-function(user,namespace="0",domaine="fr",date=TRUE,weight=TRUE) {
  uccontinue<-NULL
  result<-data.frame()
  unname(result)
  
  repeat {
    if(is.null(uccontinue)) {
      query=list(
        action="query",
        list="usercontribs",
        format="json",
        ucnamespace=namespace,
        ucprop="title|timestamp|sizediff",
        uclimit="max",
        ucuser=user)
    } else {
      query=list(
        action="query",
        list="usercontribs",
        format="json",
        ucnamespace=namespace,
        ucprop="title|timestamp|sizediff",
        uclimit="max",
        ucuser=user,
        uccontinue=uccontinue)	
    }
    
    exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
    
    content<-content(exec,"parsed")
    uccontinue<-tryCatch(content$continue$uccontinue,error=function(e) NULL)
    dF<-tryCatch(content$query$usercontribs,error=function(e) NULL)
    
    if(!is.null(dF)) {
      
      # Extraction de toutes les données de la requête
      dF<-lapply(dF,function(x) {
        if(!"sizediff" %in% names(x)) {
          x$sizediff<-NA
        }
        tryCatch(matrix(c(x$title,x$timestamp,x$sizediff),ncol=3,byrow = FALSE),warning=function(w) {
          print(x)
          print("erreur")
        })
      })
      
      dF<-do.call(rbind,dF)
      
      dF<-as.data.frame(dF)
      unname(dF)
      
      result<-rbind(result,dF)
      
    }
    
    
    if(is.null(uccontinue)){
      break
    }
  }	
  
  if(!date) {
    result<-result[,-2]
  }
  
  if(!weight) {
    if(!date) {
      result<-result[,-2]
    } else {
      result<-result[,-3]
    }
  }
  
  if(length(result)==1) {
    result<-as.vector(unique(result))
  }
  return(result)
}

