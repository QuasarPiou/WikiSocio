#' Trouver l'ensemble des liens d'une page
#' 
#' @param title Le titre de la page
#' @return Une liste de tous les liens de la page
#' @import httr
#' @export
#' 

links <- function (titre,domaine="fr",namespace="0") {
  
  # Téléchargement des liens
  plcontinue<-NULL
  result<-vector(mode="character")
  repeat {
    
    if(is.null(plcontinue)) {
      query=list(
        action="query",
        prop="links",
        format="json",
        plnamespace=namespace,
        pllimit="max",
        titles=titre,
        redirects="")
    } else {
      query=list(
        action="query",
        prop="links",
        format="json",
        plnamespace=namespace,
        pllimit="max",
        titles=titre,
        plcontinue=plcontinue,
        redirects="")
    }
    
    exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
    
    content<-content(exec,"parsed")
    plcontinue<-content$continue$plcontinue
    data<-content$query$pages
    idPage<-data[[1]][1]
    data<-data[[as.character(idPage)]][["links"]]
    data<-sapply(data,function(x) {x[[2]]})
    result<-c(result,data)
    
    if(is.null(plcontinue)){
      break
    }
  }
  
  return(result)
}

#' Vérifie si un lien existe entre deux pages
#' 
#' @param from Le titre de la page dont le lien est supposé partir.
#' @param to Le titre, ou un vecteur contenant plusieurs titres, vers lesquels sont supposés pointer les liens.
#' @return Si to est de longueur 1, la fonction retourne TRUE ou FALSE pour signifier que oui ou non les deux pages sont liés. Si to est un vecteur, la fonction retourne un vecteur listant les différentes pages parmis to avec lesquelles from est lié
#' @import httr
#' @export
#' 

isLink <- function(from,to,domaine="fr") {
  
  # On divise le vecteur to en partie de 50
  to<-divideList(to,50)
  to<-lapply(to,paste,collapse="|")
  # On calcule pour chaque 50 liens (case de to) lesquels sont la cible d'un lien présent sur from.
  result<-lapply(to,function(x){
    data<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=list(
      action="query",
      prop="links",
      format="json",
      pllimit="max",
      pltitles=x,
      titles=from,
      redirects=""))
    
    data<-content(data,"parsed")$query$pages
    idPage<-data[[1]][1]
    data<-data[[as.character(idPage)]]["links"]
    
    if(!is.null(unlist(data))) {
      data<-matrix(unlist(data),ncol=2,byrow=TRUE)
      data[,2]
    }
  })
  return(as.vector(unlist(result)))
}

#' Fais la liste des contributeurs d'une page
#' @param page Le nom de la page à regarder, qui peut appartenir à n'importe quel espace de nom du wiki
#' @return Une liste des contributeurs qui ont contribués à la page
#' @import httr
#' @export
#' 

contribPage <- function(page,domaine="fr") {
  
  pccontinue<-NULL
  result<-vector() 
  repeat {
    if(is.null(pccontinue)) {
      query=list(
        action="query",
        prop="contributors",
        format="json",
        pclimit="max",
        titles=page,
        redirects="")
    } else {
      query=list(
        action="query",
        prop="contributors",
        format="json",
        pclimit="max",
        titles=page,
        redirects="",
        pccontinue=pccontinue)	
    }
    
    exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
    
    content<-content(exec,"parsed")
    pccontinue<-content$continue$pccontinue
    
    # Sélection des données
    data<-content$query$pages
    idPage<-data[[1]][1]
    # VOIR SI IL EST POSSIBLE DE VIRER IDPAGE
    data<-data[[as.character(idPage)]]["contributors"]
    
    if(!is.null(data)) {
      names(data)<-NULL
      data<-matrix(unlist(data),ncol=2,byrow=TRUE)
      data<-data[,2]
      result<-c(result,data)
    }
    
    if(is.null(pccontinue)){
      break
    }
  }
  
  return(result)
  
}

#' Liste toutes les pages membres d'une catégorie
#' @param cat Le nom de la catégorie - sans le préfixe "Catégorie:"
#' @return Un data-frame avec deux colonnes, la première comprenant le nom de la page appartenant à la catégorie, et la seconde donnant le namespace de cette même page.
#' @import httr
#' @export
#' 

catPage <- function(cat,domaine="fr") {
  
  continue<-NULL
  result<-matrix(ncol=2)
  name<-paste("Cat\u00e9gorie:",cat,sep="")
  repeat {
    if(is.null(continue)) {
      query=list(
        action="query",
        list="categorymembers",
        format="json",
        cmtitle=name,
        cmprop="title",
        cmlimit="max"
      )
    } else {
      query=list(
        action="query",
        list="categorymembers",
        format="json",
        cmtitle=name,
        cmprop="title",
        cmlimit="max",
        cmcontinue=continue)	
    }
    
    
    exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
    content<-content(exec,"parsed")
    continue<-content$continue$cmcontinue
    
    data<-tryCatch(content$query$categorymembers,error=function(e) NULL)
    if(!is.null(data)) {
      data<-matrix(unlist(data),ncol=2,byrow=TRUE)
      result<-rbind(result,data)
    }
    if(is.null(continue)){
      break
    }
    
  }
  
  result<-result[-1,]
  return(result)
}

#' Cette fonction est basée sur catPage, et conçue plus spécifiquement pour des catégories ne contenant que des pages utilisateurs
#' @param cat Le nom de la catégorie - sans le préfixe "Catégorie:"
#' @return Un data-frame avec deux colonnes, la première comprenant le nom de la page appartenant à la catégorie, et la seconde donnant le namespace de cette même page.
#' @export
#' 

listUser <- function(cat,domaine="fr") {
  
  data<-as.data.frame(catPage(cat,domaine))
  names(data)<-c("ns","page")
  data<-data[data$ns=="2",]
  data[,1]<-NULL
  data<-sapply(as.vector(data$page),strsplit,":")
  names(data)<-NULL
  
  list<-matrix(unlist(data),ncol=2,byrow=TRUE)[,2]
  return(list)
  
}

#' Une liste de toutes les révisions d'une page
#' @param page Le titre de la page
#' @return Un data-frame avec 5 colonnes : 1) l'utilisateur qui a fait la révision, 2) la date de la révision (sous forme de timestamp), 3) la taille en octet de la révision, 4) une valeur booléenne indicant si, oui ou non, la revision est faite par un utilisateur anonyme, 5) la taille de la révision
#' @export
#' 

revisionsPage <- function(page,domaine="fr") {
  
  continue<-NULL
  result<-data.frame(matrix(ncol = 5))
  names(result)<-c("user","timestamp","size","anon","weight")
  repeat {
    if(is.null(continue)) {
      query=list(
        action="query",
        prop="revisions",
        format="json",
        rvlimit="max",
        rvprop="user|timestamp|size",
        titles=page,
        redirects="")
    } else {
      query=list(
        action="query",
        prop="revisions",
        format="json",
        rvlimit="max",
        rvprop="user|timestamp|size",
        titles=page,
        rvcontinue=continue,
        redirects="")	
    }
    
    exec<-tryCatch(GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query),error=function(e) NULL)
    if(!is.null(exec)) {
      content<-content(exec,"parsed")      
    } else {
      content<-NULL
    }
    continue<-tryCatch(content$continue$rvcontinue,error=function(e) NULL)
    
    # Sélection des données
    content<-tryCatch(content[["query"]][[1]][[1]][[4]],error=function(e) NULL)
    
    if(!is.null(content)) {
      user<-sapply(content,function(x){
        x$user
      })
      timestamp<-sapply(content,function(x){
        x$timestamp
      })
      size<-sapply(content,function(x){
        x$size
      })
      anon<-sapply(content,function(x){
        if(is.null(x$anon)) FALSE
        else TRUE
      })
      data<-data.frame(user,timestamp,size,anon)
      if(length(data$size)>1) {
        data$weight<-c(data[1:(nrow(data)-1),3]-data[2:nrow(data),3],data[nrow(data),3])
      } else {
        data$weight<-data$size
      }
      result<-rbind(result,data)
    }
    if(is.null(continue)){
      break
    }
    
  }
  result<-result[-1,]
  return(result)
}
