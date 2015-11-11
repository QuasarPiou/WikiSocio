#' Download a list of all the links in a Wikipedia page
#' 
#' @param title The title of the page
#' @param domaine The domain of the wiki. For example, for the english wikipedia, the domain is "en" (because the adress of the whole website is http://en.wikipedia.org/)
#' @return A list of all links of a page
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
			titles=titre)
	} else {
			query=list(
			action="query",
			prop="links",
			format="json",
			plnamespace=namespace,
			pllimit="max",
			titles=titre,
			plcontinue=plcontinue)
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

#' Check if a link exists between two pages
#' 
#' @import httr miscTools
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
    titles=from))
	
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

#' Retourne un vecteurs avec les pages auxquelles le contributeur contribue
#' @import httr
#' @export
#' 
contribUser<- function(user) {

	data<-user(user)
	result<-unique(data[,1])
	return(result)
	
}

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
			titles=page)
	} else {
		query=list(
			action="query",
			prop="contributors",
			format="json",
			pclimit="max",
			titles=page,
			pccontinue=pccontinue)	
	}
	
	exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
  
	content<-content(exec,"parsed")
	pccontinue<-content$continue$pccontinue
	
	# Sélection des données
	data<-content$query$pages
	idPage<-data[[1]][1]
	data<-data[[as.character(idPage)]]["contributors"]
	names(data)<-NULL
	data<-matrix(unlist(data),ncol=2,byrow=TRUE)
	data<-data[,2]
	result<-c(result,data)
	
	if(is.null(pccontinue)){
		break
	}
  }
  
  return(result)

}

#' Liste tous les membres d'une catégorie
#' @import httr
#' @export
#' 

catPage <- function(cat,domaine="fr") {
    
	continue<-NULL
	result<-matrix(ncol=2)
	name<-paste("Cat%E9gorie:",cat,sep="")
	print(name)
	repeat {
		if(is.null(continue)) {
			query=list(
				action="query",
				list="categorymembers",
				format="json",
				cmtitle=URLdecode(name),
				cmprop="title",
				cmlimit="max"
			)
		} else {
			query=list(
				action="query",
				list="categorymembers",
				format="json",
				cmtitle=URLdecode(name),
				cmprop="title",
				cmlimit="max",
				cmcontinue=continue)	
		}
	
        
		exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
		content<-content(exec,"parsed")
        
		continue<-content$continue$cmcontinue
	
		data<-content$query$categorymembers
		data<-matrix(unlist(data),ncol=2,byrow=TRUE)
	
		result<-rbind(result,data)
	
		if(is.null(continue)){
			break
		}

	}
	
	result<-result[-1,]
	return(result)
}

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

#' @export
#' 

revisionsPage <- function(page,domaine="fr") {
    
continue<-NULL
result<-matrix(ncol=3) 
repeat {
  if(is.null(continue)) {
    query=list(
      action="query",
      prop="revisions",
      format="json",
      rvlimit="max",
      rvprop="user|timestamp|size",
      titles=page)
  } else {
    query=list(
      action="query",
      prop="revisions",
      format="json",
      rvlimit="max",
      rvprop="user|timestamp|size",
      titles=page,
      rvcontinue=continue)	
  }
  
  exec<-GET(paste("https://",domaine,".wikipedia.org/w/api.php",sep=""),query=query)
  
  content<-content(exec,"parsed")
  continue<-content$continue$rvcontinue
  
  # Sélection des données
  content<-content[[2]][[1]][[1]][[4]]
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
  
  if(is.null(continue)){
    break
  }
        
}
}