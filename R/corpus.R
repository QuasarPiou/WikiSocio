#' Dégage la liste des articles écrits par un groupe de contribteurs
#' @param cat La catégorie où se trouvent les pages utilisateurs concernées
#' @import pbapply
#' @import plyr
#' @return Un vecteur d'article
#' @export

createCorpus<-function(cat,limit="max",clean=T,threesold=50,log=T) {
  
    # Liste des contributeurs
    list<-listUser(cat)
    if(limit != "max") {
      list<-list[1:limit]
    }
    
    print("Extraction de la liste des articles...")
    corpus<-pblapply(list,function (x) {
      if(log) {
        write.csv(x,append=TRUE,file="articleLog.csv")
      }
      article<-user(x,date=FALSE,weight=FALSE)
      names<-rep(x,length(article))
      data.frame(names=names,article=article)
    })
    corpus<-do.call(rbind,corpus)
  
  if(clean) {

    # On a un data-frame qui reprend les utilisateurs et leurs articles. Il faut le couper en plusieurs dF en fonction des utilisateurs, et les traiter tous un par un.
    print("Extraction de la liste des révisions...")
    
    list<-pblapply(unique(corpus$article),function(x) {
      if(log) {
        write.csv(as.character(x),append=TRUE,file="revisionsLog.csv")
      }
      revisionsPage(as.character(x))
    })
    names(list)<-unique(corpus$article)
    
    print("Extraction des listes de premiers contributeurs...")
    
    rank<-pblapply(unique(corpus$article),function(x,list,threesold) {
      if(log) {
        write.csv(as.character(x),append=TRUE,file="rankLog.csv")
      }
      rankingContrib(list[as.character(x)],threesold)
    },list,threesold)
    names(rank)<-unique(corpus$article)
    
    print("Nettoyage du corpus...")
    
    corpus$weight<-pbapply(corpus,1,function(x,rank) {
      if(log) {
        write.csv(as.character(x),append=TRUE,file="cleanLog.csv")
      }
      x[1] %in% rank[[as.character(x[2])]]$contrib
    },rank)
    result<-unique(as.vector(corpus[corpus$weight==TRUE,2]))
    
    return(result)
    
  } else {
    return (corpus)
  }
}

#' Selectionner un corpus d'articles de manière aléatoire.
#' 
#' @param nb Le nombre d'article à obtenir
#'
#' @param domaine le domaine du wiki de Wikipédia où l'on veut sélectionner les données
#' @param namespace le namespace des articles 
#'
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
        rnnamespace=namespace,
        redirects="")
      
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
      rnnamespace=namespace,
      redirects="")
    
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

#' Construit un tableau de données à partir d'un corpus
#' 
#' @import pbapply
#' @export

dataTable <- function (corpus,selection=c("nbLinks","nbContrib","nbRevisions","percentAnon","percent10")) {
  if("nbLinks" %in% selection) {
    print("Nombre de liens")
    corpus$nbLiens<-pbsapply(corpus$article,function(x) {
      return(tryCatch(nbLinks(as.character(x)),error=function(e) NULL))
    })
  }
  
  if("nbContrib" %in% selection) {
    print("Nombre de contributions")
    corpus$nbContrib<-pbsapply(corpus$article,function(x) {
      print(as.character(x))
      result<-contribPage(as.character(x))
      return(tryCatch(length(result),error=function(e) NULL))
    })
  }
  
  print("Extraction de la liste des révisions")
  list<-pblapply(corpus$article,function(x) {
    return(tryCatch(revisionsPage(as.character(x)),error=function(e) NULL))
  })
  names(list)<-corpus$article
  
  if("nbRevisions" %in% selection) {
    print("Nombre de révisions")
    corpus$nbRevisions<-pbsapply(corpus$article,function(x,list){
      return(tryCatch(length(as.data.frame(list[as.character(x)])[,1]),error=function(e) NULL))
    },list)
  }
  
  if("percentAnon" %in% selection) {
    print("Pourcentage des contributions anonymes")
    corpus$percentAnon<-pbsapply(corpus$article,function(x,list) {
      data<-percentAnon(list[as.character(x)])
      return(tryCatch(data,error=function(e) NULL))
    },list)
  }
  
  if("percent10" %in% selection) {
    print("Poids des 10% de premiers contributeurs")
    corpus$weightContrib<-pbsapply(corpus$article,function(x,list) {
      data<-rankingContrib(list[as.character(x)],10)
      if(is.null(data$weight)) {
        return(NULL)
      } else {
        return(data$weight)
      }
    },list)
  }
  
  return(corpus)
}

#' Créer un tableau de données avec les boite utilisateur affichées sur les pages
#'
#' @param random Une valeur booléenne indiquant si le corpus est consittué de nb utilisateurs tirés au hasard, ou bien si il est spécifié dans names
#' @param nb Le cas échant le nombre d'utilisateur à tirer
#' @param names Le cas échant un vecteur de caractères listant les utiisateurs du corpus
#' @param domaine Le domaine où se trouve le wiki
#' 
#' @import httr
#' @import stringr
#' @import pbapply
#'
#' @return Un data-frame avec en lignes les utilisateurs et en colonne les catégories
#' @export

tableCorpus <- function(random=TRUE,nb=NULL,names=NULL,domaine="fr") {
  
  if(random) {
    names<-selectRandom(nb,namespace="2")
    names<-str_split_fixed(names, "/", 2)[, 1]
  }
  
  print("Téléchargement des caractéristiques des utilisateurs.")
  corpus<-pblapply(names,function(x) {
    if(grepl(x,"Utilisateur:")) {
      x<-paste("utilisateur",x,sep=":")
    }
    cat<-pageCat(x)
    cat[grepl("Utilisateur ",cat)]
  })
  
  names(corpus)<-names
  
  dimDF<-unique(unlist(corpus))
  
  print("Mise en forme du tableau de données")
  data<-pblapply(corpus,function(x,dimDF) {
    as.vector(sapply(dimDF,function(y,x) {
      if(y %in% x) TRUE
      else FALSE
    },x))
  },dimDF)
  
  data<-as.data.frame(do.call(rbind,data))
  names(data)<-str_replace(dimDF,"Catégorie:Utilisateur","")
  
}