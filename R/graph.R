#' Make the graph of the links of one page
#' 
#' @param title the title of a page
#' @param domain the wiki of a page
#' @return A graph object
#' @import igraph
#' @export
#' 

graphArticle <- function(titre,domaine="fr",namespace="0") {

	listLinks<-links(titre,domaine,namespace)
	listLinks<-c(listLinks,titre)
	graph<-graphArticleGroup(listLinks,domaine=domaine,namespace=namespace)
	return(graph)
}

#' @import igraph
#' @import pbapply
#' @export

graphArticleGroup <- function(group,domaine="fr",namespace="0") {
	
	# création de l'objet igraph, et ajout des sommets
	graph <- make_empty_graph()
	graph <- add_vertices(graph,length(group),attr=list(title=group,type="contibutors"))
	
	# pour chacune des valeurs de group, regarde sur les autres valeurs de group les liens qui existent
	edgelist<-pbsapply(group,isLink,group)
	edgelist<-sapply(edgelist,match,group)
	names(edgelist)<-NULL
	
	# mise en forme
	firstRow<-vector()
	for(i in 1:length(edgelist)) {
		firstRow<-c(firstRow,rep(i,length(edgelist[[i]])))
	}
	secondRow<-unlist(edgelist)
	edgelist<-matrix(c(firstRow,secondRow),ncol=length(firstRow),byrow=TRUE)
	
	# On ajoute tous ces liens au graphe et on le retourne
	graph <- add.edges(graph,edgelist)
	return(graph)
	
}

#' @import igraph
#' @export

addContrib <- function(graph) {
	
	# On récupère le graphe et les attribus des noeuds
	articles<-V(graph)$title
	
	# Pour chacun des titres, on cherche la liste des contributeurs, et on les ajoute au graph que l'on retourne
	contrib<-sapply(articles,contribPage)
	
	return(contrib)
	# AJOUTER LES LIENS PLUS COMPLIQUER QUE PREVU, IL FAUT POUVOIR AJOUTER UN SOMET UNIQUEMENT S'IL N'EXISTE PAS
	
}