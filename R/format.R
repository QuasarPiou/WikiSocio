#' Divise une liste en vecteur d'une certaine taille
#' @param x La liste a divisé
#' @param number La longueur des divisions
#' @return Une liste de vecteur de longueur number décomposant la liste x
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

#' Compare deux vecteurs
#' @param vector1 Le premier vecteur
#' @param vector2 Le second vecteur
#' @return Un vecteur ne contenant que les valeurs communes
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


#' Executer une requête xPath sur une page
#'
#' @param url l'URL de la page. Attention : différent du titre de la page, il faut l'URL complète !
#' @param xpath la requête à exécuter.
#'
#' @import XML
#' @import RCurl
#' 
#' @return un vecteur texte
#' @export

xPath <- function(url,xpath) {
  
  dom <- htmlParse(getURL(url,followlocation = TRUE, httpheader=c("User-Agent" = "WikiSocio"),encoding="UTF-8"))
  return(xpathSApply(dom,xpath,xmlValue))
  
}