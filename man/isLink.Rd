% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list.R
\name{isLink}
\alias{isLink}
\title{Vérifie si un lien existe entre deux pages}
\usage{
isLink(from, to, domaine = "fr")
}
\arguments{
\item{from}{Le titre de la page dont le lien est supposé partir.}

\item{to}{Le titre, ou un vecteur contenant plusieurs titres, vers lesquels sont supposés pointer les liens.}
}
\value{
Si to est de longueur 1, la fonction retourne TRUE ou FALSE pour signifier que oui ou non les deux pages sont liés. Si to est un vecteur, la fonction retourne un vecteur listant les différentes pages parmis to avec lesquelles from est lié
}
\description{
Vérifie si un lien existe entre deux pages
}

