% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list.R
\name{listUser}
\alias{listUser}
\title{Cette fonction est basée sur catPage, et conçue plus spécifiquement pour des catégories ne contenant que des pages utilisateurs}
\usage{
listUser(cat, domaine = "fr")
}
\arguments{
\item{cat}{Le nom de la catégorie - sans le préfixe "Catégorie:"}
}
\value{
Un data-frame avec deux colonnes, la première comprenant le nom de la page appartenant à la catégorie, et la seconde donnant le namespace de cette même page.
}
\description{
Cette fonction est basée sur catPage, et conçue plus spécifiquement pour des catégories ne contenant que des pages utilisateurs
}

