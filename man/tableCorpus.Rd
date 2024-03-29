% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpus.R
\name{tableCorpus}
\alias{tableCorpus}
\title{Créer un tableau de données avec les boite utilisateur affichées sur les pages}
\usage{
tableCorpus(random = TRUE, nb = NULL, names = NULL, domaine = "fr")
}
\arguments{
\item{random}{Une valeur booléenne indiquant si le corpus est consittué de nb utilisateurs tirés au hasard, ou bien si il est spécifié dans names}

\item{nb}{Le cas échant le nombre d'utilisateur à tirer}

\item{names}{Le cas échant un vecteur de caractères listant les utiisateurs du corpus}

\item{domaine}{Le domaine où se trouve le wiki}
}
\value{
Un data-frame avec en lignes les utilisateurs et en colonne les catégories
}
\description{
Créer un tableau de données avec les boite utilisateur affichées sur les pages
}

