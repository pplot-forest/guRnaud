% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculs_Arbres.R
\name{calculs_Arbres}
\alias{calculs_Arbres}
\title{Fonction de calcul des résultats pour les arbres}
\usage{
calculs_Arbres(
  df = NULL,
  echant_change = F,
  code_qual = NULL,
  code_essreg = NULL,
  code_tarif = NULL,
  code_prix = NULL,
  diam_cat = NULL
)
}
\arguments{
\item{df}{= table d'inventaire en entrée}

\item{echant_change}{= cet argument indique s'il y a eu un changement de protocole entre 2 inventaires}

\item{code_qual}{= table listant les seuils de qualité}

\item{code_essreg}{= table listant les regroupements d'essence}

\item{code_tarif}{= table listant les tarifs de cubage}

\item{code_prix}{= table listant les prix par essence, classe de diamètre et qualité}

\item{diam_cat}{= table listant les seuils de qualité}
}
\value{
La fonction renvoie la table d'entrée en ayant rajouté les variables de résultats correspondant aux calculs
}
\description{
La fonction calcule les résultats par arbre (Nha, Gha, Vha, ...) en fonction des paramètres 
d'échantillonnage, des tarifs de cubage, des seuils de qualités, etc.
}
