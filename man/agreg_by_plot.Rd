% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/agreg_by_plot.R
\name{agreg_by_plot}
\alias{agreg_by_plot}
\title{Fonction correspondant à l'étape d'agrégation des résultats par placettes}
\usage{
agreg_by_plot(
  wd = NULL,
  output_dir = NULL,
  forest = NULL,
  last_cycle = NULL,
  combination_table = NULL,
  complete_progress = complete_progress,
  i18n = i18n
)
}
\arguments{
\item{wd}{= répertoire de travail}

\item{output_dir}{= répertoire de sortie}

\item{forest}{= numéro et nom de la forêt}

\item{last_cycle}{= numéro du dernier passage en inventaire}

\item{combination_table}{= table listant les combinaisons d'attributs à utiliser pour agréger les résultats}

\item{complete_progress}{= barre de progression (application shiny)}

\item{i18n}{= fonction de traduction  (application shiny)}
}
\value{
La fonction crée une liste de tables de résultats par placette ("results_by_plot") enregistrée dans l'archive "gfTablesElaboreesPlac.Rdata"
}
\description{
La fonction utilise les résultats de calcul par arbre. Elle permet d'agréger les résultats d'inventaire par placette.
}
