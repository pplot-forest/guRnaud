#' Vérification des noms de feuille du classeur d'inventaire
#' @description La fonction contrôle les intitulés des feuilles du classeur d'inventaire contenant les données.
#' @return La fonction retourne un message en cas d'erreur/éléments manquants.
#' @param file = chemin du fichier d'inventaire
#' @param sheets_names = liste des noms de feuille à vérifier
#' @import dplyr
#' @import openxlsx
#' @import rlang
#' @import tidyr
#' @export
check_workbook_sheets <- function(
    file = NULL, sheets_names = NULL
) {
  # initialize variables
  
  
  # -- paramètres
  # feuilles du classeur d'inventaire
  workbook_sheets <- getSheetNames(file)

  # nombre de feuilles demandées
  sheets_nb <- length(sheets_names)

  # feuilles manquantes
  missing_sheets <- sheets_names[ which(!sheets_names %in% workbook_sheets) ]
  if (length(missing_sheets) > 0) {
    stop(
      "Les noms d'onglet du classeur Excel en import ne sont pas corrects.\n\nRappel : liste des onglets devant figurer dans le classeur (",
      sheets_nb, " au total) =\n'",
      paste0(sheets_names, collapse = "','"),
      "'\nIl manque les onglets :\n",
      paste0(missing_sheets, collapse = ", ")
    )
  }
}
