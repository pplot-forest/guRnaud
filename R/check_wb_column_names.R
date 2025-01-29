##### fonction d'import des tables d'inventaire #####
check_wb_column_names <- function( # former read_GF_table
    file = NULL,
    table_name = NULL,
    column_to_check = NULL
) {
  # column_to_check <- c("NumForet", "Nom", "Propri\u00E9taire", "Gestionnaire", "SurfForet") # debug
  # -- parameters
  # number of columns
  column_nb <- length(column_to_check)

  # -- reading table
  table <- read.xlsx(file, sheet = table_name) %>% select(one_of(column_to_check))
  table_names <- names(table)

  # checking columns names
  missing_columns <- column_to_check[ which(!column_to_check %in% table_names) ]
  if (length(missing_columns) > 0) {
    stop(
      "Les noms de colonnes de l'onglet ", !!table_name,
      " ne sont pas corrects.\n\nRappel : liste des colonnes de l'onglet Foret (",
      column_nb, " au total) =\n'", paste0(column_to_check, collapse = "','"), "'"
    )
  }

  # -- retour de la fonction read_GF_table
  return(table)
}
