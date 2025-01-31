#' Fonction d'agrégation des résultats par placettes
#' @description La fonction permet d'agréger les résultats d'inventaire par placette
#' @return La fonction retourne une table d'agrégation des résultats
#' @param df = table d'inventaire d'entrée
#' @param group_var = groupe à utiliser pour agréger les données
#' @param vars = variagbles à agréger
#' @import dplyr
#' @export
aggregate_tables_by_plot <- function(
    df = NULL, group_var = NULL, vars = NULL
) {
  if (nrow(df) > 0) {
    df <- df %>% group_by_at(group_var) %>% summarise_at(vars, sum, na.rm = T) %>% ungroup()
  } else {
    df <- df %>% select(all_of(group_var), all_of(vars)) #%>%
    # mutate(
    #   NumDisp = as.numeric(NumDisp), # TODO : à régler plus en amont
    #   NumPlac = as.character(NumPlac),
    #   Cycle = as.numeric(Cycle)
    # )
  }
  df <- df %>% data.frame()

  # -- return of aggregate_tables_by_plot function
  return(df)
}
