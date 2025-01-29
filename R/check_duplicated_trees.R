##### fonction de contôle des arbres dupliqués (arbres repérés az+dist uniquement) #####
check_duplicated_trees <- function(table = NULL) {
  # -- arrange table
  table <- table %>% arrange(NumForet, NumPlac, Cycle, Azimut, Dist)

  # -- valeurs dupliquées
  duplicated_trees <-
    which(duplicated(table[, c("NumForet", "NumPlac", "Cycle", "Azimut", "Dist")]))

  # -- édition d'un classeur listant les valeurs dupliquées
  if (length(duplicated_trees) > 0) {
    # get duplicated trees from last
    duplicated_trees_fromLast <- which(duplicated(
      table[, c("NumForet", "NumPlac", "Cycle", "Azimut", "Dist")],
      fromLast = TRUE
    ))
    # stack row values
    duplicated_trees <- c(duplicated_trees, duplicated_trees_fromLast)
    # duplicated table
    duplicated_trees_table <- table[
      duplicated_trees, c(
        "NumForet", "NumPlac", "Cycle", "NumArbre", "Essence",
        "Azimut", "Dist", "Diam1"
      )] %>%
      arrange(NumForet, NumPlac, Cycle, Azimut, Dist)

    # write table
    write.xlsx(duplicated_trees_table, file = "doublons_calcul_arbres.xlsx")
    # stop message
    stop("Attention : doublon(s) d\u00E9tect\u00E9(s) lors des calculs d'accroissement.\n\nDoublons list\u00E9s dans le classeur excel 'doublons_calcul_arbres.xlsx'")
  }

  # -- return of 'check_duplicated_trees' function
  return(table)
}
