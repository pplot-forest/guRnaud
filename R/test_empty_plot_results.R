
# ----------------------- SIG ---------------------------
##### fonction pour tester si résultats par placettes vides #####
test_empty_plot_results <- function(df, var_results, admin) {
  # Sécurité : détection des résultats "vides" dans la table attributaire
  # initialisation
  num <- unique(df$NumForet)
  empty_values_df <- df %>% select("NumPlac", var_results)

  # détection des lignes contenant des valeurs vides
  empty_values_pos <- c()
  for (col in colnames(empty_values_df)) {
    empty_values_pos <-
      c(empty_values_pos, which(is.na(empty_values_df[, col])) )
  }
  empty_values_pos <- unique(empty_values_pos)

  # mise en forme de la liste des placettes contenant des valeurs vides
  empty_values_plot <- with(empty_values_df, unique(NumPlac[empty_values_pos]))

  # warnings
  if (length(empty_values_plot) > 0) {
    if (length(empty_values_pos) == 1) {
      warning(
        paste0(
          "Il y a des r\u00E9sultats d'analyse vides pour la placette ",
          empty_values_plot,
          " du dispositif ",
          with(admin, unique(Nom)),
          " (placette pr\u00E9sente dans le shape initial mais sans r\u00E9sultats placettes au dernier passage)"
        ),
        call. = FALSE,
        immediate. = TRUE
      )
    } else {
      if (length(empty_values_plot) > 20) {
        warning(
          paste0(
            "Il y a des r\u00E9sultats d'analyse vides pour les placettes\n",
            paste0(empty_values_plot[1:20], collapse = ", "), "...",
            "\ndu dispositif ",
            with(admin, unique(Nom[NumForet == num])),
            " (placettes pr\u00E9sentes dans le shape initial mais sans r\u00E9sultats placettes au dernier passage)"
          ),
          call. = FALSE,
          immediate. = TRUE
        )
      } else {
        warning(
          paste0(
            "Il y a des r\u00E9sultats d'analyse vides pour les placettes\n",
            paste0(empty_values_plot, collapse = ", "),
            "\ndu dispositif ",
            with(admin, unique(Nom[NumForet == num])),
            " (placettes pr\u00E9sentes dans le shape initial mais sans r\u00E9sultats placettes au dernier passage)"
          ),
          call. = FALSE,
          immediate. = TRUE
        )
      }
    }
  }

  # retour de la fonction test_empty_plot_results
  return(df)
}
