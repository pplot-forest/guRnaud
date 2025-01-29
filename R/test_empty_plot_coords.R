##### fonction pour tester si coordonnées placettes vides #####
test_empty_plot_coords <- function(df, admin) {
  # Sécurité : détection des placettes sans localisation
  # initialisation
  num <- unique(df$NumForet)
  empty_coords_df <- df

  # détection des lignes contenant des valeurs vides
  empty_coords_pos <- which(st_is_empty(df))

  # mise en forme de la liste des placettes contenant des valeurs vides
  empty_coords_plot <- with(empty_coords_df, unique(NumPlac[empty_coords_pos]))

  # warnings
  if (length(empty_coords_plot) > 0) {
    if (length(empty_coords_plot) == 1) {
      warning(
        paste0(
          "La placette ",
          empty_coords_plot,
          " du dispositif ",
          with(admin, unique(Nom[NumForet == num])),
          " n'a pas de coordonnées renseignées dans le shape initial.
          \nLes r\u00E9sultats d'analyse pour cette placette non localis\u00E9e ne figureront pas dans les shapes de r\u00E9sultats."
        ),
        call. = FALSE,
        immediate. = TRUE
      )
    } else {
      if (length(empty_coords_plot) > 20) {
        warning(
          paste0(
            "Les placettes :\n",
            paste0(empty_coords_plot[1:20], collapse = ", "), "...",
            "\ndu dispositif ",
            with(admin, unique(Nom[NumForet == num])),
            " n'ont pas de coordonnées renseignées dans le shape initial.
            \nLes r\u00E9sultats d'analyse pour les placettes non localis\u00E9es ne figureront pas dans les shapes de r\u00E9sultats."
          ),
          call. = FALSE,
          immediate. = TRUE
        )
      } else {
        warning(
          paste0(
            "Les placettes :\n",
            paste0(empty_coords_plot, collapse = ", "),
            "\ndu dispositif ",
            with(admin, unique(Nom[NumForet == num])),
            " n'ont pas de coordonnées renseignées dans le shape initial.
            \nLes r\u00E9sultats d'analyse pour les placettes non localis\u00E9es ne figureront pas dans les shapes de r\u00E9sultats."
          ),
          call. = FALSE,
          immediate. = TRUE
        )
      }
    }
    # enlève les placettes non localisées
    df <- df %>% filter(NumPlac != empty_coords_plot)
  }

  # retour de la fonction test_empty_plot_coords
  return(df)
}
