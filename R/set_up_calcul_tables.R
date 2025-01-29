##### fonction pour joindre Placettes et Echantillonnages #####
set_up_calcul_tables <- function(
    df = NULL,
    plot_df = Placettes,
    settings_df = Echantillonnages,
    add_mirror = F
) {
  if (nrow(df) > 0) {
    df <-
      df %>%
      left_join(
        plot_df[, c(
          "NumForet", "NumPlac", "Cycle", "Strate", "PoidsPlacette",
          "Pente", "CoeffPente", "Parcelle", "Station",
          "Miroir_Azimut", "Miroir_Dist", "Miroir_Transect", "Miroir_nb_SsPlac"
        )],
        by = c("NumForet", "NumPlac", "Cycle")
      ) %>%
      left_join(
        settings_df[, c(
          "NumForet", "Cycle", "Strate", "NbPlac",
          "Taillis", "BMP", "TypeTarifBMP", "NumTarifBMP",
          "BMSLineaire", "BMSCercle",
          "NbSousPlac", "RayonSousPlac"
        )],
        by = c("NumForet", "Cycle", "Strate")
      )

    # Cas des transects amputé sur les placettes miroir -
    # gestion des longueurs de transect différentes (2024 FC Canton Genève)
    if ("Transect" %in% names(df)) {
      df <-
        df %>%
        mutate(BMSLineaire = ifelse(
          !is.na(Miroir_Transect),
          Miroir_Transect,
          BMSLineaire
        ))
    }

    # Cas des sous-placettes supprimées car transect amputé sur les placettes miroir -
    # gestion du nombre de placettes différentes (2024 FC Canton Genève)
    if ("SsPlac" %in% names(df)) {
      df <-
        df %>%
        mutate(NbSousPlac = ifelse(
          !is.na(Miroir_nb_SsPlac),
          Miroir_nb_SsPlac,
          NbSousPlac
        ))
    }
  } #else {df}
  if (add_mirror == TRUE) {
    df <- df %>% miroir_add()
  }

  # -- retour de la fonction set_up_calcul_tables
  return(df)
}
