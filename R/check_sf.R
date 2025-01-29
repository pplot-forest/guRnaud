
##### fonction de vérification du sf #####
# contrôle : 1/ présence des colonnes NumForet et NumPlac
#            2/ valeurs vides dans les colonnes NumForet et NumPlac
#            3/ coordonnées vides ?
check_sf <- function(sf) {
  # intitulé recherchés par défaut
  NumForet_label <- "NumForet"
  NumPlac_label <- "NumPlac"

  # ----- 1/ contrôle des colonnes NumForet et NumPlac -----
  if (!NumForet_label %in% names(sf)) {
    # intitulé
    title_msg <- paste0(
      str_wrap("L'intitulé de colonne pour les numéros de dispositif n'est pas reconnu dans le fichier", 70),
      " ", basename(file_path_sans_ext(sf_path)),
      " ('", NumForet_label, "' recherché).\n\n               Choisissez l'attribut désignant NumForet"
    )

    # choix
    choices_msg <-
      names(sf)[!names(sf) %in% c(NumForet_label, NumPlac_label, "geometry")]
    # sécurité sur les choix
    if (length(choices_msg) == 0) stop("Plus aucun attribut de colonne disponible !")

    # fenêtre de dialogue
    NumForet_label <- tk_select.list( # NumForet_label : NumForet0 anciennement
      title = title_msg,
      choices = choices_msg,
      multiple = F
    )
  } else if (!NumPlac_label %in% names(sf)) {
    # intitulé
    title_msg <- paste0(
      str_wrap("L'intitulé de colonne pour les numéros de placettes n'est pas reconnu dans le fichier", 70),
      " ", basename(file_path_sans_ext(sf_path)),
      " ('", NumPlac_label, "' recherché).\n\n               Choisissez l'attribut désignant NumPlac"
    )

    # choix
    choices_msg <-
      names(sf)[!names(sf) %in% c(NumForet_label, NumPlac_label, "geometry")]
    # sécurité sur les choix
    if (length(choices_msg) == 0) stop("Plus aucun attribut de colonne disponible !")

    # fenêtre de dialogue
    NumPlac_label <- tk_select.list( # NumPlac_label : NumPlac0 anciennement
      title = title_msg,
      choices = names(sf),
      multiple = F
    )
  }

  sf <-
    sf %>%
    select(NumForet_label, NumPlac_label) %>%
    rename(
      "NumForet"= NumForet_label,
      "NumPlac"= NumPlac_label
    )

  # ----- 2/ contrôle de valeurs vides dans les colonnes NumForet et NumPlac -----
  empty_pos <- with(sf, which(is.na(NumForet) | is.na(NumPlac)))
  if (length(empty_pos) > 0) {
    stop(
      "Il y a des valeurs (",
      length(empty_pos),
      ") vides dans les colonnes désignant le(s) numéro(s) de dispositif et les numéros de placettes"
    )
  }

  # ----- 3/ contrôle de geometry vides
  empty_pos <- which(st_is_empty(sf))
  if (length(empty_pos) > 0) {
    stop(
      "Il y a des placettes (",
      length(empty_pos),
      ") non localisées"
    )
  }

  # ----- 4/ table finale -----
  sf <-
    sf %>%
    mutate(
      NumForet = as.numeric(NumForet),
      NumPlac = as.character(NumPlac)
    )

  # retour de la fonction check_sf
  return(sf)
}
