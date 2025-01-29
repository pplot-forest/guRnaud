##### fonction pour choisir le dispositif à traiter #####
choose_forest <- function(
    object_list, admin = Forets, check_all_msg = NULL,
    df_2_test = NULL
) {
  # -- initialisation
  tmp <- c()
  all_num_list <- c()

  # -- loop for each object : get data and find the NumForet
  for (i in 1:length(object_list)) {
    # i = 15 # debug
    tmp <-
      if (class(object_list) == "list") object_list[[i]] else {
        get(object_list[i], envir = parent.frame())
      }

    # cas où tmp est un data.frame
    if ("data.frame" %in% class(tmp)) { # si sf présent -> plusieurs class
      if ("NumForet" %in% names(tmp)) {
        tmp <-
          tmp %>% select(NumForet) %>% distinct() %>% unlist() %>% unname()
        tmp <- tmp[!tmp %in% all_num_list]
        all_num_list <- c(all_num_list, tmp)
      }
    }

    # cas où tmp est une liste (ex : TabPla)
    if ("list" %in% class(tmp)) {
      for (tab in tmp) {
        if ("NumForet" %in% names(tab)) {
          tab <-
            tab %>% select(NumForet) %>% distinct() %>% unlist() %>% unname()
          tab <- tab[!tab %in% all_num_list]
          all_num_list <- c(all_num_list, tab)
        }
      }
    }
  } # end of loop object_list
  all_num_list <- sort( as.numeric( unique(all_num_list) ) )

  if (is.element(NA, all_num_list)) warning("NumForet vide d\u00E9tect\u00E9")
  all_forest_list <- paste0(
    all_num_list, "-", admin$Nom[match(all_num_list, admin$NumForet)]
  )

  # -- choix de la forêt
  forest_list <- tk_select.list(
    choices = c(check_all_msg, as.character(all_forest_list)),
    multiple = T,
    title = "Choisir une ou plusieurs for\u00EAts"
  )
  # -- sortie si aucun choix fait
  if (length(forest_list) == 0) stop("traitement interrompu - aucune for\u00EAt choisie", call. = FALSE)
  if (is.element(check_all_msg, forest_list)) {forest_list = all_forest_list}

  if (!is.null(df_2_test)) {
    # test si les numéros de forest_list sont présents dans la table df_2_test
    num_list <-
      as.numeric( str_sub(forest_list, 1, str_locate(forest_list, "-")[, 1] - 1) )
    pos <- which(!num_list %in% df_2_test$NumForet)
    if (length(pos) > 0) {
      stop(
        "Can't find the chosen forest number(s) '",
        paste0(num_list[pos], collapse = ", "),
        "' within the '", substitute(df_2_test), "' table"
      )
    }
  }

  # -- retour de la fonction choose_forest
  return(forest_list)
}
