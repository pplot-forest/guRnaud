##### fonction pour choisir obtenir le numéro du dernier passage en inventaire #####
get_last_cycle <- function(
    object_list = NULL,
    forest_list = NULL
) {
  # -- initialisation
  tmp <- c()
  all_cycle_list <- c()

  # -- liste des numéros de dispositifs
  num_list <- as.numeric( str_sub(forest_list, 1, str_locate(forest_list, "-")[, 1] - 1) )

  # -- boucle
  for (i in 1:length(object_list)) {
    # i = 10 # debug
    # print(i) # debug
    tmp <-
      if (class(object_list) == "list") object_list[[i]] else {
        get(object_list[i], envir = parent.frame())
      }

    # cas où tmp est un data.frame
    if ("data.frame" %in% class(tmp)) {
      if ("NumForet" %in% names(tmp) && "Cycle" %in% names(tmp)) {
        tmp <-
          tmp %>%
          filter(NumForet %in% num_list) %>%
          select(Cycle) %>%
          distinct() %>% unlist() %>% unname()
        tmp <- tmp[!tmp %in% all_cycle_list]
        all_cycle_list <- c(all_cycle_list, tmp)
      }
    }

    # cas où tmp est une liste (ex : TabPla)
    # print(class(tmp)) # debug
    if ("list" %in% class(tmp)) {
      for (i in 1:length(tmp)) {
        tab <- tmp[[i]]
        if ("NumForet" %in% names(tab) && "Cycle" %in% names(tab)) {
          tab <-
            tab %>%
            filter(NumForet %in% num_list) %>%
            select(Cycle) %>%
            distinct() %>% unlist() %>% unname()
          tab <- tab[!tab %in% all_cycle_list]
          all_cycle_list <- c(all_cycle_list, tab)
        }
      }
    }
  }
  all_cycle_list <- sort( as.numeric( unique(all_cycle_list) ) )

  # -- numéro du dernier cycle
  last_cycle <- max(all_cycle_list, na.rm = T)

  # -- retour de la fonction get_last_cycle
  return(last_cycle)
}






