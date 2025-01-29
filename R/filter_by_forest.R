
##### fonction pour filtrer les tables selon une liste de dispositifs #####
filter_by_forest <- function(
    tables = NULL,
    forest_list = NULL,
    cycle = NULL
) {
  # -- liste des numéros de dispositifs
  num_list <- as.numeric( str_sub(forest_list, 1, str_locate(forest_list, "-")[, 1] - 1) )

  for (tmp in tables) {
    # print(tmp) # debug
    # print(class(tmp)) # debug
    # tmp <- tables[[7]] # debug

    # cas où tmp est un nom de table
    # -- nom de la table
    tmp_NAME <- tmp
    # -- data.frame
    tmp <- get(tmp_NAME, envir = parent.frame())

    if ("data.frame" %in% class(tmp)) {
      if (dim(tmp)[1] > 0) {
        # -- filtre selon la liste de dispositifs sélectionnés
        if ("NumForet" %in% names(tmp)) {
          tmp <- tmp %>% filter(NumForet %in% num_list)
        }

        # -- filtre selon le cycle
        if ("Cycle" %in% names(tmp)) {
          tmp <- tmp %>% filter(Cycle <= cycle)
        }

        # if (cycle == 1) {
        #   tmp <-
        #     tmp %>%
        #     mutate(
        #       AcctGper = NULL,
        #       AcctVper = NULL,
        #       AcctD = NULL
        #     )
        # }
      } # end of cond 'dim(tmp)[1] > 0'
    } # end of cond '"data.frame" %in% class(tmp)'

    # cas où tmp est une liste
    if ("list" %in% class(tmp)) {
      for (i in 1:length(tmp)) {
        if (dim(tmp[[i]])[1] > 0) {
          # -- filtre selon la liste de dispositifs sélectionnés
          if ("Cycle" %in% names(tmp[[i]])) {
            tmp[[i]] <- tmp[[i]] %>% filter(NumForet %in% num_list)
          }

          # -- filtre selon le cycle
          if ("Cycle" %in% names(tmp[[i]])) {
            tmp[[i]] <- tmp[[i]] %>% filter(Cycle <= cycle)
          }

          # if (cycle == 1) {
          #   tmp[[i]] <-
          #     tmp[[i]] %>%
          #     mutate(
          #       AcctGper = NULL,
          #       AcctVper = NULL,
          #       AcctD = NULL
          #     )
          # }
        }
      } # end of loop length(tmp)
    } # end of cond '"list" %in% class(tmp)'

    # retour de la fonction filter_by_forest : assign tmp_NAME to tmp
    assign(tmp_NAME, tmp, envir = parent.frame())
  }
}
