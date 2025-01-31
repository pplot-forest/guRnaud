#' Fonction d'agrégation des résultats par ensemble
#' @description La fonction permet d'agréger les résultats à différentes échelles (forêt,
# strate d'échantillonnage, ...)
#' @return La fonction crée une liste de tables de résultats par ensemble ("results_by_group") enregistrée dans l'archive "gfTablesElaborees.Rdata"
#' @param wd = répertoire de travail
#' @param output_dir = répertoire de sortie
#' @param combination_table = table listant les combinaisons de variables à suivre pour agréger les résultats
#' @param forest = numéro et nom de la forêt
#' @param last_cycle = numéro du dernier passage en inventaire
#' @param complete_progress = barre de progression (application shiny)
#' @param i18n = fonction de traduction  (application shiny)
#' @import dplyr
#' @import shiny
#' @import rlang
#' @import tidyr
#' @importFrom stats na.omit
#' @export
agreg_by_stand <- function(
  wd = NULL,
  output_dir = NULL, # à améliorer
  combination_table = NULL,
  # lang = "FRA",
  forest = NULL, last_cycle = NULL,
  complete_progress = NULL,
  i18n = NULL
) {
  # initialize variables
  Cycle <- Foret <- Forets <- Nbre <- NbreAcct <- Nom <- NumForet <- NumPlac <- Observations <- NULL
  Poids <- PoidsAcct <- PoidsPlacette <- Regroups <- gf_AgregMoySdEr <- results_by_plot <- NULL
  
  # -- switch shiny ***
  # incrémentation de la barre de progression
  # incProgress(detail = i18n()$t("Agrégation des résultats par dispositif..."))
  # *** --

  ##### 1/ Initialisation #####
  # -- création du dossier de sortie
  # output_dir <- file.path( wd, "out", clean_names(disp) )
  # # output_dir <- file.path("out", disp, i18n()$t("livret_AFI"))
  # output_dir <- file.path(output_dir, "tables")
  dir.create(output_dir, showWarnings = F, recursive = T)

  # -- chargement des données d'inventaire et administratives
  inventory_tables <- load(file.path(wd, "tables/DonneesBrutes.Rdata"))
  # -- chargement du dictionnaire de traduction
  load( file.path(wd, "tables/dictionary.Rdata") )
  # -- chargement des résultats par arbre
  results_by_plot_data <- load(file.path(output_dir, "tables/gfTablesElaboreesPlac.Rdata"))

  # df_list <- load(file.path(repSav, "tables/gfTablesElaboreesPlac.Rdata"))

  # # -- chargement des résultats de gf_Calculs()
  # if (repSav == repGF) {
  #   # -- choix du dispositif
  #   # initialisation
  #   check_all_msg <- "Editer les r\u00E9sultats pour tous les dispositifs"
  #   forest_list <- choose_forest(df_list, Forets, check_all_msg) # TODO : laisser le choix du dispositif ?(même si déjà fait au job4)
  # } else {
  #   forest_list <- forest
  # }
  # last_cycle <- get_last_cycle(df_list, forest_list)
  #
  # # -- filtre des tables d'inventaire en fonction des numéros de dispositif sélectionnés
  # # Placettes,IdArbres,ValArbres,PCQM,Reges,Transect,BMSsup30,Reperes,Cycles
  # tables <- "results_by_plot"
  # filter_by_forest(tables, forest_list, last_cycle)
  # filter_by_forest("Cycles", forest_list, last_cycle)

  #  Définition de count_vars_by_pop
  count_vars_by_pop <- data.frame(
    Pop = c(
      "Fpied", "Den", "PFutaie", "Exploit", "Per", "Taillis", "Codes",
      "Rege", "BM", "BMS", "BMP"
    ),
    data = c(
      rep("Dendro", 4), "Per", "Taillis", "Codes", "Rege", "BM", "BMS", "BMP"
    )
  )


  # -- Problème calcul des accroissements (rajout de l'accroissement sur la moitié de la périodes aux arbres coupés)
  # df4 <- df %>%
  #   filter(EssReg == "Hêtre" & Cat == "GB") %>%
  #   group_by(NumForet) %>%
  #   summarise(AcctGper = sum(AcctGper, na.rm = T) / 153) %>%
  #   ungroup()

  ##### 2/ Boucle(s) - agrégations par ensembles #####
  # --- Paramètres par défaut (dont projet PP Tétras)
  TabData <-
    data_frame(
      Pop = c( # var
        "Tot", "Fpied", "Den", "PFutaie", "Exploit", "Per", "Taillis", "Codes",
        "Rege", "BM", "BMS", "BMP"
      ),
      data = c(rep("Dendro", 6), "Taillis", "Codes", "Rege", "BM", "BMS", "BMP")
    )

  results_by_group <- c()

  # --- Agrégations par ensembles (fixée à forêt pour application SAIC actuelle)
  # Table listant les ensembles à prendre en compte (individuellement) dans l'agrégation (1:forêt)
  # combination_table <-
  #   data.frame(
  #     V1 = "Foret", V2 = NA, V3 = NA, V4 = NA, V5 = NA,
  #     V6 = NA, V7 = NA, V8 = NA, V9 = NA,
  #     stringsAsFactors = F
  #   )
  # combination_table <-
  #   data.frame(
  #     V1=c("Foret", NA), V2=c(NA, "Groupe2"), V3=NA, V4=NA, V5=NA, V6=NA, V7=NA, V8=NA, V9=NA,
  #     stringsAsFactors = F
  #   )
  # combination_table <-
  #   data.frame(
  #     V1=c("Foret", NA), V2=c(NA, "Parcelle"), V3=NA, V4=NA, V5=NA, V6=NA, V7=NA, V8=NA, V9=NA,
  #     stringsAsFactors = F
  #   )

  for (i in 1:nrow(combination_table)) { # loop 'i in 1:nrow(combination_table)'
    # print(paste0("i = ",i))
    group <- combination_table[i, 1:ncol(combination_table)] %>% unlist() %>% unname() %>% na.omit()
    results_by_group_tmp <- c()

    # if (!"Foret" %in% group) {
    #   group <- c("Foret", group)
    # }
    results_by_group_tmp <- c()

    # ----- Tables pondération - distinction stock et accroissement
    # DernierCycle <- max(as.numeric(Placettes$Cycle),na.rm=T)

    # join forest name
    if (i == 1) {
    Placettes <-
      Placettes %>%
      left_join(Forets[, c("NumForet", "Nom")], by = "NumForet") %>%
      rename(Foret = Nom)
    }
    # regroupement de la table Placettes et de la table Regroups
    regroup_DF <-
      Placettes %>%
      select(-Observations) %>%
      left_join(Regroups, by = c("NumForet", "NumPlac", "Cycle")) %>%
      select(-Observations)
    # table contenant les nombres et les poids des placettes des différents cycles
    # (importance de PoidsAcct et NbreAcct pour cycle > 1)
    ponderation_DF <-
      regroup_DF %>%
      select("NumForet", "Foret", all_of(group), "NumPlac", "Cycle", "PoidsPlacette") %>%
      rename(Poids = PoidsPlacette) %>%
      mutate(Nbre = 1) %>%
      # on complète les placettes qui seraient éventuellement absentes
      # (indispensables pour faire les bons calculs d'accroissement)
      group_by(NumForet, Foret) %>%
      complete(NumPlac, Cycle) %>%

      arrange(NumForet, Foret, NumPlac, Cycle) %>%
      group_by(NumForet, Foret, NumPlac) %>%
      mutate(
        NbreAcct = ifelse(
          Cycle > 1,
          (Nbre + lag(Nbre)) / 2,
          (Nbre + lead(Nbre)) / 2
          ),
        PoidsAcct = ifelse(
          Cycle > 1,
          (Poids + lag(Poids)) / 2,
          (Poids + lead(Poids)) / 2
          ),

        NbreAcct = ifelse(last_cycle > 1, NbreAcct, Nbre),
        PoidsAcct = ifelse(last_cycle > 1, PoidsAcct, Poids)
      ) %>%
      ungroup() %>%
      # mutate(NumPlac = as.numeric(NumPlac)) %>% modif 03/05/2024

      # agrégation :
      group_by_at(c("NumForet", group, "Cycle")) %>%
      summarise_at(c("Poids", "Nbre", "PoidsAcct", "NbreAcct"), sum, na.rm = T) %>%
      ungroup() %>%
      data.frame()

    if (last_cycle == 1) {
      regroup_DF <- regroup_DF %>% mutate(NbreAcct = NULL, PoidsAcct = NULL)
    }

    for (j in 1:length(results_by_plot)) {
      # j=41 # debug
      # j=90 # debug
      # print(j) # debug

      # -- paramètres de regroupement
      # nom de la table des résultats par placettes
      table_name <- names(results_by_plot[j])
      # nom de la population
      # data <- str_split(table_name, "_")[[1]][1] %>% str_sub(6, -1)
      pop <- str_sub(table_name, 6, str_locate(table_name, "_")[, 1] - 1)
      data <- unique(TabData$data[TabData$Pop %in% pop])
      # table de résultats par placette
      table <- results_by_plot[[j]]

      # table de résultats par ensemble
      results_table <- gf_AgregMoySdEr(
        df = table,
        group = group,
        data = data,
        last_cycle = last_cycle,
        ponderation_DF = ponderation_DF,
        regroup_DF = regroup_DF
      )

      # group <- "Foret" # debug
      # data <-  data # debug
      results_by_group_tmp <- c(results_by_group_tmp, list(results_table))

      # table name
      attributes <- str_sub(table_name, str_locate(table_name, "_")[, 1] + 1, -1)
      table_name <- paste0(
        "gf", paste0(group, collapse = ""), pop, "_",
        attributes
      )

      # assign name
      names(results_by_group_tmp)[j] <- table_name

      # incrémentation de la barre de progression
      # -- switch shiny ***
      incProgress(amount = 1 / complete_progress)
      print(paste0("complete_progress = ", complete_progress, "; progress = ", 1 / complete_progress)) # debug
      # *** --

    }
    results_by_group <- c(results_by_group,results_by_group_tmp)
  } # end of loop 'i in 1:nrow(combination_table)'

  ##### 3/ Sauvegarde #####
  # if (repSav == repGF) {
  #   save(results_by_group, file = "tables/gfTablesElaborees.Rdata")
  # } else  {
  #   dir.create(file.path(repSav, "tables"), showWarnings = F, recursive = T)
    save(results_by_group, file = paste0(output_dir, "/tables/gfTablesElaborees.Rdata"))
  # }

  # msg_str <- "Agr\u00E9gation des r\u00E9sultats par ensemble termin\u00E9e"
  # print(msg_str)
}
