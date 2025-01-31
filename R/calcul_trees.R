#' Fonction de calcul des résultats d'inventaire par arbre
#' @description La fonction utilise les données d'inventaire de chaque population (arbres, régénération, bois mort, ...) 
#' et calcule pour chaque individu les résultats d'inventaire ramenés à l'hectare.
#' @return La fonction renvoie un ensemble de tables de résultats enregistrés dans une archive Rdata.
#' @param wd = répertoire de travail
#' @param output_dir = répertoire de sortie
#' @param TauxR = taux d'actualisation
#' @param forest = numéro et nom de la forêt
#' @param last_cycle = numéro du dernier passage en inventaire
#' @param combination_table = table listant les combinaisons d'attributs à utiliser pour agréger les résultats
#' @param complete_progress = barre de progression (application shiny)
#' @param i18n = fonction de traduction  (application shiny)
#' @import dplyr
#' @import stringr
#' @import rlang
#' @import tidyr
#' @importFrom stats quantile
#' @export
gf_Calculs <- function(
    wd = NULL, # TODO : inutile à l'avenir. Gardé par sécurité mais à supprimer
    output_dir = NULL,
    TauxR = 0.03,
    forest = NULL,
    last_cycle = NULL,
    # arch1 = "tables/DonneesBrutes.Rdata",
    complete_progress = NULL, # shiny related
    i18n = NULL
) {
  # Initialize variables
  Abroutis <- AccD <- AcctGper <- AcctGper_changed <- AcctVper <- AcctVper_changed <- NULL
  Acct_Diam <- Algan <- Attribut <- Azimut <- BMSCercles <- Caract1 <- Caract2 <- Caract3 <- NULL
  Cat <- Cats <- Class1 <- Class2 <- Class3 <- Classe <- Classe1Ha <- Classe2Ha <- Classe3Ha <- NULL
  ClasseSup <- CodeEcolo <- CodeEcolos <- CodeStadeDecomposition <- CoefHoupp <- Coeff <- NULL
  Coeff.Freq <- Coeff.Var1 <- Coeff.Var2 <- Coeff.Var3 <- Coupe <- Coupe_changed <- NULL
  Coupe_temp <- Cycle <- Cycles <- DensiteBoisMort <- Descriptif_Attribut <- Diam <- NULL
  Diam1 <- Diam2 <- DiamSup <- Dist <- Echantillonnages <- EssInd <- EssReg <- EssValor <- NULL
  Essence <- Essences <- Forets <- Gain <- Gainper <- Gainper_changed <- Gha <- Haut <- IdArbre <- NULL
  InfraDensite <- Limite <- Liste <- Listes <- NbPlac <- Nbre <- Nha <- NumArbre <- NumForet <- NULL
  NumPlac <- NumTarif <- Observations <- PU <- PUSup <- Parcelle <- Placettes <- Population <- NULL
  Prix <- Qual <- Quals <- Recouv <- Ref_CodeEcolo <- Reg1 <- Reg2 <- Rejet <- SsPlac <- Stade <- NULL
  StadeD <- StadeE <- Station <- Strate <- Surf <- Tarifs <- Taux <- TauxCarbone <- NULL
  TauxCarboneBoisMort <- TauxPU <- TauxV <- Type <- TypeEss <- TypeTarif <- VcHa <- Vha <- NULL
  VhaIFN <- VhaSup <- Vides <- calculate_increments <- calculs_Arbres <- NULL
  calculs_BM_carbone <- calculs_Carbone <- calculs_bm_lineaire <- calculs_bmp <- NULL
  calculs_bms_sup30 <- calculs_cercles <- calculs_dmh <- calculs_rege <- time_span <- NULL
  change_protocole <- check_duplicated_trees <- echant_ID <- find_ESSREG <- NULL
  miroir_add <- quantile <- set_db_id <- set_up_calcul_tables <- share_accD <- tCha <- NULL

  
  
  ##### 1/ Initialisation #####
  # -- création du dossier de sortie
  # output_dir <- file.path(output_dir, "tables")
  # output_dir <- file.path("out", disp, i18n()$t("livret_AFI"))
  dir.create(file.path(output_dir, "tables"), showWarnings = F, recursive = T)
  
  # -- chargement des données d'inventaire et administratives
  inventory_tables <- load(file.path(wd, "tables/DonneesBrutes.Rdata"))
  # -- chargement du dictionnaire de traduction
  load(file.path(wd, "tables/dictionary.Rdata"))
  
  # incrémentation de la barre de progression
  detail = i18n()$t("Chargement des données d'inventaire")
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  print(detail)
  
  # # -- sélection des données propres au(x) dispositif(s) choisi(s) : Utilisation en cas d'analyse transversale - à tester
  # if (repSav == wd) {
  #   # -- choix du dispositif
  #   # initialisation
  #   check_all_msg <- "Editer les r\u00E9sultats pour tous les dispositifs"
  #   forest_list <- choose_forest(tables, Forets, check_all_msg)
  # } else {
  forest_list <- forest
  # }
  # last_cycle <- get_last_cycle(tables, forest_list)
  
  # -- filtre des tables d'inventaire en fonction des numéros de dispositif sélectionnés
  # inventory_tables <- c(
  #   "IdArbres", "BMortSup30","BMortLineaires", "Cycles", #"Placettes",
  #   "Reges", "Coords", "Taillis"
  # )
  
  
  # tables with "NumForet" -> à filtrer
  tables_to_filter <- c()
  for (table in inventory_tables) {
    if ("NumForet" %in% names(get( table ))) {
      tables_to_filter <- c(tables_to_filter, table)
    }
  }
  filter_by_forest(tables = tables_to_filter, forest_list = forest_list, cycle = last_cycle)
  
  
  ##### 2/ Calculs des variables de la table Arbres ----- #####
  # id variables
  id_vars <- c("NumForet", "NumPlac", "Cycle", "Strate")
  # -- rajout de la colonne TauxCarbone à la table Essences
  # load("/Users/Valentin/Travail/Outils/Inventaire_PP/tables/afi_species_table.Rdata")
  # Essences <- Essences %>% left_join(afi_species_table[, c("Essence", "TauxCarbone")], by = c("Nom" = "Essence"))
  
  # -- set up
  echant_DF <- Echantillonnages
  Arbres <-
    IdArbres %>%
    left_join(ValArbres, by = c("IdArbre" = "IdArbre")) %>%
    # filter(NumPlac == 102) %>%
    # filter(!is.na(Essence) & !is.na(Azimut) & !is.na(Dist)) %>%
    # et les éléments de bois mort ou non repérés ????
    # filtre ci-dessus important. Sinon pblme dcast accroissement
    
    # jonction avec la table des essences - récupération des informations carbone (TypeEss, CoefHoupp, TauxCarbone, InfraDensite)
    left_join(Essences, by = c("NumForet", "Essence" = "Nom")) %>%
    
    # -- jonction avec la table 'Placettes' -> récupération des variables Strate, CoeffPente, Miroir_Azimut, Miroir_Dist,
    # initialement, on récupérait : PoidsPlacette (inutile avant le job5), Pente (à préciser avec travail pour Evrard)
    left_join(
      Placettes %>% select(c("NumForet", "NumPlac", "Cycle", "Strate", "CoeffPente", "Miroir_Azimut", "Miroir_Dist")),
      by = c("NumForet", "NumPlac", "Cycle")
    ) %>%
    # sort names
    select(all_of(id_vars), everything()) %>%
    
    # -- jonction avec la table Echantillonnages
    left_join(
      echant_DF[, c(
        "NumForet", "Cycle", "Strate", "Surface", "NbPlac",
        "DiamLim1", "Rayon1", "DiamLim2", "Rayon2", "DiamLim3", "Rayon3",
        "Coeff", "DiamLim",
        
        # "BMP_DiamLim1", "BMP_Rayon1", # TODO : à revoir
        # "BMP_DiamLim2", "BMP_Rayon2",
        "BMP"
      )],
      by = c(
        "NumForet" = "NumForet",
        "Cycle" = "Cycle",
        "Strate" = "Strate"
      )
    ) %>%
    # correction
    mutate(Coeff = Coeff * 100) # correction nécessaire car 28 >= 14 * 0.02 * 100 est faux (tests B.Meheux)
  
  # -- placettes miroir
  if (length( with(Arbres, which( !is.na(Miroir_Azimut) | !is.na(Miroir_Dist) )) ) > 0) {
    # rajout des arbres miroirs pour les placettes miroirs (Azimut_Mirroir et Dist_Mirroir nécessaires)
    Arbres <- Arbres %>% miroir_add()
    
    # renuméroter arbres à cause du rajout des arbres miroirs
    # -- define id_columns
    id_columns <-
      c("NumForet", "NumPlac", "NumArbre", "Essence", "Azimut", "Dist")
    
    db_tables <-
      set_db_id(table = Arbres, id_columns = id_columns, id_var = "IdArbre")
    IdArbres <- db_tables$id_table # add table 'IdArbres'
    ValArbres <- db_tables$value_table
    
    Arbres <-
      IdArbres %>%
      left_join(ValArbres, by = c("IdArbre" = "IdArbre"))
  }
  
  ##### 2.1/ calculs des variables stationnaires #####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  code_qual = Quals
  code_essreg = EssReg
  code_tarif = Tarifs
  code_prix = Prix
  diam_cat = Cats
  
  Arbres <-
    calculs_Arbres(
      Arbres,
      code_qual = Quals, code_essreg = EssReg, # TODO : faire une liste avec toutes les codifications
      code_tarif = Tarifs, code_prix = Prix,
      diam_cat = Cats
    ) %>%
    
    # récupération des regroupements d'essences
    find_ESSREG(code_essences = Essences)
  
  
  ##### 2.2/ calcul des accroissements (variables dynamiques) #####
  # perches + autres tiges sans Az ni Dist mis à l'écart
  # (bloquant pour les calculs d'accroissement - perche promue apparaîtrait coupée)
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  # -- tri des populations
  # distinction des perches
  # on utilise Diam et pas Diam1 comme distinction : cela permet d'éviter les classes 5 ou 20 dans les figures des perches)
  stem_threshold_min <- Cats$Diam[Cats$Cat == "PER"]
  stem_threshold_max <- Cats$Diam[Cats$Cat == "PB"] # 17.5 usuellement, 12.5 pour Audenge
  Perches <-
    Arbres %>% filter(Diam >= stem_threshold_min & Diam < stem_threshold_max) # 12.5 pour Audenge
  # BMP contenus dans les perches :
  BMP <- Perches %>% filter(!is.na(Type)) %>% select(
    NumForet, NumPlac, Cycle, Dist,
    Essence, EssReg,
    Diam, Classe, Cat,
    Haut, Type, Limite,
    Stade,
    Nha, Gha, Vha
  )
  Perches <- Perches %>% filter(is.na(Type))
  
  # arbres non repérés
  Arbres_nonRep <- Arbres %>% filter((is.na(Essence) | is.na(Azimut) | is.na(Dist)) & Diam >= stem_threshold_max) # 17.5 usuellement, 12.5 pour Audenge
  
  # arbres repérés
  Arbres <-
    Arbres %>% filter(!is.na(Essence) & !is.na(Azimut) & !is.na(Dist) & Diam >= stem_threshold_max) # 17.5 usuellement, 12.5 pour Audenge
  
  
  if (max(Arbres$Cycle) > 1) {
    # -- calculs d'accroissement
    # test = acct_tables %>% filter(NumPlac == 1) %>% select(c(1,2,3,5,7,9,10,17,18,19,20,31,32,33,46:50)) # debug
    acct_tables <-
      Arbres %>%
      check_duplicated_trees() %>%
      calculate_increments(
        cycles_table = Cycles,
        plot_table = Placettes
      )
    
    # -- sauvegarde table des accroissements en diamètre
    AcctD <-
      acct_tables %>%
      select(
        IdArbre, NumForet, NumPlac, NumArbre, Cycle,
        Essence, Classe, Acct_Diam
      )
    
    Arbres <- share_accD(acct_tables)
    # tk_messageBox(type = "ok", message = "rajouter Num dans Essence + contrôle sur les prix + vérifier calculs accroissement pour changement de protocole")
    
    # -- rajout de colonnes vides dans Arbres_nonRep
    Arbres_nonRep <-
      Arbres_nonRep %>%
      # TODO faire une fonction pour aligner les tables automatiquement
      mutate(
        AcctGper = NA, AcctVper = NA, Gainper = NA, AcctD = NA, time_span = NA
      )
    # rajout de colonnes vides dans Perches
    Perches <-
      Perches %>%
      mutate(
        AcctGper = NA, AcctVper = NA, Gainper = NA, AcctD = NA, time_span = NA
      )
    
    # -- gestion des cas où changement de protocole
    # extrait de la table Echantillonnages des cycles concernés par
    # le changement de protocole (s'il existe)
    echant_DF <- Echantillonnages
    echant_DF <- change_protocole(echant_DF) # TODO : à revoir
    
    if (length(unique(echant_DF$echant_ID)) > 1) {
      # Recalculs des valeurs à l'hectare selon les paramètres stables dans le temps.
      Arbres_Acct <-
        left_join(
          IdArbres, ValArbres, by = "IdArbre"
        ) %>%
        filter(
          !is.na(Essence) & !is.na(Azimut) & !is.na(Dist)
        ) %>% # on ne prend que les arbres repérés
        
        # jonction avec la table des essences - récupération des informations carbone (TypeEss, CoefHoupp, TauxCarbone, InfraDensite)
        left_join(Essences, by = c("NumForet", "Essence" = "Nom")) %>%
        
        left_join(
          Placettes[, c(
            "NumForet", "NumPlac", "Cycle", "Strate", "PoidsPlacette",
            "Pente", "CoeffPente", "Parcelle", "Station"
          )],
          by = c("NumForet", "NumPlac", "Cycle")
        ) %>%
        mutate(
          Qual = as.character(Qual),
          Coupe = as.character(Coupe),
          echant_ID = paste0(NumForet, "-", Cycle, "-", Strate)
        ) %>%
        # on ne sélectionne que les arbres concernés par le changement de protocole
        right_join(
          echant_DF[, c(
            "NumForet", "Cycle", "Strate",
            "Surface", "NbPlac",
            "DiamLim1", "Rayon1",
            "DiamLim2", "Rayon2",
            "DiamLim3", "Rayon3",
            "Coeff", "DiamLim",
            "echant_ID"
          )],
          by = c("NumForet", "Cycle", "Strate", "echant_ID" = "echant_ID")
        ) %>%
        mutate(Coeff = Coeff * 100)
      
      # Calculs d'accroissement avec le nouveau protocole
      Arbres_Acct <- calculs_Arbres(
        Arbres_Acct,
        echant_change = T,
        code_qual = Quals, code_essreg = EssReg,
        code_tarif = Tarifs, code_prix = Prix,
        diam_cat = Cats
      ) %>%
        
        # -- tri : ne tenir compte que des arbres repérés Azimut/Distance
        # arbres repérés
        filter(!is.na(Essence) & !is.na(Azimut) & !is.na(Dist) & Diam >= stem_threshold_max) # 17.5 usuellement, 12.5 pour Audenge
      
      # repérage des cycles concernés par le changement de protocole
      cycle_INI <- min(echant_DF$Cycle)
      cycle_FIN <- max(echant_DF$Cycle)
      # recalcul des accroissements
      Arbres_Acct <-
        # calculs_Acct(
        Arbres_Acct %>%  #,
        #   cycles_table = Cycles,
        #   echant_change = T
        # ) %>%
        calculate_increments(
          cycles_table = Cycles,
          plot_table = Placettes
        )
      
      # fusion de l'ancienne tables arbres avec la table Arbres_Acct
      Arbres <-
        full_join(
          Arbres,
          Arbres_Acct,
          by = c(
            "NumForet", "NumPlac", "NumArbre", "Strate", "IdArbre", "Cycle"
          ),
          suffix = c("", "_changed")
        ) %>%
        select(
          NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate,
          Essence, EssReg, Azimut, Dist,
          Diam1, Diam2, Diam, Classe, Cat,
          DiamSup, ClasseSup, VhaSup, TauxV,
          Qual, Reg1, Reg2, PU, PUSup,
          Nha, Gha, Vha, VhaIFN, VcHa,
          Coupe, Limite,
          CodeEcolo, Ref_CodeEcolo,
          Type, Haut, Stade, Caract1, Caract2, Caract3, Observations,
          
          NumTarif, TypeTarif, TypeEss, CoefHoupp, TauxCarbone, InfraDensite,
          
          AcctGper, AcctGper_changed,
          AcctVper, AcctVper_changed,
          Gainper, Gainper_changed,
          AcctD, #AcctD_changed, # on garde quoi qu'il arrive AcctD initial car contient plus d'infos
          Coupe, Coupe_changed,
          
          time_span, echant_ID
        ) %>%
        # si le cycle est concerné par le changement de protocole alors on prend
        # les valeurs d'accroissement 'changed'
        mutate(
          AcctGper =
            ifelse(
              Cycle %in% c(cycle_INI, cycle_FIN), AcctGper_changed, AcctGper
            ),
          AcctVper =
            ifelse(
              Cycle %in% c(cycle_INI, cycle_FIN), AcctVper_changed, AcctVper
            ),
          Gainper =
            ifelse(
              Cycle %in% c(cycle_INI, cycle_FIN), Gainper_changed, Gainper
            )
        ) %>%
        group_by(NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate) %>%
        # cas de la colonne Coupe :
        # comme seulement 2 cycles sont concernés dans un changement de protocole.
        # CAS 1 : cycle_INI = 1
        # --- 1/ Cycle == cyle_INI( = 1) il ne peut pas y avoir de notation PF,
        # PF/E ou PF/C (il faudrait pour cela que cycle_INI soit > 1)
        # = > Donc pour ne pas manquer les bons Coupe_changed = "E" ou "C", je dois
        # récupérer la notation Coupe_changed
        mutate(
          Coupe_temp =
            ifelse(
              Cycle == cycle_INI, Coupe_changed, Coupe # on récupère les notations "E" ou "C"
            ),
          Coupe = Coupe_temp
        ) %>%
        ungroup() %>%
        mutate(
          AcctGper_changed = NULL,
          AcctVper_changed = NULL,
          Gainper_changed = NULL,
          Coupe_changed = NULL,
          Coupe_temp = NULL
        ) %>%
        as.data.frame()
      #        # --- 2/ Cycle = cycle_FIN( = 2) il n'y a pas de "E" ou "C" dans Arbres_Acct.
      #        # Mais l'arbre peut avoir été coupé au tour suivant.
      #        # je dois donc récupérer la notation "E" ou "C" de la colonne Coupe et,
      #        # si nécessaire, l'associer à la notation "PF" éventuellement contenue
      #        # dans Arbres_Acct
      
      Arbres <- Arbres
      # rajout de colonnes vides dans Arbres_nonRep
      Arbres_nonRep <-
        Arbres_nonRep %>%
        # faire une fonction pour aligner les tables automatiquement
        mutate(echant_ID = NA)
      Perches <-
        Perches %>%
        mutate(echant_ID = NA)
      
    }
  } else {
    Arbres <-
      left_join(
        Arbres, AcctD,
        by = c("NumForet", "Strate", "Essence", "Classe", "Cycle") #, "Cycle"
      ) %>%
      mutate(
        AcctD = AccD,
        echant_ID = NA,
        # rajout des colonnes d'accroissement vides
        AcctGper = NA,
        AcctVper = NA,
        Gainper = NA
      )
    
    # rajout de colonnes vides dans Arbres_nonRep
    Arbres_nonRep <-
      Arbres_nonRep %>%
      # TODO faire une fonction pour aligner les tables automatiquement
      mutate(
        AccD = NA,
        AcctD = NA,
        echant_ID = NA,
        # rajout des colonnes d'accroissement vides
        AcctGper = NA,
        AcctVper = NA,
        Gainper = NA
      )
    # rajout de colonnes vides dans Perches
    Perches <-
      Perches %>%
      # faire une fonction pour aligner les tables automatiquement
      mutate(
        AccD = NA,
        AcctD = NA,
        echant_ID = NA,
        # rajout des colonnes d'accroissement vides
        AcctGper = NA,
        AcctVper = NA,
        Gainper = NA
      )
  } # end of cond if (max(Arbres$Cycle) > 1)
  # A voir si on garde cette partie ci-dessous ?
  # if (dim(AcctD)[1] > 0)  {
  #   pos <- which(is.na(Arbres$AcctD))
  #   if (length(pos) > 0) {
  #     Arbres$AcctD[pos] <- 0.3
  #   }
  # }
  ##### / \ #####
  
  ##### 2.3/ Fin calculs onglet arbres #####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  Arbres <-
    Arbres %>%
    rbind(Arbres_nonRep) %>%
    # rbind(Perches) %>%
    mutate(
      TauxPU = log(PUSup / PU) / 5,
      Taux = (TauxPU + TauxV) * AcctD,
      AcctV = TauxV * Vha * AcctD,
      Gain = Taux * VcHa,
      VpHa = Gain / TauxR,
      AcctG = pi / 20000 * AcctD * Diam * Nha
    ) %>%
    arrange(NumForet, NumPlac, NumArbre, Cycle) #%>%
  # Rajout de la moitié des accroissements pour les arbres coupés
  # group_by(NumForet, NumPlac, NumArbre) %>%
  # mutate(
  #   AcctD = ifelse(
  #     !is.na(lag(Coupe)) &
  #       (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")),
  #     AcctD/2, AcctD
  #     ),
  #   AcctG =  ifelse(
  #     is.na(AcctG) &
  #       (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")),
  #     lag(AcctG)/2, AcctG
  #   ),
  #   AcctGper =  ifelse(
  #     is.na(AcctGper) &
  #       (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")),
  #     lag(AcctG)/2, AcctGper
  #   ),
  #   AcctV =  ifelse(
  #     is.na(AcctV) &
  #       (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")),
  #     lag(AcctV)/2, AcctV
  #   ),
  #   AcctVper =  ifelse(
  #     is.na(AcctVper) & (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")),
  #     lag(AcctV)/2, AcctVper
  #   ),
  #   Gain =  ifelse(
  #     is.na(Gain) & (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")),
  #     lag(Gain)/2, Gain
  #   ),
  #   Gainper =  ifelse(
  #     is.na(Gainper) & (str_detect(lag(Coupe), "E") | str_detect(lag(Coupe), "C")),
  #     lag(Gain)/2, Gainper
  #   )
  # ) %>%
  # ungroup()
  
  # -- séparation des populations arbres vivants et arbres morts sur pied/taillis
  tAutres <- filter(Arbres, !is.na(Type))
  Arbres <- filter(Arbres, is.na(Type))
  
  # -- cleaning Arbres table
  # define attributes (caract1/2/3) from 'Listes' table
  attributes_to_add <-
    Listes %>%
    filter(Liste == "ListeCaract") %>%
    distinct() %>%  # sécurité - à voir si utile
    filter(!is.na(Attribut) & !is.na(Descriptif_Attribut)) %>%
    select(Attribut) %>%
    unlist() %>%
    unname()
  
  # TODO : faire une liste avec tous les id_vars selon les tables,
  # toutes les variables d'attribut selon les tables
  # variables d'identifiant
  id_vars <- c(
    "NumForet", "NumPlac", "NumArbre", "Cycle", "Coupe",
    "IdArbre", "Azimut", "Dist", "Observations"
  )
  # variables d'attribut
  attribute_vars <- c(
    "Essence", "EssReg", "Qual", attributes_to_add,
    "Type", "Reg1", "Reg2", "Haut", "Stade", "Limite",
    "Diam1", "Diam2", "Diam", "Classe", "Cat",
    "CodeEcolo", "Ref_CodeEcolo", "PU"
  )
  # variables des attributs carbone (nécessaires au calcul carbone)
  carbon_attributes_vars <- c(
    "TypeTarif", "NumTarif", "TypeEss", "CoefHoupp", "TauxCarbone", "InfraDensite"
  )
  # variables de résultat
  results_vars <- c(
    "Nha", "Gha", "Vha", "VhaIFN", "VcHa", "VpHa",
    "Gain", "Gainper", "AcctV", "AcctVper", "AcctG", "AcctGper",
    "Taux", "TauxPU", "TauxV", "AcctD"
  )
  # if (max(Arbres$Cycle) == 1) { # use last_cycle - tester plusieurs forêts en même temps
  #   results_vars <-
  #     setdiff(results_vars, c("Gainper", "AcctVper", "AcctGper"))
  # } -> NON : mettre les colonnes des variables dynamiques, même si vides.
  
  # selection vars
  vars_to_select <- c(
    id_vars, carbon_attributes_vars, attribute_vars, results_vars
  )
  # select
  Arbres <- Arbres %>% select( any_of(vars_to_select) )
  
  if (nrow(tAutres) > 0) {
    posBMP <- which(is.element(tAutres$Type, c("A", "C", "S")))
    posTaillis <- which(is.element(tAutres$Type, "Taillis"))
    
    if (length(posBMP) > 0) {
      BMP <- rbind(
        BMP,
        tAutres[posBMP, ] %>% select( all_of(names(BMP)) )
      ) %>%
        mutate(
          StadeE = floor(Stade / 10),
          StadeD = Stade - StadeE * 10,
          EssReg = as.character(EssReg),
          Vha = NA
        ) %>%
        select(
          NumForet, NumPlac, Cycle, Dist,
          Essence, EssReg,
          Diam, Classe, Cat,
          Haut, Type, Limite,
          Stade, StadeD, StadeE,
          Nha, Gha, Vha
        ) %>%
        find_ESSREG(code_essences = Essences)
    } else {
      BMP <-
        BMP %>%
        select(
          NumForet, NumPlac, Cycle, Dist,
          Essence, EssReg,
          Diam, Classe, Cat,
          Haut, Type, Limite,
          Stade,
          Nha, Gha, Vha
        )
    }
    if (length(posTaillis) > 0) {
      Taillis <-
        tAutres[posTaillis, ] %>%
        select(
          NumForet, NumPlac, Cycle,
          # Quart,
          Essence, EssReg,
          Diam, Classe, Cat,
          Nha, Gha, Vha
        ) %>%
        find_ESSREG(code_essences = Essences)
    } else {
      Taillis <-
        data.frame(
          NumForet = numeric(),
          NumPlac = character(),
          Cycle = numeric(),
          # Quart = integer(),
          Essence = character(),
          EssReg = character(),
          Diam = numeric(),
          Classe = numeric(),
          Cat = character(),
          Nha = numeric(),
          Gha = numeric(),
          Vha = numeric(),
          stringsAsFactors = F
        )
    }
  } else {
    BMP <-
      BMP %>%
      select(
        NumForet, NumPlac, Cycle, Dist,
        Essence, EssReg,
        Diam, Classe, Cat,
        Haut, Type, Limite,
        Stade,
        Nha, Gha, Vha
      )
    Taillis <-
      data.frame(
        NumForet = numeric(),
        NumPlac = character(),
        Cycle = numeric(),
        # Quart = integer(),
        Essence = character(),
        EssReg = character(),
        Diam = numeric(),
        Classe = numeric(),
        Cat = character(),
        Nha = numeric(),
        Gha = numeric(),
        Vha = numeric(),
        stringsAsFactors = F
      )
  }
  
  
  
  # -- calcul du stock de carbone dans les arbres vivants
  # -- paramètres de calcul
  DecroissanceRx <- 1
  # RendementABC <- 0.6
  # RendementD <- 0.99
  DureeVieD <- 3
  DureeVieConnexes <- 1
  
  # -- calcul
  # table des rendements
  yield_table <- tibble(
    qual = c("A", "B", "C", "D"),
    yield = c(0.6, 0.6, 0.6, 0.99)
  )
  # table de simulation des hauteurs de grume (à partir des volumes grume + commerciaux + tiges)
  # height_by_class_and_NumTarif <-
  #   Arbres %>%
  #   select(Essence, Classe, Vha, Nha) %>%
  #   mutate(Haut = Vha / Nha *)
  # -> pas possible si pas de DiamMed
  
  # -- chargement table algan
  load(paste0(wd, "/tables/algan_table.Rdata"))
  
  # table principale
  living_trees_carbon_splitted_by_log <-
    Arbres %>%
    # exclusion des arbres limites (calculs inutiles)
    filter(is.na(Limite)) %>%
    # calcul des tC/ha et durées de vie des billons des arbres sur pied
    calculs_Carbone(
      height_by_class_and_NumTarif = Algan,
      DecroissanceRx = DecroissanceRx,
      yield_table = yield_table,
      DureeVieD = DureeVieD,
      DureeVieConnexes = DureeVieConnexes
    )
  
  # carbone total par arbre
  living_trees_carbon <-
    living_trees_carbon_splitted_by_log %>%
    group_by(IdArbre) %>%
    summarise(tCha = sum(tCha, na.rm =T)) %>%
    ungroup()
  
  Arbres <-
    Arbres %>%
    # jonction de la table living_trees_carbon
    left_join(
      living_trees_carbon,
      by = "IdArbre"
    ) %>%
    select(-all_of(carbon_attributes_vars))
  #####/\#####
  
  #### 3/ Régénération ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  if (nrow(Reges) > 0) {
    # -- calcul densités à l'hectare (poids)
    Reges <-
      Reges %>%
      set_up_calcul_tables(
        plot_df = Placettes,
        settings_df = Echantillonnages
      ) %>%
      mutate(
        Recouv = as.numeric(Recouv),
        Class1 = as.numeric(Class1),
        Class2 = as.numeric(Class2),
        Class3 = as.numeric(Class3),
        
        #  initiate EssValor column # TODO : to suppress ?
        EssValor = NA
      ) %>%
      calculs_rege(EssReg)
    
    # -- prise en compte EssInd
    for (i in 1:nrow(Forets)) {
      pos_EssInd <- which(EssInd$NumForet == Forets$NumForet[i])
      if (length(pos_EssInd) > 0) {
        EssEnTour <-
          EssInd$Essence[ EssInd$NumForet == Forets$NumForet[i] ]
        pos <-
          which(
            Reges$NumForet == Forets$NumForet[i] & Reges$Essence %in% EssEnTour
          )
        Reges$EssValor[pos] <- 1
      } else {
        Reges$EssValor <- 1
      }
    }
    Reges <-
      Reges %>%
      mutate(
        Surf = ifelse(Class1 + Class2 + Class3 >= 5, 1, 0),
        Surf = Surf / NbPlac
      )
    SurfRege <-
      Reges %>%
      group_by(
        NumForet, NumPlac, Cycle, SsPlac, Essence
      ) %>%
      mutate(
        Surf = sum(Surf, na.rm = T),
        SurfEssValor = sum(Surf * EssValor, na.rm = T)
      ) %>%
      ungroup()
    Reges <-
      Reges %>%
      select(
        NumForet, NumPlac, Cycle, NumPlac, SsPlac,
        Parcelle, Station, #Groupe, Typologie, Groupe1
        Essence, EssReg, EssValor,
        Recouv, Class1, Class2, Class3,
        Rejet, Abroutis,
        Classe1Ha, Classe2Ha, Classe3Ha
      )
  } else {
    Reges <-
      data.frame(
        NumForet = integer(),
        Strate = integer(),
        Cycle = integer(),
        NumPlac = character(),
        SsPlac = character(),
        Parcelle = character(),
        Groupe = character(),
        Typologie = character(),
        Groupe1 = character(),
        Station = character(),
        Essence = character(),
        EssReg = character(),
        Essvalor = character(),
        Recouv = numeric(),
        Class1 = numeric(),
        Class2 = numeric(),
        Class3 = numeric(),
        Rejet = numeric(),
        Abroutis = numeric(),
        Classe1Ha = numeric(),
        Classe2Ha = numeric(),
        Classe3Ha = numeric(),
        stringsAsFactors = F
      )
  }
  
  #### 4/ Traitement PCQM ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  if (nrow(PCQM) > 0) {
    # -- paramètres et correction
    PCQM <-
      PCQM %>%
      set_up_calcul_tables(
        plot_df = Placettes,
        settings_df = Echantillonnages
      )
    
    
    # -- calculs
    # population de Taillis
    if (length(which(PCQM$Population == "Taillis")) > 0) {
      # table PCQM
      Taillis_PCQM <- PCQM %>% filter(Population == "Taillis") %>% miroir_add(PCQM = T)
      
      # table correction des quarts vides
      Corr <-
        data.frame(
          Coeff = table(Taillis_PCQM$NumForet, Taillis_PCQM$Cycle, Taillis_PCQM$NumPlac)
        ) %>%
        dplyr::rename(
          NumForet = Coeff.Var1,
          Cycle = Coeff.Var2,
          NumPlac = Coeff.Var3,
          Nbre = Coeff.Freq
        ) %>%
        mutate(
          NumForet = as.numeric(as.character(NumForet)),
          NumPlac = as.character(NumPlac),
          Cycle = as.numeric(as.character(Cycle)),
          Nbre = as.numeric(Nbre),
          Vides = 4 - Nbre,
          Surf = Vides * quantile(Taillis_PCQM$Dist, probs = 0.95) ^ 2
        )
      
      # table PCQM suite
      Taillis_PCQM <-
        Taillis_PCQM %>%
        mutate(Taillis = ifelse(is.na(Taillis), 25, Taillis)) %>% # valeur par défaut
        filter(Dist <= Taillis) %>%
        left_join(
          Corr[, c("NumForet", "NumPlac", "Cycle", "Surf")],
          by = c("NumForet", "NumPlac", "Cycle")
        ) %>%
        group_by(NumForet, NumPlac, Cycle) %>%
        mutate(
          Nha = sum(Dist ^ 2, na.rm = T),
          Nha = Nha + Surf,
          Nha = 10000 * 3 / pi / Nha
        ) %>%
        ungroup() %>%
        mutate(
          # Nha = Nha * Coeffts,
          Gha = pi / 40000 * Diam ^ 2 * Nha,
          Classe = floor(Diam / 5 + 0.5) * 5,
          Cat = cut(
            Diam,
            breaks = c(Cats$Diam, Inf),
            labels = Cats$Cat,
            include.lowest = T, right = F
          ),
          Cat = as.character(Cat),
          Vha = Gha * 7
        ) %>%
        left_join(EssReg, by = c("NumForet", "Essence")) %>%
        select(
          NumForet, NumPlac, Cycle, #Quart,
          Essence, EssReg,
          Diam, Classe, Cat,
          Nha, Gha, Vha
        ) %>%
        find_ESSREG(code_essences = Essences)
    }
    
    # population de BMP
    if (length(which(PCQM$Population == "BMP")) > 0) {
      # table PCQM
      BMP_PCQM <- PCQM %>% filter(Population == "BMP") %>% miroir_add(PCQM = T)
      
      # table correction des quarts vides
      Corr <-
        data.frame(
          Coeff = table(BMP_PCQM$NumForet, BMP_PCQM$Cycle, BMP_PCQM$NumPlac)
        ) %>%
        dplyr::rename(
          NumForet = Coeff.Var1,
          Cycle = Coeff.Var2,
          NumPlac = Coeff.Var3,
          Nbre = Coeff.Freq
        ) %>%
        mutate(
          NumForet = as.numeric(as.character(NumForet)),
          NumPlac = as.character(NumPlac),
          Cycle = as.numeric(as.character(Cycle)),
          Nbre = as.numeric(Nbre),
          Vides = 4 - Nbre,
          Surf = Vides * quantile(BMP_PCQM$Dist, probs = 0.95) ^ 2
        )
      
      # table PCQM suite
      BMP_PCQM <-
        BMP_PCQM %>% # bug si texte dans le champs
        mutate(BMP = ifelse(is.na(BMP), 25, BMP)) %>% # valeurs par d\u00E9faut
        filter(Dist <= BMP) %>%
        left_join(
          Corr[, c("NumForet", "NumPlac", "Cycle", "Surf")],
          by = c("NumForet", "NumPlac", "Cycle")
        ) %>%
        group_by(NumForet, NumPlac, Cycle) %>%
        mutate(
          Nha = sum(Dist ^ 2, na.rm = T),
          Nha = Nha + Surf,
          Nha = 10000*3 / pi / Nha
        ) %>%
        ungroup() %>%
        mutate(
          # Nha = Nha * Coeffts,
          Gha = pi / 40000 * Diam ^ 2 * Nha,
          Classe = floor(Diam / 5 + 0.5) * 5,
          Cat = cut(
            Diam,
            breaks = c(Cats$Diam, Inf),
            labels = Cats$Cat,
            include.lowest = T, right = F
          ),
          Cat = as.character(Cat),
          StadeE = floor(Stade / 10),
          StadeD = Stade - StadeE * 10,
          Vha = NA
        ) %>%
        left_join(EssReg, by = c("NumForet", "Essence")) %>%
        mutate(EssReg = as.character(EssReg), Limite = NA) %>%
        find_ESSREG(code_essences = Essences)
      
      BMP_PCQM <-
        BMP_PCQM %>%
        select(
          NumForet, NumPlac, Cycle, Dist,
          Essence, EssReg,
          Diam, Classe, Cat,
          Haut, Type, Limite,
          Stade,
          Nha, Gha, Vha
        )
    }
  }
  
  
  #### 5/ Traitement Cercles ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  # -- set up
  Cercles <-
    Cercles %>%
    set_up_calcul_tables(
      plot_df = Placettes,
      settings_df = Echantillonnages
    )
  
  if (nrow(Cercles) > 0) {
    # -- calcul
    # population de Taillis
    Taillis_Cercles <- calculs_cercles(Cercles, "Taillis", 10, code_essreg = EssReg, diam_cat = Cats)
    # population de BMP
    BMP_Cercles <-
      calculs_cercles(
        df = Cercles,
        population = "BMP",
        dist_max = 20, # valeur par défaut si non précisée dans la table Echantillonnages
        code_essreg = EssReg,
        diam_cat = Cats,
        add_bmp_vars = T
      ) %>%
      mutate(Dist = NA, Limite = NA)
  }
  
  ##### 6/ Rassemblement des population Taillis/BMP (inventaires PCQM et/ou Cercles)
  # -- Taillis
  if (!is.element("Taillis_PCQM", ls())) {
    Taillis_PCQM <-
      data.frame(
        NumForet = numeric(),
        NumPlac = character(),
        Cycle = numeric(),
        # Quart = integer(),
        Essence = character(),
        EssReg = character(),
        Diam = numeric(),
        Classe = numeric(),
        Cat = character(),
        Nha = numeric(),
        Gha = numeric(),
        Vha = numeric(),
        stringsAsFactors = F
      )
  }
  if (!is.element("Taillis_Cercles", ls())) {
    Taillis_Cercles <-
      data.frame(
        NumForet = numeric(),
        NumPlac = character(),
        Cycle = numeric(),
        # Quart = integer(),
        Essence = character(),
        EssReg = character(),
        Diam = numeric(),
        Classe = numeric(),
        Cat = character(),
        Nha = numeric(),
        Gha = numeric(),
        Vha = numeric(),
        stringsAsFactors = F
      )
  }
  # -- BMP
  if (!is.element("BMP_PCQM", ls())) {
    BMP_PCQM <-
      data.frame(
        NumForet = numeric(),
        NumPlac = character(),
        Cycle = numeric(),
        Dist = numeric(),
        Essence = character(),
        EssReg = character(),
        Diam = numeric(),
        Classe = numeric(),
        Cat = character(),
        Haut = numeric(),
        Type = character(),
        Limite = numeric(),
        Stade = numeric(),
        StadeD = numeric(),
        StadeE = numeric(),
        Nha = numeric(),
        Gha = numeric(),
        Vha = numeric(),
        stringsAsFactors = F
      )
  }
  if (!is.element("BMP_Cercles", ls())) {
    BMP_Cercles <-
      data.frame(
        NumForet = numeric(),
        NumPlac = character(),
        Cycle = numeric(),
        Dist = numeric(),
        Essence = character(),
        EssReg = character(),
        Diam = numeric(),
        Classe = numeric(),
        Cat = character(),
        Haut = numeric(),
        Type = character(),
        Limite = numeric(),
        Stade = numeric(),
        StadeD = numeric(),
        StadeE = numeric(),
        Nha = numeric(),
        Gha = numeric(),
        Vha = numeric(),
        stringsAsFactors = F
      )
  }
  
  # -- Superposition des tables :
  Taillis <- rbind(Taillis, Taillis_PCQM, Taillis_Cercles)
  BMP <- rbind(BMP, BMP_PCQM, BMP_Cercles)
  # -- Calcul du volume des BMP :
  # load carbon shares deadwood
  load(paste0(wd, "/tables/carbon_codes_tables.Rdata"))
  
  BMP <-
    BMP %>%
    set_up_calcul_tables(
      plot_df = Placettes,
      settings_df = Echantillonnages
    ) %>%
    calculs_bmp() %>%
    calculs_BM_carbone(
      species_table = Essences,
      living_trees_table = Arbres,
      type = "Bois mort sur pied",
      dead_wood_carbon_content = TauxCarboneBoisMort,
      dead_wood_density = DensiteBoisMort,
      decomposition_stage_code = CodeStadeDecomposition
    )
  
  
  ##### 6/ Bois mort au sol ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  #N.B : pour miroir BMSLineaires, consigne = déplacer Azimut des transects pour les faire malgré tout
  # ----- 6.1/ Linéaires : BMSLineaire
  BMSLineaires <-
    BMSLineaires %>%
    set_up_calcul_tables(
      plot_df = Placettes,
      settings_df = Echantillonnages
    ) %>%
    filter(!is.na(Essence)) %>% # TODO : à homogénéiser/vérifier
    calculs_bm_lineaire(code_essreg = EssReg) %>%
    calculs_BM_carbone(
      living_trees_table = Arbres,
      species_table = Essences,
      type = "Bois mort au sol",
      dead_wood_carbon_content = TauxCarboneBoisMort,
      dead_wood_density = DensiteBoisMort,
      decomposition_stage_code = CodeStadeDecomposition
    )
  
  # ----- 6.2/ Circulaire : BMSCercle
  BMSsup30 <- # TODO : changer nom ?
    BMSCercles %>%
    set_up_calcul_tables(
      plot_df = Placettes,
      settings_df = Echantillonnages
    ) %>%
    calculs_bms_sup30(code_essreg = EssReg) %>%
    calculs_BM_carbone(
      living_trees_table = Arbres,
      species_table = Essences,
      type = "Bois mort au sol",
      dead_wood_carbon_content = TauxCarboneBoisMort,
      dead_wood_density = DensiteBoisMort,
      decomposition_stage_code = CodeStadeDecomposition
    )
  
  #### 7/ Dendromicohabitats ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  Codes <-
    Arbres %>%
    filter(
      CodeEcolo != "" &
        !is.na(NumForet) & !is.na(NumPlac) &
        !is.na(NumArbre) & !is.na(CodeEcolo)
    ) %>%
    mutate(
      Ref_CodeEcolo = tolower(Ref_CodeEcolo),
      CodeEcolo = tolower(CodeEcolo) # TODO : mettre tolower dans job2
    ) %>%
    calculs_dmh(dmh_df = CodeEcolos)
  ##### / \ #####
  
  #### 9/ Sauvegarde ####
  
  # -- switch shiny ***
  incProgress(amount = 1 / complete_progress) # detail = i18n()$t(detail)
  # *** --
  
  # -- setting df classes
  # (évite pblme fonction Agreg01 dans agrégation par placettes)
  Arbres <- data.frame(Arbres)
  Perches <- data.frame(Perches)
  Reges <- data.frame(Reges)
  Taillis <- data.frame(Taillis)
  Reperes <- data.frame(Reperes)
  BMSLineaires <- data.frame(BMSLineaires)
  BMSsup30 <- data.frame(BMSsup30)
  BMP <- data.frame(BMP)
  Codes <- data.frame(Codes)
  
  # # -- save directory
  # output_dir <- "tables"
  # dir.create(output_dir, showWarnings = F, recursive = T)
  #
  # -- saving
  # if (output_dir == wd) {
  #   save(
  #     TauxR, Arbres, Perches, Reges, Taillis, Reperes, BMSLineaires,
  #     BMSsup30, BMP, Codes, AcctD, living_trees_carbon_splitted_by_log,
  #     file = "tables/gfTablesBrutes.Rdata"
  #   )
  # } else {
  #   dir.create(
  #     paste0(output_dir, "/tables"),
  #     showWarnings = F, recursive = T
  #   )
  #
  save(
    TauxR, Arbres, Perches, Reges, Taillis, Reperes, BMSLineaires,
    BMSsup30, BMP, Codes, AcctD, living_trees_carbon_splitted_by_log,
    file = file.path(output_dir, "tables/gfTablesBrutes.Rdata")
  )
  # }
}




# #### fonction de calcul du carbone dans le bois mort sur pied ####
# calculs_BMP_carbone <- function(
#   df = NULL,
#   species_table = NULL,
#   dead_wood_carbon_content = NULL,
#   dead_wood_density = NULL,
#   decomposition_stage_code = NULL
# ) {
#   # certaines essences non comptées car pas de données
#   standing_dead_wood_carbon_content <-
#     dead_wood_carbon_content %>%
#     filter(Type == "Bois mort sur pied") %>%
#     dplyr::select(-Type)
#   standing_dead_wood_density <-
#     dead_wood_density %>%
#     filter(Type == "Bois mort sur pied") %>%
#     dplyr::select(-Type)
#
#   df <-
#     df %>%
#     mutate(
#       Stade_AFI = paste(StadeE, StadeD, sep = "."),
#       time_span = paste(".", StadeD, sep = "")
#     ) %>%
#     left_join(
#       decomposition_stage_code,
#       by = c("time_span" = "Stade_AFI")
#     ) %>%
#     mutate(
#       Code = ifelse(Stade_AFI == "1.1", 1, Code),
#       Code = ifelse(Stade_AFI == "2.1", 1, Code),
#       Code = ifelse(Stade_AFI == "4.4", 5, Code),
#       Code = ifelse(Stade_AFI == "4.5", 5, Code)
#     ) %>%
#     # taux carbone Fs/Rx
#     left_join(
#       species_table[,c("Essence","TypeEss")],
#       by = "Essence") %>%
#     # taux de carbone dans feuillus/resineux en fonction stade
#     left_join(
#       standing_dead_wood_carbon_content,
#       by = c("TypeEss", "Code")
#     ) %>%
#     left_join(
#       standing_dead_wood_density,
#       by = c("Essence", "Code")
#     ) %>%
#     mutate(
#       Carbone = Vha * Infradensite * (Taux_carbone) / 100
#     ) %>%
#     select(
#       -Stade_AFI, -time_span, -Code, -TypeEss,
#       -Taux_carbone, -SRF, -Infradensite
#     )
#
#   # -- return of calculs_BMP_carbone function
#   return(df)
# }

