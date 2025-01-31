#' Fonction de calcul des accroissements
#' @description La fonction utilise une liste de noms de tables de résultats (nomenclature spécifique : "gfForetRege_Essence"). 
#' Elle décompose chaque nom de table pour retrouver les variables qui le composent. Ces éléments sont ensuite répartis dans une table
#' qui liste les combinaisons de variables possibles. Cette table de combinaisons est utilisée lors de l'étape d'agrégation des résultats.
#' Les tables de résultats obtenus en sortie de l'agrégation correspondent à la liste de noms de tables en entrée.
#' @return La fonction renvoie la table d'entrée en ayant rajouté les variables de résultats correspondant au calcul d'accroissement (en diamètre, en volume, ...) 
#' @param df = table d'inventaire en entrée
#' @param echant_change = cet argument indique s'il y a eu un changement de protocole entre 2 inventaires
#' @param cycles_table = tables listant les cycles d'inventaire
#' @param plot_table = tables listant les placettes d'inventaire
#' @param vars = variables de résultats
#' @import dplyr
#' @import stringr
#' @import rlang
#' @import tidyr
#' @importFrom stats quantile
#' @export
calculate_increments <- function(
    df = NULL,
    echant_change = F,
    cycles_table = NULL,
    plot_table = NULL,
    vars = c("Diam", "Gha", "Vha", "VcHa")
) {
  # initialize variables
  Angle <- Annee <- Azimut <- Caract1 <- Caract2 <- Caract3 <- Cat <- Classe <- ClasseSup <- Code <- NULL
  CodeEcolo <- CoefHoupp <- Coupe <- Cycle <- Diam <- Diam1 <- Diam2 <- DiamSup <- Dist <- EssReg <- NULL
  Essence <- Gha <- Haut <- IdArbre <- InfraDensite <- Infradensite <- Limite <- Lineaire <- NULL
  Nha <- NumArbre <- NumForet <- NumPlac <- NumTarif <- NumTarifBMP <- Observations <- PU <- NULL
  PUSup <- Qual <- Ref_CodeEcolo <- Reg1 <- Reg2 <- SRF <- Stade <- StadeD <- StadeE <- Stade_AFI <- NULL
  Strate <- TauxCarbone <- TauxV <- Taux_carbone <- Transect <- Type <- TypeEss <- TypeTarif <- NULL
  TypeTarifBMP <- VcHa <- Vha <- VhaIFN <- VhaSup <- calculs_Eco <- calculs_Nha <- test <- time_span <- NULL
  calculs_Vol <- echant_DF <- echant_ID <- main_species <- prep_df <- species_to_join <- NULL
  
  table <-
    df %>%
    # Arbres %>%
    # filter(NumPlac %in% c("1")) %>%  # debug 1, 11, 129
    select(
      "NumForet", "NumPlac", "NumArbre", "IdArbre", "Cycle", "Type", "Coupe",
      "Nha", # repère pour savoir si coupe ou PF
      all_of(vars)
    ) %>%
    # pivot_longer(cols = vars) %>%
    arrange(NumForet, NumPlac, NumPlac, IdArbre)

  # table contenant les temps de passage entre chaque cycle
  time_span_table <-
    cycles_table %>%
    group_by(NumForet) %>%
    mutate(time_span = Annee - lag(Annee)) %>%
    ungroup() %>%
    select(NumForet, Cycle, time_span)

  # table de suivi des placettes entre les inventaires (+ pas de temps)
  monitoring_table <-
    plot_table %>%
    # filter(NumPlac %in% c(10, 129)) %>%  # debug champlalot

    # join time_span_table
    left_join(time_span_table, by = c("NumForet", "Cycle")) %>%

    # select & arrange
    select(NumForet, NumPlac, Cycle, time_span) %>%
    arrange(NumForet, NumPlac, Cycle) %>%

    # liste des placettes pour lesquelles le calcul des accroissements est possible
    group_by(NumForet, NumPlac) %>%
    mutate(test = case_when(
      Cycle > 1 & !is.na(lag(Cycle)) ~ 1,
      Cycle == 1 ~ 1
    )) %>%
    ungroup() %>%
    filter(test == 1) %>%
    select(-test)

  # main table
  table <- left_join(
    monitoring_table, table,
    by = c("NumForet", "NumPlac", "Cycle")
  ) %>%
    arrange(NumForet, NumPlac, NumArbre, IdArbre, Cycle) %>%

    group_by(NumForet) %>%
    mutate(last_cycle = last(Cycle)) %>%

    group_by(NumForet, NumPlac, NumArbre, IdArbre) %>%
    # head() %>% # debug
    mutate(
      Coupe = as.character(Coupe),

      # arbres disparus (coupés ou chablis ou morts sur pied)
      missing = case_when(
        # arbres devenus morts sur pied et non exploités (valeur par défaut car notation non disponible) :
        is.na(Type) & !is.na(lead(Type)) & Cycle < last_cycle & is.na(Coupe) ~
          "C",
        # arbres devenus morts sur pied et exploités ou chablis (avec notation disponible) :
        is.na(Type) & !is.na(lead(Type)) & Cycle < last_cycle & !is.na(Coupe) ~
          Coupe,

        # arbres coupés :
        is.na(lead(Nha)) & !is.na(Nha) & Cycle < last_cycle & is.na(Coupe) ~
          "E",
        is.na(lead(Nha)) & !is.na(Nha) & Cycle < last_cycle & Coupe == "C" ~
          "C",
        is.na(lead(Nha)) & !is.na(Nha) & Cycle < last_cycle & Coupe != "C" ~
          "E"
      ),

      # arbres passant à la futaie (N.B : avec case_when, impossible de combiner "PF" et "E" ):
      # la colonne coupe peut déjà contenir des infos
      # les notations "C" ou "PF/C" sont à conserver.
      # les notations "E", "PF" ou "PF/E", sont de toutes façons reconstituées
      promoted = case_when(
        is.na(lag(Nha)) & !is.na(Nha) & Nha > 0 & Cycle > 1 ~
          "PF",
        # cas des arbres limites déjà présents dans l'inventaire et qui deviennent bons
        (!is.na(lag(Nha)) & lag(Nha) == 0) & # arbre précédemment limite
          !is.na(Nha) & Nha > 0 & # arbre actuellement non limite
          Cycle > 1 ~
          "PF"
      ),

      # merge missing and promoted columns
      Coupe = case_when(
        promoted == "PF" & is.na(missing) ~ "PF",
        promoted == "PF" & !is.na(missing) ~ paste0(promoted, "/", missing),
        is.na(promoted) ~ missing
      ),
      # missing = NULL,
      # promoted = NULL,

      # Coupe = ifelse(is.na(Limite), Coupe, NA)
      # --- Calcul des accroissements
      # time_span = Annee - lag(Annee),
      # default_value = ifelse(name == "Diam", NA, 0),
      # digits_value = 10, # possibilité de changer si besoin
      Acct_Diam = round( (Diam - lag(Diam, default = NA)) / time_span, digits = 10),
      AcctGper = round( (Gha - lag(Gha, default = 0)) / time_span, digits = 10),
      AcctVper = round( (Vha - lag(Vha, default = 0)) / time_span, digits = 10),
      Gainper = round( (VcHa - lag(VcHa, default = 0)) / time_span, digits = 10),

      # cleaning
      last_cycle = NULL,
      # time_span = NULL,
      Diam = NULL,
      Gha = NULL,
      Vha = NULL,
      VcHa = NULL
    ) %>%
    ungroup() %>%
    as.data.frame() #%>%
  # select(names(df1))

  df <-
    df %>%
    select(-Coupe) %>%
    left_join(
      table,
      by = c("NumForet", "NumPlac", "NumArbre", "IdArbre", "Cycle", "Type", "Nha")
    )

  # -- return of 'calculate_increments' function
  return(df)
}
