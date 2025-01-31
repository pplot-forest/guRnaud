#' Fonction de calcul des résultats pour les arbres
#' @description La fonction calcule les résultats par arbre (Nha, Gha, Vha, ...) en fonction des paramètres 
#' d'échantillonnage, des tarifs de cubage, des seuils de qualités, etc.
#' @return La fonction renvoie la table d'entrée en ayant rajouté les variables de résultats correspondant aux calculs
#' @param df = table d'inventaire en entrée
#' @param echant_change = cet argument indique s'il y a eu un changement de protocole entre 2 inventaires
#' @param code_qual = table listant les seuils de qualité
#' @param code_essreg = table listant les regroupements d'essence
#' @param code_tarif = table listant les tarifs de cubage
#' @param code_prix = table listant les prix par essence, classe de diamètre et qualité
#' @param diam_cat = table listant les seuils de qualité
#' @import dplyr
#' @import stringr
#' @import rlang
#' @import tidyr
#' @export
calculs_Arbres <- function(
    df = NULL, echant_change = F,
    code_qual = NULL, code_essreg = NULL,
    code_tarif = NULL, code_prix = NULL,
    diam_cat = NULL
) {
  # initialize variables
  Azimut <- Caract1 <- Caract2 <- Caract3 <- Cat <- Classe <- ClasseSup <- CodeEcolo <- NULL
  CoefHoupp <- Coupe <- Cycle <- Diam <- Diam1 <- Diam2 <- DiamSup <- Dist <- EssReg <- Essence <- NULL
  Gha <- Haut <- IdArbre <- InfraDensite <- Limite <- Nha <- NumArbre <- NumForet <- NumPlac <- NULL
  NumTarif <- Observations <- PU <- PUSup <- Qual <- Ref_CodeEcolo <- Reg1 <- Reg2 <- Stade <- NULL
  Strate <- TauxCarbone <- TauxV <- Type <- TypeEss <- TypeTarif <- VcHa <- Vha <- VhaIFN <- NULL
  VhaSup <- echant_DF <- echant_ID <- prep_df <- NULL
  
  
  
  # df <- Arbres # debug
  # df <- Arbres_Acct # debug
  # echant_DF <- Echantillonnages # debug
  # -- préparation
  df <-
    df %>%
    prep_df(
      echant_DF,
      code_qual = code_qual, code_essreg = code_essreg,
      code_tarif = code_tarif, code_prix = code_prix,
      diam_cat = diam_cat
    )

  # -- calcul du poids
  df <- df %>% calculs_Nha()

  # -- calcul de la surface terrière
  df$Gha <- pi * df$Diam1 ^ 2 / 40000 * df$Nha # Utiliser Diam1 pour retrouver le coeff relascopique (2 à 2%)

  # -- calcul du volume
  # volume gestionnaire
  df <- calculs_Vol(df)
  # volume IFN
  df <- calculs_Vol(df, IFN = T)

  # -- calcul des variables économiques
  df <- calculs_Eco(df, code_prix)

  if (echant_change == F) {
    # selection de variables
    df <-
      df %>%
      select(
        NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate,
        Essence, EssReg, Azimut, Dist,

        NumTarif, TypeTarif, TypeEss, CoefHoupp, TauxCarbone, InfraDensite,

        Diam1, Diam2, Diam, Classe, Cat,
        DiamSup, ClasseSup, VhaSup, TauxV,
        Qual, Reg1, Reg2, PU, PUSup,
        Nha, Gha, Vha, VhaIFN, VcHa,
        Coupe, Limite,
        CodeEcolo, Ref_CodeEcolo,
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations
      )
  } else {
    df <-
      df %>%
      mutate(echant_ID = NA) %>%
      # selection de variables
      select(
        NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate,
        Essence, EssReg, Azimut, Dist,

        NumTarif, TypeTarif, TypeEss, CoefHoupp, TauxCarbone, InfraDensite,

        Diam1, Diam2, Diam, Classe, Cat,
        DiamSup, ClasseSup, VhaSup, TauxV,
        Qual, Reg1, Reg2, PU, PUSup,
        Nha, Gha, Vha, VhaIFN, VcHa,
        Coupe, Limite, echant_ID,
        CodeEcolo, Ref_CodeEcolo,
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations
      )
  }

  # -- retour fonction calculs_Arbres
  return(df)
}
