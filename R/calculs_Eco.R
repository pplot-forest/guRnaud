#' Fonction de calcul des variables éconoomiques des arbres
#' @description La fonction calcule les variables économiques de chaque arbre en fonction de la mercuriale de prix choisie
#' @return La fonction renvoie la table d'entrée avec les variables économiques des arbres (Vha)
#' @param df = table d'inventaire en entrée
#' @param code_prix = table listant les prix par essence, classe de diamètre et qualité
#' @import dplyr
#' @import stringr
#' @import rlang
#' @import tidyr
#' @export
calculs_Eco <- function(
    df = NULL, code_prix = NULL
) {
  # initialize variables
  Classe <- Diam <- PU <- Vha <- VhaSup <- NULL
  
  df <-
    df %>%
    # ---------- Valeur de consommation
    mutate(
      VcHa = Vha * PU,
      # ---------- Volume de la classe supérieure
      DiamSup = Diam + 5,
      ClasseSup = Classe + 5
    ) %>%
    calculs_Vol(Sup = T) %>%
    # ---------- Taux d'accroissement en volume
    mutate(
      TauxV = ifelse(Vha > 0, log(VhaSup / Vha) / 5, 0)
    ) %>%
    # ---------- Calcul prix de la classe supérieure (préparation valeur potentielle)
    left_join(
      code_prix, # PrixSup
      by = c("Essence", "ClasseSup" = "Classe", "Reg1" = "Qual"),
      suffix = c("", "Sup")
    )
  # retour fonction calculs_Eco
  return(df)
}
