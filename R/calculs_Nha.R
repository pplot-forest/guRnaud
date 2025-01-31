#' Fonction de calcul du poids pour les arbres
#' @description La fonction calcule le poids de chaque arbre en fonction des paramètres 
#' d'échantillonnage.
#' @return La fonction renvoie la table d'entrée avec la variable du poids des arbres (Nha)
#' @param df = table d'inventaire en entrée
#' @import dplyr
#' @import rlang
#' @import stringr
#' @import tidyr
#' @export
calculs_Nha <- function(df = NULL) {
  # initialize variables
  Nha <- NULL
  
  df <- df %>%
    mutate(
      Nha = case_when(
        # -- *** perches ***
        # - cas des perches vivantes...
        Cat == "PER" & Diam1 >= DiamLim1 & is.na(Type) &
          # ... sans distance
          is.na(Dist) ~
          # Nha =
          10000 / pi / Rayon1 ^ 2,

        # - cas des perches vivantes...
        Cat == "PER" & Diam1 >= DiamLim1 & is.na(Type) &
          # ... avec distance
          !is.na(Dist) & Dist < Rayon1 ~
          # Nha =
          10000 / pi / Rayon1 ^ 2,


        # -- *** précomtables ***
        # - cas des arbres précomtables vivants ...
        Cat != "PER" & is.na(Type) &
          # --- inventoriés par angle fixe
          Diam1 >= DiamLim & Diam1 >= Dist * Coeff ~
          # Nha =
          10 ^ 4 * Coeff ^ 2 / pi / Diam1 ^ 2,

        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 3ème cercle (2 autres sous-entendus + exclure angle fixe)
          Diam1 < DiamLim & Diam1 >= DiamLim3 & Dist <= Rayon3 * CoeffPente ~
          # Nha =
          10000 / pi / Rayon3 ^ 2,

        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 2ème cercle (1er sous-entendu + exclure angle fixe) avec ou sans 3ème cercle ...
          Diam1 < DiamLim & Diam1 >= DiamLim2 & Dist <= Rayon2 * CoeffPente ~
          # Nha =
          10000 / pi / Rayon2 ^ 2,

        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 1er cercle avec ou sans 2ème cercle (exclure angle fixe)
          Diam1 < DiamLim & Diam1 >= DiamLim1 & Dist / CoeffPente <= Rayon1 ~
          # Nha =
          10000 / pi / Rayon1 ^ 2,


        # -- *** bois mort sur pied ***
        # - cas des bois morts sur pied...
        !is.na(Type) & #BMP -> manque argument !!!!
          # >= 30 cm de diamètre
          # ... inventoriés sur le 2ème cercle (1er sous-entendu + exclure angle fixe) avec ou sans 3ème cercle ...
          Diam >= 30 & Dist <= 20 ~
          # Nha =
          10000 / pi / 20 ^ 2,

        # !is.na(Type) &
        #   # >= 30 cm de diamètre
        #   # ... inventoriés par angle fixe
        #   Diam >= 30 & Diam1 >= Dist * Coeff  ~
        #   # Nha =
        #   10 ^ 4 * Coeff ^ 2 / pi / Diam1 ^ 2,

        # - cas des bois morts sur pied...
        !is.na(Type) &
          # < 30 cm de diamètre
          Diam >= DiamLim1 & Diam < 30 & Dist <= 10 ~
          # Nha =
          10000 / pi / 10 ^ 2
      ),

      # définition attribut limite d'un arbre
      Limite = ifelse(is.na(Nha), 1, 0),
      # arbre limite => Nha = 0
      Nha = ifelse(is.na(Nha), 0, Nha)
    ) # debug %>% select(NumPlac, NumArbre, Essence, Azimut, Dist, Diam1, Diam, Type, Nha, DiamLim, Coeff, BMP, DiamLim1, Rayon1, Limite)

  # rm(pos)
  # -- retour fonction calculs_Nha
  return(df)
}
