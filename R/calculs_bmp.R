#' Fonction de calcul des volumes de bois mort sur pied
#' @description La fonction permet de calculer les variables de résultats du bois mort sur pied.
#' @return La fonction retourne la table d'entrée avec les variables de résultats.
#' @param df = table d'inventaire en entrée
#' @import dplyr
#' @import rlang
#' @importFrom stats quantile
#' @import stringr
#' @import tidyr
#' @export
calculs_bmp <- function(df = NULL) {
  # initialize variables
  Cat <- Classe <- Cycle <- Diam <- Dist <- EssReg <- Essence <- Gha <- Haut <- Limite <- Nha <- NULL
  NumForet <- NumPlac <- NumTarifBMP <- Stade <- StadeD <- StadeE <- Type <- TypeTarifBMP <- Vha <- NULL
  
  df <-
    if (nrow(df) > 0) {
      df %>%
        mutate(
          TypeTarifBMP = ifelse(
            is.na(Type) | Type == "C"| Type == "S",
            NA,
            TypeTarifBMP
          ),
          NumTarifBMP = ifelse(
            is.na(Type) | Type == "C"| Type == "S",
            NA,
            NumTarifBMP
          )
        ) %>%
        rename(
          TypeTarif = TypeTarifBMP,
          NumTarif = NumTarifBMP
        ) %>%
        calculs_Vol() %>%
        mutate(
          Vha = ifelse(
            !is.na(Vha), Vha,
            # Type == "A" & !is.na(Type), Vha,
            ifelse(
              is.na(Haut),
              8 * Gha,
              pi / 40000 * (Diam - (Haut / 2 - 1.30)) ^ 2 *
                Haut * Nha
            )
          ),
          StadeE = floor(Stade / 10),
          StadeD = Stade - StadeE * 10
        ) %>%
        select(
          NumForet, NumPlac, Cycle, Dist, Essence, EssReg,
          Diam, Classe, Cat, Type, Limite, Stade, StadeD, StadeE, Nha, Gha, Vha
        )
    } else {
      data.frame(
        NumForet = numeric(), NumPlac = character(),
        Cycle = numeric(), Dist = numeric(), Transect = character(),
        Essence = character(), EssReg = character(),
        Diam = numeric(), Classe = numeric(), Cat = character(),
        Type = character(), Limite = numeric(), Stade = numeric(),
        StadeD = numeric(), StadeE = numeric(),
        Nha = numeric(), Gha = numeric(), Vha = numeric(),
        stringsAsFactors = F
      )
    }

  # -- retour de la fonction calculs_bmp
}
