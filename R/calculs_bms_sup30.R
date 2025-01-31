#' Fonction de calcul des volumes de bois mort au sol > 30 cm 
#' @description La fonction permet de calculer les variables de résultats des bois mort au sol > 30 cm .
#' @return La fonction retourne la table d'entrée avec les variables de résultats.
#' @param df = table d'inventaire en entrée
#' @param code_essreg = table listant les regroupements d'essence
#' @import dplyr
#' @import rlang
#' @importFrom stats quantile
#' @import stringr
#' @import tidyr
#' @export
calculs_bms_sup30 <- function(
    df = NULL, code_essreg = NULL
) { # TODO : si un jour besoin changement protocole, cf PermPSDRF2
  # initialize variables
  Cat <- Classe <- Cycle <- DiamFin <- DiamIni <- DiamMed <- EssReg <- Essence <- Longueur <- NULL
  NumForet <- NumPlac <- Rayon <- Stade <- StadeD <- StadeE <- Vha <- NULL
  
  df <-
    if (nrow(df) > 0) {
      df %>%
        rename("Rayon" = "BMSCercle") %>%
        mutate(
          DiamIni = ifelse(is.na(DiamIni), 0, DiamIni),
          DiamMed = ifelse(is.na(DiamMed), 0, DiamMed),
          DiamFin = ifelse(is.na(DiamFin), 0, DiamFin),
          Vha = 0,
          Classe = 0,
          # ---- formule de Huber
          Vha = ifelse(
            (DiamIni+ DiamFin) == 0,
            pi / 40000 * DiamMed ^ 2 * Longueur * 10000 / pi / Rayon ^ 2,
            Vha
          ),
          Classe = ifelse(
            (DiamIni + DiamFin) == 0,
            floor(DiamMed / 5 + 0.5) * 5,
            Classe
          ),
          StadeE = floor(Stade / 10),
          StadeD = Stade - StadeE * 10,
          # ---- formule de Smalian
          Vha = ifelse(
            (DiamIni + DiamFin) != 0 & DiamMed == 0,
            pi / 80000 * (DiamIni ^ 2 + DiamFin ^ 2) *
              Longueur * 10000 / pi / Rayon ^ 2,
            Vha
          ),
          Classe = ifelse(
            (DiamIni + DiamFin) != 0 & DiamMed == 0,
            floor((DiamIni + DiamFin) / 2 / 5 + 0.5) * 5,
            Classe
          ),
          # ---- formule de Newton
          Vha = ifelse(
            (DiamIni + DiamFin) != 0 & DiamMed != 0,
            pi / 240000 * (DiamIni ^ 2 + DiamFin ^ 2 + 4 * DiamMed ^ 2) *
              Longueur * 10000 / (pi * Rayon ^ 2),
            Vha
          ),
          Classe = ifelse(
            (DiamIni + DiamFin) != 0 & DiamMed != 0,
            floor((DiamIni + DiamFin + DiamMed) / 3 / 5 + 0.5) * 5,
            Classe
          )
        ) %>%
        mutate(
          Cat = cut(
            Classe,
            breaks = c(0, 17.5, 27.5, 47.5, 67.5, 500),
            labels = c("PER", "PB", "BM", "GB", "TGB"),
            include.lowest = T,
            right = F
          ),
          Cat = as.character(Cat)
        ) %>%
        left_join(code_essreg, by = c("NumForet", "Essence")) %>%
        # TODO : add find_ESSREG ? sur autres tables aussi
        mutate(EssReg = as.character(EssReg)) %>%
        select(
          NumForet, NumPlac, Cycle, Essence, EssReg,
          DiamIni, DiamMed, DiamFin,
          Classe, Cat, Stade, StadeD, StadeE, Vha
        )
    } else {
      BMSsup30 <-
        data.frame(
          NumForet = numeric(), NumPlac = character(),
          Cycle = numeric(),
          Essence = character(), EssReg = character(),
          Diam = numeric(), Classe = numeric(),
          Cat = character(),
          Stade = numeric(),
          StadeD = numeric(), StadeE = numeric(), Vha = numeric(),
          stringsAsFactors = F
        )
    }

  # -- retour de la fonction calculs_BMSsup30
  return(df)
}
