#' Fonction de traitement de la régénération
#' @description La fonction permet de traiter les données d'inventaire de régénération.
#' @return La fonction retourne la table d'entrée avec les variables de résultats.
#' @param df = table d'inventaire en entrée
#' @param code_essreg = table contenant la liste des regroupement d'essences
#' @import dplyr
#' @import rlang
#' @importFrom stats quantile
#' @import stringr
#' @import tidyr
#' @export
calculs_rege <- function(
    df = NULL, code_essreg = NULL
) {
  # initialize variables
  Class1 <- Class2 <- Class3 <- Cycle <- Essence <- NbSousPlac <- NumForet <- NumPlac <- NULL
  RayonSousPlac <- Recouv <- SsPlac <- plac_nb <- NULL
  
  df <-
    df %>%
    left_join(code_essreg, by = c("NumForet", "Essence")) %>%
    filter(!is.na(Essence)) %>% # Il peut y avoir des essences vides si on a fait une Ss-placette et juste noté 1 observation
    # select(-one_of("Rejet","Observations")) %>%
    arrange(NumForet, Cycle, NumPlac, SsPlac) %>%
    # replace(is.na(.),0) %>%
    mutate(
      # EssReg = ifelse(!is.na(EssRegPar), EssRegPar, EssReg),

      Recouv = ifelse(is.na(Recouv), 0, Recouv),
      Class1 = ifelse(is.na(Class1), 0, Class1),
      Class2 = ifelse(is.na(Class2), 0, Class2),
      Class3 = ifelse(is.na(Class3), 0, Class3),

      Recouv = as.numeric(Recouv),
      Class1 = as.numeric(Class1),
      Class2 = as.numeric(Class2),
      Class3 = as.numeric(Class3),

      # Surf = ifelse(Class1 + Class2 + Class3 >= 5, 1, 0),
      plac_nb = NbSousPlac,

      Recouv = Recouv / plac_nb,
      Classe1Ha = Class1 * 10000 / (pi * RayonSousPlac ^ 2) / plac_nb,
      Classe2Ha = Class2 * 10000 / (pi * RayonSousPlac ^ 2) / plac_nb,
      Classe3Ha = Class3 * 10000 / (pi * RayonSousPlac ^ 2) / plac_nb
    )

  # -- retour de la fonction calculs_Reges
  return(df)
}
