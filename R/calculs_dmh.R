#' Fonction de traitement des données de dendromicrohabitats (DMH)
#' @description La fonction récupère les DMH inventoriés sur chaque arbre, décompose et liste l'ensemble des DMH rencontrés dans une table "Codes".
#' @return La fonction retourne la table "Codes" contenant las liste des DMH rencontrés pour des calculs de fréquence/densité.
#' @param df = table d'inventaire en entrée
#' @param dmh_df = table de codification des DMH
#' @import dplyr
#' @import rlang
#' @importFrom stats quantile
#' @import stringr
#' @import tidyr
#' @export
calculs_dmh <- function(
    df = NULL, dmh_df = NULL
) {
  # initialize variables
  Code <- CodeEcolo <- Codification <- Ref_CodeEcolo <- dmh_split <- NULL

  if (nrow(df) > 0) {
    df <- df %>% mutate(Ref_CodeEcolo = tolower(Ref_CodeEcolo))
    df_ProSilva <- df %>% filter(Ref_CodeEcolo == "prosilva")
    df_AFI <- df %>% filter(Ref_CodeEcolo == "engref" | Ref_CodeEcolo == "afi")
    df_EFI <- df %>% filter(Ref_CodeEcolo == "efi")
    df_IRSTEA <- df %>% filter(Ref_CodeEcolo == "irstea")

    # ----- Codification ProSilva
    if (nrow(df_ProSilva) > 0) {
      # ---- décomposition
      # liste
      list <- with(
        df_ProSilva,
        str_split(CodeEcolo, boundary("word"))
      )
      # df
      codes1 <- dmh_split(df_ProSilva, list)
    } else {
      codes1 <- data.frame()
    }

    # ----- Codification AFI
    if (nrow(df_AFI) > 0) {
      # ----- df # TODO : à mettre dans le job2
      df_AFI <- df_AFI %>% mutate(CodeEcolo = str_replace(CodeEcolo, "0", ""))
      # ----- niveaux
      niveaux <-
        dmh_df %>%
        filter(Codification == "engref") %>%
        select(Code) %>%
        unlist()
      # ---- décomposition
      # liste
      list <- c()
      for (i in 1:nrow(df_AFI)) {
        list0 <- with(df_AFI, str_extract(CodeEcolo[i], niveaux))
        list0 <- list0[ !is.na(list0) ]
        list <- c(list, list(list0))
      }
      # df
      codes2 <- dmh_split(df_AFI, list)
    } else {
      codes2 <- data.frame()
    }

    # ----- Codification EFI
    if (nrow(df_EFI) > 0) {
      # ---- décomposition
      # liste
      list <- with(
        df_EFI,
        str_split(CodeEcolo, boundary("word"))
      )
      # df
      codes3 <- dmh_split(df_EFI, list)
    } else {
      codes3 <- data.frame()
    }

    # ----- Codification IRSTEA
    if (nrow(df_IRSTEA) > 0) {
      # ----- df # TODO : à mettre dans le job2
      df_IRSTEA <- df_IRSTEA %>% mutate(CodeEcolo = str_replace(CodeEcolo, "0", ""))
      # ----- niveaux
      niveaux <-
        dmh_df %>%
        filter(Codification == "IRSTEA") %>%
        select(Code) %>%
        unlist()
      # ---- décomposition
      # liste
      list <- c()
      for (i in 1:dim(df_IRSTEA)[1]) {
        list0 <- with(df_IRSTEA, str_extract(CodeEcolo[i], niveaux))
        list0 <- list0[ !is.na(list0) ]
        list <- c(list, list(list0))
      }
      # df
      codes4 <- dmh_split(df_IRSTEA, list)
    } else {
      codes4 <- data.frame()
    }

    codes <- rbind(codes1, codes2, codes3, codes4)
  } else {
    codes <- df
  }

  # retour de la fonction calculs_dmh
  return(codes)
}
