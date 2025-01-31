#' Fonction de calcul du volume des arbres
#' @description La fonction calcule le volume de chaque arbre en fonction des tarifs de cubage choisis
#' @return La fonction renvoie la table d'entrée avec la variable du volume des arbres (Vha)
#' @param df = table d'inventaire en entrée
#' @param IFN = variable permettant de rajouter un volume "IFN"
#' @param Sup = variable permettant de calculer le volume pour la classe de diamètre au-dessus de celle de l'arbre 
#' (utile dans les calculs de variables économiques)
#' @import dplyr
#' @import stringr
#' @import rlang
#' @import tidyr
#' @export
calculs_Vol <- function(df = NULL, IFN = F, Sup = F) {
  # initialize variables
  Gha <- Haut <- Nha <- Type <- calculs_Eco <- NULL
  
  add_var <- ""
  add_Tarif <- ""
  add_Diam <- ""
  if (IFN == T) {
    add_var <- "IFN"
    add_Tarif <- "IFN"
    add_Diam <- ""
  }
  if (Sup == T) {
    add_var <- "Sup"
    add_Tarif <- ""
    add_Diam <- "Sup"
  }

  # Pour mémo (1) fonctionnement sans argument "add_var" :
  # var <- enquo(var)
  # var_name <- quo_name(var) # puis appel !!var_name: = f(!!var) dans mutate

  # Pour mémo (2) fonctionnement avec argument d'entrée définissant la variable (Vha)
  # var <- enquo(var)
  # var_name <- paste0(quo_name(var), add_var)

  # construction des variables
  var_name <- paste0("Vha", add_var)
  var <- quo(!!parse_expr(var_name))
  # variable = Type de tarif
  TypeTarif <- paste0("TypeTarif", add_Tarif)
  TypeTarif <- quo(!!parse_expr(TypeTarif))
  # variable = Numéro de tarif
  NumTarif <- paste0("NumTarif", add_Tarif)
  NumTarif <- quo(!!parse_expr(NumTarif))
  # variable = Diamètre
  Diam <- paste0("Diam", add_Diam)
  Diam <- quo(!!parse_expr(Diam))

  # calculs des volumes selon les tarifs de cubage
  df <-
    df %>%
    mutate(
      !!var_name := NA,
      !!var_name :=
        ifelse(
          !!TypeTarif == "SchR",
          5 / 70000 * (8 + !!NumTarif) * (!!Diam - 5) * (!!Diam - 10) * Nha,
          !!var
        ),
      !!var_name :=
        ifelse(
          !!TypeTarif == "SchI",
          5 / 80000 * (8 + !!NumTarif) * (!!Diam - 2.5) * (!!Diam - 7.5) * Nha,
          !!var
        ),
      !!var_name :=
        ifelse(
          !!TypeTarif == "SchL",
          5 / 90000*(8 + !!NumTarif) * (!!Diam - 5)* !!Diam * Nha,
          !!var
        ),
      !!var_name :=
        ifelse(
          !!TypeTarif == "SchTL",
          5 / 101250 * (8 + !!NumTarif) * !!Diam * !!Diam * Nha,
          !!var
        ),
      !!var_name :=
        ifelse(
          !!var < 0, 0, !!var
        ), # sécurité pour les tiges de moins de 10 # A revoir ?

      # cas des chandelles et des souches :
      !!var_name :=
        ifelse(
          !is.na(Type) & Type %in% c("C", "S"),
          ifelse(
            is.na(Haut), 8 * Gha,
            # calcul du volume à partir de la hauteur (diamètre médian et décroissance métrique = 1cm/m par défaut)
            pi / 40000 * (!!Diam - (Haut / 2 - 1.30)) ^ 2 * Haut  ) * Nha,
          !!var
        )
    )
  # retour fonction calculs_Vol
  return(df)
}
