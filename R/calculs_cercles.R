#' Fonction de calcul des résultats sur le cercle
#' @description La fonction permet de calculer les variables de résultats pour la/les population(s) inventoriées sur un cercle de rayon défini.
#' @return La fonction retourne la table d'entrée avec les variables de résultats.
#' @param df = table d'inventaire en entrée
#' @param population = définition de la population concernée
#' @param add_bmp_vars = variable indiquant si des variables de résultat propres au bois mort sont à rajouter
#' @param code_essreg = table contenant la liste des regroupement d'essences
#' @param diam_cat = table listant les différentes catégories de diamètre
#' @import dplyr
#' @import rlang
#' @importFrom stats quantile
#' @import stringr
#' @import tidyr
#' @export
calculs_cercles <- function(
    df = NULL, population = NULL, dist_max = NULL,
    add_bmp_vars = F, code_essreg = NULL,
    diam_cat = NULL
) {
  # initialize variables
  Cat <- Diam <- Nbre <- Nha <- Population <- var <- NULL
  
  # id vars
  id_vars <- c("NumForet", "NumPlac", "Cycle")

  # bmp vars to add
  attribute_vars <- c(
    "Essence", "EssReg", "Diam", "Classe", "Cat",
    "Haut", "Type", "Stade"
  )
  if (add_bmp_vars == F) {
    attribute_vars <-
      setdiff(attribute_vars, c("Haut", "Type", "Stade"))
  }

  # results_vars
  results_vars <- c("Nha", "Gha", "Vha")
  vars_to_select <- c(
    id_vars, attribute_vars, results_vars
  )

  # quote
  quo_population <- quo(!!parse_expr(population))

  # process
  df <-
    if (nrow(df) > 0) {
      df %>%
        filter(Population == population) %>%
        mutate(
          # !!population := ifelse(
          #   is.na(!!quo_population),
          #   dist_max, # valeur par défaut
          #   !!quo_population
          # ),
          var = !!quo_population,
          var = ifelse(
            is.na(var),
            dist_max, # valeur par défaut
            var
          ),
          Nha = 10000 / pi/ (var ^ 2) * Nbre,
          Gha = pi / 40000 * Diam ^ 2 * Nha,
          Classe = floor(Diam / 5 + 0.5) * 5,
          Cat = cut(
            Diam,
            breaks = c(diam_cat$Diam, Inf),
            labels = diam_cat$Cat,
            include.lowest = T, right = F
          ),
          Cat = as.character(Cat),
          # Vha = Gha * 7
          Vha = case_when(
            # if population = "Taillis" then volume = Gha * 7
            population == "Taillis" ~ Gha * 7,
            # if population = "BMP" with empty heights then volume = Gha * 8 ...
            is.na(Haut) & population == "BMP" ~ Gha * 8,
            # ... else volume -> cyclindre
            !is.na(Haut) & population == "BMP" ~ pi / 40000 * (Diam - (Haut / 2 - 1.30)) ^ 2 * Haut * Nha,
            TRUE ~ 0
          )
        ) %>%
        left_join(code_essreg, by = c("NumForet", "Essence")) %>%
        select(
          all_of(vars_to_select)
        ) #%>% # TODO : à contrôler
      # find_ESSREG()
    } else {
      data.frame(
        NumForet = numeric(), NumPlac = character(),
        Cycle = numeric(), Essence = character(),
        EssReg = character(), Diam = numeric(),
        Classe = numeric(), Cat = character(),
        Nha = numeric(), Gha = numeric(), Vha = numeric(),
        stringsAsFactors = F
      ) %>%
        select( all_of(vars_to_select) )
    }

  # -- retour de la fonction calculs_cercles
  return(df)
}
