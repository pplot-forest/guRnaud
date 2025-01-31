#' Fonction de calcul du carbone dans le bois mort au sol 
#' @description La fonction permet de calculer les variables de résultats relatives au stock de carbone dans le bois mort.
#' @return La fonction retourne la table d'entrée avec les variables de résultats relatives au stock de carbone.
#' @param df = table d'inventaire en entrée
#' @param living_trees_table = table contenant les données de bois vivant (utilisée pour déterminer la liste des essences majortaires),
#' @param species_table = table des essences
#' @param type = type de bois mort ("Bois mort au sol" ou "Bois mort sur pied")
#' @param dead_wood_carbon_content = table de données carbone
#' @param dead_wood_density = table de données sur la densité de bois par essence
#' @param decomposition_stage_code = table listant les stades de décomposition rencontrés dans l'inventaire
#' @import dplyr
#' @import stringr
#' @import rlang
#' @import tidyr
#' @importFrom stats quantile
#' @export
calculs_BM_carbone = function(
    df = NULL,
    living_trees_table = NULL,
    species_table = NULL,
    type = NULL, # "Bois mort au sol" ou "Bois mort sur pied"
    dead_wood_carbon_content = NULL,
    dead_wood_density = NULL,
    decomposition_stage_code = NULL
){
  # initialize variables
  Code <- Cycle <- Essence <- Infradensite <- NumForet <- NumPlac <- SRF <- StadeD <- StadeE <- Stade_AFI <- NULL
  Taux_carbone <- Type <- TypeEss <- Vha <- main_species <- species_to_join <- time_span <- NULL
  
  # certaines essences bois mort non comptées car pas de données (ex Pin L)
  # associer les Ind. à l'essence majoritaire de la placette

  # -- set up
  # main species by plot
  main_species_by_plot_and_cycle <-
    living_trees_table %>%
    group_by(NumForet, Cycle, NumPlac, Essence) %>%
    summarise(Vha = sum(Vha)) %>%
    filter(Vha == max(Vha)) %>%
    rename(main_species = Essence) %>%
    select(-Vha)

  # filter dead_wood_carbon_content & dead_wood_density
  dead_wood_carbon_content_filtered <-
    dead_wood_carbon_content %>%
    filter(Type == type) %>%
    select(-Type)
  dead_wood_density_filtered <-
    dead_wood_density %>%
    filter(Type == type) %>%
    select(-Type)

  # -- processing
  df <-
    df %>%
    mutate(
      Stade_AFI = paste(StadeE, StadeD, sep = "."),
      time_span = paste(".", StadeD, sep ="")
    ) %>%
    left_join(
      decomposition_stage_code,
      by = c("time_span" = "Stade_AFI")
    ) %>%
    mutate(
      Code = case_when(
        Stade_AFI == "1.1" ~ 1,
        Stade_AFI == "2.1" ~ 1,
        Stade_AFI == "4.4" ~ 5,
        Stade_AFI == "4.5" ~ 5,
        TRUE ~ Code
      )
    ) %>%
    # essence majoritaire du dispositif pour bois mort indéterminé
    left_join(
      main_species_by_plot_and_cycle,
      by = c("NumForet", "Cycle", "NumPlac")
    ) %>%
    mutate(
      species_to_join = ifelse(
        Essence %in% c(
          "Ind.", "Indéterminée", "Non relevée",
          "Résineux Ind.", "Feuillus Ind.", "Feuillus", "Résineux"
        ),
        main_species,
        Essence
      ),
      # sécurité si table vide (Essence doit être character pour jonction)
      species_to_join = as.character(species_to_join)
    ) %>%
    left_join(
      species_table[, c("Nom", "TypeEss")],
      by = c("species_to_join" = "Nom")
    ) %>%
    # taux de carbone dans feuillus/resineux en fonction stade
    left_join(
      dead_wood_carbon_content_filtered,
      by = c("TypeEss", "Code")
    ) %>%
    left_join(
      dead_wood_density_filtered,
      by = c("species_to_join" = "Essence", "Code")
    ) %>%
    # carbone en tC/ha
    mutate(
      tCha = Vha * Infradensite * SRF * (Taux_carbone) / 100
    ) %>%
    select(
      -Stade_AFI, -time_span, -Code, -main_species,
      -TypeEss, -Taux_carbone, -SRF, -Infradensite, -species_to_join
    )

  # -- return of 'calculs_BM_carbone' function
  return(df)
}
