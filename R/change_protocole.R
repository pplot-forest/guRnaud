##### fonction change_protocole : extrait de la table Echantillonnages les cycles concernés par le changement de protocole (s'il existe) #####
change_protocole <- function (echant_DF) {
  # echant_DF <- Echantillonnages # debug
  # on détecte si le protocole change à travers les cycles d'inventaire
  # echant_NAMES <- syms(setdiff(names(echant_DF), "Cycle"))
  echant_NAMES <- c(
    "NumForet", "Strate", "DiamLim1", "Rayon1", "DiamLim2", "Rayon2",
    "DiamLim3", "Rayon3", "Coeff", "DiamLim"
  )
  echant_DF <-
    echant_DF %>%
    distinct(!!!syms(echant_NAMES), .keep_all = T)
  # on recrée une table "echant_DF" avec les paramètres du plus
  # grand dénominateur commun entre les 2 (ou plus) protocoles d'inventaire
  # différents
  echant_DF <-
    echant_DF %>%
    group_by(NumForet, Strate) %>%
    mutate(
      DiamLim1 = min(DiamLim1),
      Rayon1 = min(Rayon1),

      DiamLim2 = min(DiamLim2),
      Rayon2 = min(Rayon2),

      DiamLim3 = min(DiamLim3),
      Rayon3 = min(Rayon3),

      DiamLim = min(DiamLim),
      Coeff = max(Coeff),

      # Identifiant pour retrouver les arbres concernés par le
      # changement de protocole
      echant_ID =
        paste0(
          NumForet, "-", Cycle,"-", Strate
        ),
      Observations = NULL
    ) %>%
    ungroup()
  # retour fonction change_protocole
  return(echant_DF)
}
