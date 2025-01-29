##### fonction de calcul des variables éconoomiques #####
calculs_Eco <- function(
    df = NULL, code_prix = NULL
) {
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
