##### fonction de partage des accroissements en diamètre calculés aux autres arbres #####
share_accD <- function(df = NULL, echant_change = F) {
  ### ----- Table des accroissements en diamètre ----- ###
  # Attribution d'une valeur d'accroissement en diamètre (si Acct_Diam vide)
  AcctD_DF <-
    df %>% # s'assurer qu'on a bien des arbres limites dans la table
    select(
      IdArbre, NumForet, NumPlac, NumArbre, Cycle, Essence,
      Classe, Acct_Diam, Coupe
    ) %>%

    # valeurs moyennes d'AcctD attribuées par forêt, essence et classe
    group_by(NumForet, Essence, Classe) %>%
    mutate(AcctD_ForetEssClasse = mean(Acct_Diam, na.rm = T)) %>%
    # sinon valeurs moyennes d'AcctD attribuées par forêt et par essence
    group_by(NumForet, Essence) %>%
    mutate(AcctD_ForetEss = mean(Acct_Diam, na.rm = T)) %>%
    # sinon valeurs moyennes d'AcctD attribuées par forêt
    group_by(NumForet) %>%
    mutate(AcctD_Foret = mean(Acct_Diam, na.rm = T)) %>%
    ungroup() %>%

    # distribution des valeurs d'AcctD moyennes calculées
    mutate(
      AcctD =
        ifelse(
          is.na(Acct_Diam), AcctD_ForetEssClasse, Acct_Diam
        ),
      AcctD =
        ifelse(
          is.na(AcctD), AcctD_ForetEss, AcctD
        ),
      AcctD =
        ifelse(
          is.na(AcctD), AcctD_Foret, AcctD
        )
    ) %>%

    # on remplace les valeurs d'AcctD du cycle 1 par les valeurs du cycle 2
    group_by(NumForet, NumPlac, NumArbre) %>%
    mutate(
      AcctD =
        ifelse(
          Cycle == 1 & !(Coupe %in% c("E", "C")), lead(AcctD), AcctD
        )
    ) %>%
    ungroup() %>%

    select(IdArbre, NumForet, NumPlac, NumArbre, Cycle, AcctD)

  # Récupération des AcctD dans la table principale
  df <-
    df %>%
    left_join(
      AcctD_DF,
      c("IdArbre", "NumForet", "NumPlac", "NumArbre", "Cycle")
    )
  if (echant_change == F) {
    df <-
      df %>%
      mutate(echant_ID = NA) %>%
      # selection de variables
      select(
        NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate,
        Essence, EssReg, Azimut, Dist,

        NumTarif, TypeTarif, TypeEss, CoefHoupp, TauxCarbone, InfraDensite,

        Diam1, Diam2, Diam, Classe, Cat,
        DiamSup, ClasseSup, VhaSup, TauxV,
        Qual, Reg1, Reg2, PU, PUSup,
        Nha, Gha, Vha, VhaIFN, VcHa,
        Coupe, Limite,
        CodeEcolo, Ref_CodeEcolo,
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations,

        AcctGper, AcctVper, Gainper, AcctD,
        Coupe, time_span
      )
  } else {
    # selection de variables
    df <-
      df %>%
      select(
        NumForet, NumPlac, NumArbre, IdArbre, Cycle, Strate,
        Essence, EssReg, Azimut, Dist,
        Diam1, Diam2, Diam, Classe, Cat,
        DiamSup, ClasseSup, VhaSup, TauxV,
        Qual, Reg1, Reg2, PU, PUSup,
        Nha, Gha, Vha, VhaIFN, VcHa,
        Coupe, Limite, echant_ID,
        CodeEcolo, Ref_CodeEcolo,
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations,

        AcctGper, AcctVper, Gainper, AcctD,
        Coupe, time_span
      )
  }
  # Changer les noms !!!
  # + Raccorder à AccD lorsqu'il n'y a qu'un seul cycle
  # + Attention aux exigences du modèle .xls

  # -- return of 'share_accD' function
  return(df)
}
