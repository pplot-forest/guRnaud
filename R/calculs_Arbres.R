##### fonction de calcul du Nha, Gha, Vha #####
calculs_Arbres <- function(
    df = NULL, echant_change = F,
    code_qual = NULL, code_essreg = NULL,
    code_tarif = NULL, code_prix = NULL,
    diam_cat = NULL
) {
  # df <- Arbres # debug
  # df <- Arbres_Acct # debug
  # echant_DF <- Echantillonnages # debug
  # -- préparation
  df <-
    df %>%
    prep_df(
      echant_DF,
      code_qual = code_qual, code_essreg = code_essreg,
      code_tarif = code_tarif, code_prix = code_prix,
      diam_cat = diam_cat
    )

  # -- calcul du poids
  df <- df %>% calculs_Nha()

  # -- calcul de la surface terrière
  df$Gha <- pi * df$Diam1 ^ 2 / 40000 * df$Nha # Utiliser Diam1 pour retrouver le coeff relascopique (2 à 2%)

  # -- calcul du volume
  # volume gestionnaire
  df <- calculs_Vol(df)
  # volume IFN
  df <- calculs_Vol(df, IFN = T)

  # -- calcul des variables économiques
  df <- calculs_Eco(df, code_prix)

  if (echant_change == F) {
    # selection de variables
    df <-
      df %>%
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
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations
      )
  } else {
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
        Coupe, Limite, echant_ID,
        CodeEcolo, Ref_CodeEcolo,
        Type, Haut, Stade, Caract1, Caract2, Caract3, Observations
      )
  }

  # -- retour fonction calculs_Arbres
  return(df)
}
