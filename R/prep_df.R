##### fonction de préparation de la table Arbres #####
# TODO : rajouter les autres tables en arguments
prep_df <- function(
    df = NULL, echant_DF = NULL,
    code_qual = NULL, code_essreg = NULL,
    code_tarif = NULL, code_prix = NULL,
    diam_cat = NULL
) {
  df <-
    df %>%
    # filter(NumPlac == "21_3308") %>% # debug
    left_join(
      code_qual %>% select(-Couleur),
      by = c("NumForet", "Qual" = "Nom")
    ) %>%
    left_join(
      code_essreg %>% select(-Couleur),
      by = c("NumForet", "Essence")
    ) %>%
    mutate(
      Cycle = as.numeric(Cycle),
      # NumArbre = as.numeric(NumArbre),
      Limite = 1,
      Dist = as.numeric(Dist),
      Azimut = as.numeric(Azimut),
      Diam1 = as.numeric(Diam1),
      Diam2 = as.numeric(Diam2),
      Haut = as.numeric(Haut),
      Stade = as.numeric(Stade),
      Diam1 = ifelse(is.na(Diam1), Diam2, Diam1),
      Diam2 = ifelse(is.na(Diam2), Diam1, Diam2),
      Diam1 = ifelse(Diam1 == 0, Diam2, Diam1),
      Diam2 = ifelse(Diam2 == 0, Diam1, Diam2),
      Diam = (Diam1 + Diam2) / 2,
      Classe = floor(Diam / 5 + 0.5) * 5,
      # définition de Cat à partir de Diam et pas Diam1 car on veut que la
      # population ait un sens au point de vue dendrométrique
      # (mais attention, calculs toujours fondés sur Diam1)
      Cat = cut(
        Diam,
        breaks = c(diam_cat$Diam, Inf),
        labels = diam_cat$Cat,
        include.lowest = T,
        right = F
      ),
      Cat = as.character(Cat),
      Nha = NA
    ) %>%
    left_join(code_tarif, by = c("NumForet", "Essence", "Strate")) %>%
    left_join(code_prix, by = c("NumForet", "Essence", "Classe", "Reg1" = "Qual")) %>%
    mutate(PU = ifelse(
      Classe < 7.5 & !is.na(Classe), 0, PU
    )) %>%
    arrange(NumForet, NumPlac, NumArbre, Cycle) %>%
    filter(
      !is.na(Diam1) | !is.na(Diam2) | Diam1 == 0 | Diam2 == 0 # supprime les arbres où les 2 diamètres sont vides
    )

  # -- retour fonction prep_df
  return(df)
}
