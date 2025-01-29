##### fonction rajout des arbres - placettes miroir #####
miroir_add <- function(df, PCQM = F) {
  # df <- Arbres # debug
  # df <- Taillis_PCQM # debug
  # plac_df <- Placettes # debug
  miroir_DF <-
    # jonction with plot df needed beforehand
    df %>% filter(!is.na(Miroir_Azimut) & !is.na(Miroir_Dist))

  if (dim(miroir_DF)[1] > 0) {
    miroir_DF <-
      miroir_DF %>%
      # mutate(Azimut = c(0, 100, 200, 300), Dist = rep(1, 4), Miroir_Azimut = 300, Miroir_Dist = 1) %>% # debug
      mutate(
        # Azimut = Azimut + 400,

        # - ancienne version avant commande modulo :) !!!
        # theta = 500 - Azimut,
        # theta = (theta/400 - floor(theta/400)) * 400, # même sens que coordonnées polaires
        theta = (500 - Azimut) %% 400,

        # theta = theta * pi / 200, # conversion en radians
        X = Dist * cos(theta * pi / 200),
        Y = Dist * sin(theta * pi / 200),
        # Azimut = Azimut + 400,
        # coordonnées du point miroir
        Miroir_Azimut =
          ifelse(Miroir_Azimut < 200, Miroir_Azimut + 200, Miroir_Azimut - 200), # inverse la position du centre miroir
        Miroir_theta = 500 - Miroir_Azimut,
        Miroir_theta = (Miroir_theta/400 - floor(Miroir_theta/400)) * 400, # même sens que coordonnées polaires
        # Miroir_theta = Miroir_theta * pi / 200, # conversion en radians
        Miroir_Dist = Miroir_Dist * 2,
        c_X = Miroir_Dist * cos(Miroir_theta * pi / 200),
        c_Y = Miroir_Dist * sin(Miroir_theta * pi / 200),

        X = X + c_X, # coordonnées symétrie du centre placette miroir
        Y = Y + c_Y,

        X = ifelse(abs(X) < 10^(-10), 0, X),
        Y =  ifelse(abs(Y) < 10^(-10), 0, Y),
        # Retrouve Azimut et Distance MAJ
        theta = ifelse(X != 0, atan(Y/X) * 200 / pi, 0),
        Dist = sqrt(X^2 + Y^2),
        COS = X/Dist,
        SIN = Y/Dist,
        # Dist = round(Dist, digits = 1),

        Azimut = ifelse(COS < 0 & theta != 0, 300 - theta, 0),
        Azimut = ifelse(COS > 0 & theta != 0, 100 - theta, Azimut),
        Azimut = ifelse(COS > 0 & Y == 0, 100, Azimut)
      ) %>%
      # suppression des variables
      select(names(df))

    df <- rbind(df, miroir_DF) # Rajouter 400 grd aux azimuts ?
    # Redéfinir les éléments des 4 quarts pour le PCQM
    if (PCQM == T) {
      df <-
        df %>%
        mutate(
          Quart = cut(
            Azimut,
            breaks = c(0, 100, 200, 300, 400),
            labels = c("NE", "SE", "SO", "NO"),
            include.lowest = T,
            right = T
          )
        ) %>%
        group_by(NumForet, NumPlac, Population, Quart) %>%
        filter(Dist == min(Dist)) %>%
        ungroup()
    }
  }

  # -- retour fonction miroir_add
  return(df)
}
