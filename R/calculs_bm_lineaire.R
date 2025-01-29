
##### fonction de traitement du bois mort au sol < 30 cm #####
calculs_bm_lineaire <- function(
    df = NULL, code_essreg = NULL
) {
  df <- if (nrow(df) > 0) {
    df %>%
      # rename("Lineaire" = "BMSLin\u00E9aire") %>%
      rename("Lineaire" = "BMSLineaire") %>%
      mutate(
        Angle = ifelse(is.na(Angle), 0, Angle),
        Classe = floor(Diam / 5 + 0.5) * 5,
        Cat = cut(
          Diam,
          breaks = c(0, 17.5, 27.5, 47.5, 67.5, 200),
          labels = c("PER", "PB", "BM", "GB","TGB"),
          include.lowest = T,
          right = F
        ),
        Cat = as.character(Cat),
        StadeE = floor(Stade / 10),
        StadeD = Stade - StadeE * 10,
        Vha = pi ^ 2 / 8 / Lineaire * Diam ^ 2 / cos(Angle / 180 * pi)
      ) %>%
      left_join(code_essreg, by = c("NumForet", "Essence")) %>%
      mutate(EssReg = as.character(EssReg)) %>%
      select(
        NumForet, NumPlac, Cycle, Transect, Essence, EssReg,
        Diam, Classe, Cat, Stade, StadeD, StadeE, Vha
      )
  } else { # TODO : raccourcir avec quotes ?
    data.frame(
      NumForet = numeric(), NumPlac = character(),
      Cycle = numeric(),
      Transect = character(), Essence = character(),
      EssReg = character(), Diam = numeric(),
      Classe = numeric(), Cat = character(),
      Stade = numeric(),
      StadeD = numeric(), StadeE = numeric(), Vha = numeric(),
      stringsAsFactors = F
    )
  }

  # -- retour de la fonction calculs_BMSLineaires
  return(df)
}
