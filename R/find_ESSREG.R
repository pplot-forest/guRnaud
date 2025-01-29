#### fonction pour accoler des EssReg par défaut à une table #####
find_ESSREG <- function(
    df = NULL, code_essences = NULL
) {
  pos <- which(is.na(df$EssReg))

  if (length(pos) > 0) {
    essences_LISTE <- unique(df$Essence[pos])
    essences_DF <- filter(code_essences, Nom %in% essences_LISTE) %>% select(Nom, Reg)
    df <-
      df %>%
      left_join(essences_DF, by = c("Essence" = "Nom")) %>%
      mutate(EssReg = ifelse(is.na(EssReg), Reg, EssReg)) %>%
      select(-Reg)
  }

  # -- retour de la fonction find_ESSREG
  return(df)
}
