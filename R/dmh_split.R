##### fonction dmh_split #####
dmh_split <- function (
    df0 = NULL,
    list = list
) {
  df <- data.frame(
    NumForet = rep.int(df0$NumForet, sapply(list, length)),
    NumPlac = rep.int(df0$NumPlac, sapply(list, length)),
    NumArbre = rep.int(df0$NumArbre, sapply(list, length)),
    CodeEcolo = unlist(list),
    stringsAsFactors  =  F
  )
  # suppression de la colonne CodeEcolo
  df0 <- df0 %>% select(-CodeEcolo)
  # fusion de df et df0
  df <- df %>% left_join(df0, by = c("NumForet", "NumPlac", "NumArbre"))

  # -- retour de dmh_split
  return(df)
}
