##### fonction pour construire la table des combinaisons de données à obtenir #####
build_combination_table <- function(vecteur) {
  df <- data.frame(
    var = vecteur,
    stringsAsFactors = F
  ) %>%
    mutate(
      # essences
      Var1 = ifelse(str_detect(var, "Essence"), "Essence", NA),
      Var1 = ifelse(str_detect(var, "EssReg"), "EssReg", Var1),
      # diamètre
      Var2 = ifelse(str_detect(var, "Classe"), "Classe", NA),
      Var2 = ifelse(str_detect(var, "Cat"), "Cat", Var2),
      # stades bois mort
      Var3 = ifelse(str_detect(var, "StadeD"), "StadeD", NA),
      Var4 = ifelse(str_detect(var, "StadeE"), "StadeE", NA),
      # qualités
      Var5 = ifelse(str_detect(var, "Qual"), "Qual", NA),
      Var5 = ifelse(str_detect(var, "Reg1"), "Reg1", Var5),
      Var5 = ifelse(str_detect(var, "Reg2"), "Reg2", Var5),
      # type de bmp
      Var6 = ifelse(str_detect(var, "Type"), "Type", NA),
      # durée de vie (carbone)
      Var6 = ifelse(str_detect(var, "Lifetime"), "Lifetime", Var6),
      # DMH
      Var6 = ifelse(str_detect(var, "CodeEcolo"), "CodeEcolo", Var6),
      # coupe et rejet
      Var7 = ifelse(str_detect(var, "Coupe"), "Coupe", NA),
      Var7 = ifelse(str_detect(var, "Rejet"), "Rejet", Var7),
      # populations
      Data = ifelse(str_detect(var, "BM"), "BM", NA),
      Data = ifelse(str_detect(var, "BMP"), "BMP", Data),
      Data = ifelse(str_detect(var, "BMS"), "BMS", Data),
      Data = ifelse(str_detect(var, "Fpied"), "Fpied", Data),
      Data = ifelse(str_detect(var, "Per"), "Per", Data),
      Data = ifelse(str_detect(var, "Taillis"), "Taillis", Data),
      # Data = ifelse(str_detect(var, "Den"), "Den", Data),
      Data = ifelse(str_detect(var, "PFutaie"), "PFutaie", Data),

      # Data = ifelse(str_detect(var, "Exploit"), "Exploit", Data),
      Data = ifelse(str_detect(var, "Exploites"), "Exploites", Data),
      Data = ifelse(str_detect(var, "Chablis"), "Chablis", Data),

      Data = ifelse(str_detect(var, "Codes"), "Codes", Data),
      Data = ifelse(str_detect(var, "Carbone"), "Carbone", Data),
      Data = ifelse(str_detect(var, "Rege"), "Rege", Data),
      var = NULL
    ) %>%
    filter(!is.na(Data)) %>% # sécurité
    distinct()

  # retour de la fonction build_combination_table
  return(df)
}
##### /\ #####
