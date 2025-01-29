##### fonction de calcul des moyennes, écart-types et erreurs relatives #####
gf_AgregMoySdEr <- function(
    df = NULL,
    group = "Foret",
    data = "Dendro",
    last_cycle = NULL,
    ponderation_DF = NULL,
    regroup_DF = NULL
) {
  # --- Jonction avec les regroupements choisis (table regroup_DF)
  df <-
    regroup_DF[, c("NumForet", group, "NumPlac", "Cycle", "PoidsPlacette")] %>%
    right_join(df, by = c("NumForet", "NumPlac", "Cycle"))

  # --- Cas où df vide
  if (nrow(df) == 0) {
    df <- df %>% mutate(NumPlac = NULL, PoidsPlacette = NULL)
    return(df)
  }

  # --- variables de résultats
  results_vars <- c(
    # BV
    "Nha", "Gha", "Vha", "VcHa", "VpHa", "AcctD", "AcctV", "AcctVper",
    "AcctG", "AcctGper", "TauxV", "TauxPU", "Taux", "Gain", "Gainper",
    "VcHav", "VpHav", "tCha",
    # BM
    "Vha_BMSinf", "Vha_BMSsup", "Vha_BMPinf", "Vha_BMPsup", "Vha_total",
    "tCha_BMSinf", "tCha_BMSsup", "tCha_BMPinf", "tCha_BMPsup", "tCha_total",
    # régénération
    "Recouv", "Classe1Ha", "Classe2Ha", "Classe3Ha"
  )

  # variables à analyser
  vars <- names(df)[ which(names(df) %in% results_vars) ]
  ncol <- length(vars)
  # définition des variables de regroupement
  group_var <- setdiff(names(df), vars)
  group_var <- group_var[-match(c("NumPlac", "PoidsPlacette"), group_var)]

  # --- Table de résultats
  df <- df %>%
    pivot_longer(
      cols = vars,
      names_to = "var",
      values_to = "value"
    ) %>% arrange(NumPlac) %>%
    # filter(EssReg == "Hêtre" & Cat == "GB" & var == "AcctGper") %>%
    mutate(
      var = factor(var, levels = vars),
      value1 = PoidsPlacette * value,
      value2 = PoidsPlacette * value ^ 2
    ) %>% #
    group_by_at(c(group_var, "Cycle", "var")) %>%
    summarise(
      value1 = sum(value1),
      value2 = sum(value2)
    ) %>%
    ungroup() %>%
    left_join(ponderation_DF, by = c("NumForet", group, "Cycle")) %>% # joindre ponderation_DF pour pondération juste
    mutate(
      poids =
        ifelse(
          str_detect(var, "Acct") | str_detect(var, "Gain"),
          PoidsAcct, Poids
        ),
      nbre =
        ifelse(
          str_detect(var, "Acct") | str_detect(var, "Gain"),
          NbreAcct, Nbre
        ),

      Moy = value1 / poids, # moyenne
      Sd = ((poids * value2 - value1 ^ 2) / poids / (poids - 1)) ^ 0.5, # écart-type
      CV = Sd / Moy * 100, # coefficient de variation
      Er = qt(0.975, nbre) * CV / nbre ^ 0.5 # erreur relative
    ) %>%
    select(
      all_of(group_var), "Cycle", "var", "Poids", "Nbre", "PoidsAcct", "NbreAcct",
      "Moy", "CV", "Er"
    ) %>%
    pivot_longer(
      cols = Moy:Er,
      names_to = "var_result",
      values_to = "value"
    ) %>%
    unite(var, var_result, var) %>%
    mutate(
      var = gsub("Moy_", "", var),
      var = factor(var, levels = c(
        paste0("", vars),
        paste0("CV_", vars),
        paste0("Er_", vars)
      ))
    ) %>%
    select(-Poids, -Nbre, -PoidsAcct, -NbreAcct) %>%

    # arrange var
    arrange(var) %>%

    pivot_wider(
      id_cols = everything(),
      names_from = "var",
      values_from = "value"
    ) %>%
    # spread(var, value, drop = F, fill = 0) %>%
    left_join(ponderation_DF, by = c("NumForet", group, "Cycle")) %>%
    rename(
      "PoidsPlacettes" = "Poids",
      "NbrePlacettes" = "Nbre",
      "PoidsPlacettes_Acct" = "PoidsAcct",
      "NbrePlacettes_Acct" = "NbreAcct"
    ) %>%
    data.frame()
}
