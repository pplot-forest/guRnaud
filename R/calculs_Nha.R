##### fonction de calcul du poids (feuille Arbres) #####
calculs_Nha <- function(df = NULL) {
  ##### Ancienne version
  # # # debug - tests
  # # df <- data.frame(
  # #   Cat = c(rep("PER", 10), rep("PB",5), rep("BM",5)),
  # #   Diam1 = c(
  # #     17, 16, 12, 14, 15, 16, 6, 8, 12, 13,
  # #     20, 18, 21, 27, 28,
  # #     28, 26, 33, 38, 37
  # #   ),
  # #   Diam2 = c(
  # #     17, 18, 13, 14, 15, 16, 6, 6, 12, 13,
  # #     20, 17, 21, 28, 27,
  # #     27, 26, 33, 38, 37
  # #   ),
  # #   Dist = c(
  # #     NA, NA, NA, NA, NA, rep(9, 5),
  # #     7, 6, 4, 10, 8,
  # #     8, 8, 5, 4, 3
  # #   ),
  # #   Rayon1 = c(
  # #     rep(10, 10),
  # #     10, 10, 10, 10, NA,
  # #     10, 10, 10, NA, NA
  # #   ),
  # #   Rayon2 = c(
  # #     rep(20, 10),
  # #     20, 20, 20, 20, NA,
  # #     20, 20, 20, NA, NA
  # #   ),
  # #   Rayon3 = c(
  # #     rep(30, 5), rep(NA, 5),
  # #     rep(NA, 2), rep(30, 3),
  # #     rep(30, 2), rep(NA, 3)
  # #     # 30, 30, 30, NA, NA,
  # #     # 30, 30, 30, NA, NA
  # #   ),
  # #   DiamLim1 = rep(7.5, 20),
  # #   DiamLim2 = rep(17.5, 20),
  # #   DiamLim3 = c(
  # #     rep(27.5, 5), rep(NA, 5),
  # #     rep(NA, 2), rep(27.5, 3),
  # #     rep(27.5, 2), rep(NA, 3)
  # #   ),
  # #   DiamLim = c(
  # #     rep(NA, 5), rep(27.5, 5),
  # #     rep(27.5, 2), rep(NA, 3),
  # #     rep(NA, 2), rep(27.5, 3)
  # #   ),
  # #   Coeff = c(
  # #     rep(NA, 5), rep(0.03, 5),
  # #     rep(0.03, 2), rep(NA, 3),
  # #     rep(NA, 2), rep(0.03, 3)
  # #   ),
  # #   CoeffPente = rep(1, 20),
  # #
  # #   Type = rep(NA, 20)
  # # )
  #
  #
  # # ---------- Cas des perches sans mesure de distance
  # pos <- with(
  #   df,
  #   which(Cat == "PER" & is.na(Dist) & Diam1 >= DiamLim1 & is.na(Type))
  # )
  # # Changement Verif_Calculs : rajout de la condition "Diam1 >= DiamLim1"
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon1[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  #
  # # ---------- Cercles uniques
  # pos <- with(
  #   df,
  #   which(
  #     is.na(Nha) & is.na(DiamLim2) & Diam1 >= DiamLim1 &
  #       Dist <= Rayon1 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon1[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # ---------- Cercles concentriques
  # # -- rappel : possible d'excluer les perches car la définition
  # # de cett catégorie de diamètre est paramétrée dans
  # # la feuille "Cats"
  #
  # # 3eme cercle
  # pos <- with(
  #   df,
  #   which(
  #     is.na(Nha) & !is.na(DiamLim3) & Diam1 >= DiamLim3 &
  #       Dist <= Rayon3 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon3[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # 2eme cercle avec 3eme cercle
  # pos <- with(
  #   df,
  #   which(
  #     is.na(Nha) & !is.na(DiamLim3) & Diam1 >= DiamLim2 & Diam1 < DiamLim3 &
  #       Dist <= Rayon2 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon2[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # 2eme cercle sans 3eme cercle
  # pos <- with(
  #   df,
  #   which(
  #     is.na(Nha) & is.na(DiamLim3) & Diam1 >= DiamLim2 &
  #       Dist <= Rayon2 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon2[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # 1er cercle avec 2eme cercle
  # pos <- with(
  #   df,
  #   which(
  #     is.na(Nha) & !is.na(DiamLim2) & Diam1 >= DiamLim1 &
  #       Diam1 < DiamLim2 & Dist <= Rayon1 * CoeffPente & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / df$Rayon1[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  #
  # # ---------- Angle fixe
  # # exclut les arbres vivants dont le diamètre est < DiamLim
  # pos <- which(df$Diam1 < df$DiamLim & is.na(df$Type))
  # if (length(pos) > 0) {
  #   df[pos, "Coeff"] <- NA
  # }
  # # réinitialise "Nha" et "Limite" pour les arbres vivants (perches incluses !) à considérer dans l'inventaire à angle fixe
  # pos <- which(df$Diam1 >= df$DiamLim & is.na(df$Type)) # Changement Verif_Calculs Diam devient Diam1 -> indispensable sinon arbre de 29 par 32 sera inventorié ni par surface ni par angle fixe
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- NA # Remise à zéro au cas où il y aurait déjà des valeurs renseignées (cercle(s))
  #   df[pos, "Limite"] <- 1
  # }
  # pos <- with(
  #   df,
  #   which(
  #     !is.na(Coeff) & Diam1 >= Dist * Coeff & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10 ^ 4 * df$Coeff[pos] ^ 2 / pi / df$Diam1[pos] ^ 2
  #   df$Limite[pos] <- NA
  # }
  # # cas des Arbres à inventorier par angle relascopique qui auraient été considérés comme non limite parce
  # # que > DiamLim1, mais qui en fait sont hors inventaire
  # # -- Nov 2022 -> commande inutile ?
  # pos <- with(
  #   df,
  #   which(
  #     !is.na(Coeff) & Diam1 < Dist * Coeff & is.na(Type)
  #   )
  # )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- NA
  #   df$Limite[pos] <- 1
  # }
  #
  #
  #
  #
  # # -- spécifique inventaire PP La Madeleine
  # # protocole BMP :
  # # + 1 cercle de 10m pour les Diam < 30
  # # + 1 cercle de 20m pour les Diam >= 30
  # pos <-
  #   with(
  #     df,
  #     which(
  #       is.na(Nha) & Diam1 >= 30 &
  #         Dist <= 20 & !is.na(Type)
  #     )
  #   )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / 20 ^ 2
  #   df$Limite[pos] <- NA
  #   df$TypeTarif[pos] <- "SchL" # default tarif for BMP
  #   df$NumTarif[pos] <- 6
  # }
  # # 1er cercle avec 2eme cercle pour les BMP
  # pos <-
  #   with(
  #     df,
  #     which(
  #       is.na(Nha) & Diam1 < 30 &
  #         Dist <= 10 & !is.na(Type)
  #     )
  #   )
  # if (length(pos) > 0) {
  #   df[pos, "Nha"] <- 10000 / pi / 10 ^ 2
  #   df$Limite[pos] <- NA
  #   df$TypeTarif[pos] <- "SchL" # default tarif for BMP
  #   df$NumTarif[pos] <- 6
  # }
  #
  # # Nha mis à 0 pour les arbres limites
  # pos <- which(df$Limite == 1)
  # if (length(pos) > 0) df[pos, "Nha"] <- 0
  ##### fin Ancienne version
  # tk_messageBox(type = "ok", message = "Attention recaler paramètres d'échantillonnage en attendant modif\n+ cercle 3 ne répond pas")
  df <- df %>%
    mutate(
      Nha = case_when(
        # -- *** perches ***
        # - cas des perches vivantes...
        Cat == "PER" & Diam1 >= DiamLim1 & is.na(Type) &
          # ... sans distance
          is.na(Dist) ~
          # Nha =
          10000 / pi / Rayon1 ^ 2,

        # - cas des perches vivantes...
        Cat == "PER" & Diam1 >= DiamLim1 & is.na(Type) &
          # ... avec distance
          !is.na(Dist) & Dist < Rayon1 ~
          # Nha =
          10000 / pi / Rayon1 ^ 2,


        # -- *** précomtables ***
        # - cas des arbres précomtables vivants ...
        Cat != "PER" & is.na(Type) &
          # --- inventoriés par angle fixe
          Diam1 >= DiamLim & Diam1 >= Dist * Coeff ~
          # Nha =
          10 ^ 4 * Coeff ^ 2 / pi / Diam1 ^ 2,

        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 3ème cercle (2 autres sous-entendus + exclure angle fixe)
          Diam1 < DiamLim & Diam1 >= DiamLim3 & Dist <= Rayon3 * CoeffPente ~
          # Nha =
          10000 / pi / Rayon3 ^ 2,

        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 2ème cercle (1er sous-entendu + exclure angle fixe) avec ou sans 3ème cercle ...
          Diam1 < DiamLim & Diam1 >= DiamLim2 & Dist <= Rayon2 * CoeffPente ~
          # Nha =
          10000 / pi / Rayon2 ^ 2,

        # - cas des arbres précomptables vivants...
        Cat != "PER" & is.na(Type) &
          # ... inventoriés sur le 1er cercle avec ou sans 2ème cercle (exclure angle fixe)
          Diam1 < DiamLim & Diam1 >= DiamLim1 & Dist / CoeffPente <= Rayon1 ~
          # Nha =
          10000 / pi / Rayon1 ^ 2,


        # -- *** bois mort sur pied ***
        # - cas des bois morts sur pied...
        !is.na(Type) & #BMP -> manque argument !!!!
          # >= 30 cm de diamètre
          # ... inventoriés sur le 2ème cercle (1er sous-entendu + exclure angle fixe) avec ou sans 3ème cercle ...
          Diam >= 30 & Dist <= 20 ~
          # Nha =
          10000 / pi / 20 ^ 2,

        # !is.na(Type) &
        #   # >= 30 cm de diamètre
        #   # ... inventoriés par angle fixe
        #   Diam >= 30 & Diam1 >= Dist * Coeff  ~
        #   # Nha =
        #   10 ^ 4 * Coeff ^ 2 / pi / Diam1 ^ 2,

        # - cas des bois morts sur pied...
        !is.na(Type) &
          # < 30 cm de diamètre
          Diam >= DiamLim1 & Diam < 30 & Dist <= 10 ~
          # Nha =
          10000 / pi / 10 ^ 2
      ),

      # définition attribut limite d'un arbre
      Limite = ifelse(is.na(Nha), 1, 0),
      # arbre limite => Nha = 0
      Nha = ifelse(is.na(Nha), 0, Nha)
    ) # debug %>% select(NumPlac, NumArbre, Essence, Azimut, Dist, Diam1, Diam, Type, Nha, DiamLim, Coeff, BMP, DiamLim1, Rayon1, Limite)

  # rm(pos)
  # -- retour fonction calculs_Nha
  return(df)
}
