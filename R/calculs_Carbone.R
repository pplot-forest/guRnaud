#' Fonction de calcul des quantités de carbone pour les arbres vivants
#' @description La fonction permet de calculer les variables de résultats relatives au stock de carbone dans le bois vivant sur pied.
#' @return La fonction retourne la table d'entrée avec les variables de résultats.
#' @param df = table d'inventaire en entrée
#' @param height_by_class_and_NumTarif = à préciser
#' @param DecroissanceRx = à préciser
#' @param yield_table = table de rendements - à préciser
#' @param DureeVieD = à préciser
#' @param DureeVieConnexes = à préciser
#' @import dplyr
#' @import rlang
#' @importFrom stats quantile
#' @import stringr
#' @import tidyr
#' @export
calculs_Carbone <- function(
    df = NULL,
    height_by_class_and_NumTarif = NULL,
    DecroissanceRx = NULL,
    yield_table = NULL,
    DureeVieD = NULL,
    DureeVieConnexes = NULL
) {
  # initialize variables
  CoefHoupp <- Cycle <- Diam <- Hauteur <- IdArbre <- InfraDensite <- Nha <- NumTarif <- Poids <- Reg1 <- NULL
  TauxCarbone <- TypeEss <- TypeTarif <- V1 <- V2 <- V3 <- VSch <- billon <- duree_vie <- produit <- NULL
  tCha <- volume <- yield_billon <- yield_connexe <- NULL
  
  
  # # -- table des rendements
  # yield_table <- tibble(
  #   qual = c("A", "B", "C", "D"),
  #   yield = c(0.6, 0.6, 0.6, 0.99)
  # )

  # -- table principale
  # df <- Arbres # debug
  df <-
    df %>%

    # jonction de la table Algan pour avoir les hauteurs de grume (billons 1 et 2) en fonction du numéro de tarif et de la classe de diamètre
    left_join(
      height_by_class_and_NumTarif,
      by = c("NumTarif" = "Num", "Classe")
    ) %>%

    mutate(
      # -- Rappel du poids
      Poids = Nha,

      # --- calcul des volumes
      # calcul du volume schaeffer (= total des 2 billons)
      VSch = ifelse(
        TypeTarif == "SchR",
        # Schaeffer rapide
        5 / 70000 * (8 + NumTarif) * (Diam - 5) * (Diam - 10) * Poids,
        # Schaeffer lent
        5 / 90000 * (8 + NumTarif) * (Diam - 5) * Diam * Poids
      ),
      # volume du billon 1 (les Fs sur 3m et les Rx sur 6m)
      V1 = ifelse(
        TypeEss == "Fs",
        # formule du cylindre avec diamètre médian à 1,3 m (~1,5 m)
        pi * Diam ^ 2 / 40000 * 3 * Poids,
        # formule du cylindre avec diamètre médian à 3 m
        pi * (Diam - (DecroissanceRx * 1.7)) ^ 2 / 40000 * 6 * Poids),
      # volume du billon 2 (de 3 m à la decoupe gestionnaire)
      # HYPOTHESE ==> Pour les petits diamètre le volume Schaeffer est considéré nul <== HYPOTHESE #
      V2 = ifelse(VSch - V1 < 0, 0, VSch - V1),
      # volume du billon 3 (qualite fixee a D) = houppier
      V3 = CoefHoupp * VSch,

      # -- valeur du taux de carbone
      TauxCarbone = case_when(
        !is.na(TauxCarbone) ~ TauxCarbone / 100,
        # essence avec taux carbone vide ont taux moyen
        is.na(TauxCarbone) & TypeEss == "Rx" ~ 0.508,
        is.na(TauxCarbone) & TypeEss == "Fs" ~ 0.488
      )) %>% # ,
    select(IdArbre, Cycle, Poids, Diam, Hauteur, TypeEss, TauxCarbone, InfraDensite, Reg1, V1, V2, V3) %>%

    # pivot volumes V1, V2 et V3
    pivot_longer(
      cols = c("V1", "V2", "V3"),
      names_to = "billon",
      names_pattern = "V(.)",
      names_transform = list(billon = as.numeric),
      values_to = "volume"
    ) %>%

    # -- Définition des qualités des billons
    mutate(
      # qualités des billons
      qual_billon = case_when(
        billon == 1 ~ Reg1,
        # HYPOTHESE ==> qualites du billon 2 (de 3m à la decoupe gestionnaire) :
        # si A, B ou C au premier billon alors cela sera du C et si D alors D pour le reste)
        billon == 2 & Reg1 != "D" ~ "C",
        billon == 2 & Reg1 == "D" ~ "D",
        billon == 3 ~ "D"
      ),
      # qualités des connexes
      qual_connexe = "D",
      # suppression de la qualité globale de la grume (Reg1)
      Reg1 = NULL
    ) %>%

    # jonction de la table des rendements de sciage
    left_join(yield_table, by = c("qual_billon" = "qual")) %>%
    left_join(yield_table, by = c("qual_connexe" = "qual"), suffix = c("_billon", "_connexe")) %>%

    mutate(
      # -- calculs des quantités de carbone
      # quantités de carbone des billons
      tCha_billon = volume * TauxCarbone * yield_billon * InfraDensite,
      # quantités de carbone des billons - connexes de scierie
      tCha_connexe = volume * TauxCarbone * (yield_connexe - yield_billon) * InfraDensite,
      # suppression des variables de rendement
      yield_billon = NULL,
      yield_connexe = NULL

    ) %>%

    # pivot qual
    pivot_longer(
      cols = c("qual_billon", "qual_connexe", "tCha_billon", "tCha_connexe"),
      names_to = c(".value", "produit"),
      names_pattern = "(.*)_(.*)"
      # values_to = c("qual", "tCha") # pas nécessaire
    ) %>%

    mutate(
      # -- calculs des durées de vie
      # hauteurs
      # Pour avoir la duree de vie d'un billon, il faut son diamètre médian.
      # Pour avoir le diamètre médian du billon 2, on utilise le volume Schaeffer (V2) et
      # la hauteur de la table Algan (tarifs gradues Algan (1961)).
      # -> on réajuste d'abord la hauteur pour le 2ème billon
      Hauteur = case_when(
        billon == 1 & TypeEss == "Fs" ~ 3,
        billon == 1 & TypeEss == "Rx" ~ 6,
        billon == 2 & TypeEss == "Fs" ~ Hauteur - 3,
        billon == 2 & TypeEss == "Rx" ~ Hauteur - 6,
        billon == 3 ~ 0
      ),

      # diamètres médians
      diam_median = case_when(
        # diam médian du billon 1 : distinction feuillus/résineux
        billon == 1 & TypeEss == "Fs" ~ Diam, # diamètre médian à 1,3 m pour 1er billon de 3 m (1,3 m ~ 1,5 m)
        billon == 1 & TypeEss == "Rx" ~ Diam - (DecroissanceRx * 1.7), # diamètre médian à 3 m pour 1er billon de 6 m (1,3 + 1,7),
        # Pour avoir le diamètre médian du billon 2, on utilise le volume V2 et
        # la formule du cylindre (V2 = (diam_med / 100) ^ 2 (pi * Hauteur * Poids) / 4
        billon == 2 ~ sqrt( volume * 4 / (pi * Hauteur * Poids) ) * 100
        # le diamètre médian pour le billon 3 ou les connexes de scierie est sans objet
      ),

      duree_vie = case_when(
        # NB : tous les connexes sont de qualité D -> distinction billon/connexe pas nécessaire pour A, B et C
        qual == "A" ~ 1 + (130 / (1 + (4000 * exp( - 0.16 * diam_median)))), # durée de vie pour A
        qual == "B" ~ 1 + (120 / (1 + (2500 * exp( - 0.15 * diam_median)))), # # durée de vie pour B
        qual == "C" ~ 1 + (100 / (1 + (2500 * exp( - 0.14 * diam_median)))), # durée de vie pour C
        qual == "D" & produit == "billon" & tCha != 0 ~ DureeVieD, # durée de vie pour D - grume
        qual == "D" & produit == "connexe" & tCha != 0 ~ DureeVieConnexes, # durée de vie pour D - connexes
        tCha == 0 ~ 0
      )
    ) %>%
    select(IdArbre, Cycle, billon, produit, tCha, duree_vie)

  # Retour de la fonction calculs_Carbone
  return(df)
}
