##### fonction de conversion des Coords au format shape #####
Coords2SHP <- function(Coords = NULL) {

  if (dim(Coords)[1] > 0) {

    # Fonction conversion df en sf
    convert2SHP <- function(shape) {
      shape %>%
        st_as_sf(
          coords = c("Xgps", "Ygps"),
          crs = unique(shape$EPSG),
          remove = F) %>%
        st_transform(crs = 2154)# %>%
      # mutate(
      #   Xgps=st_coordinates()[,1],
      #   Ygps=st_coordinates()[,2]
      # ) %>%
      # select(NumForet, NumPlac, EPSG, Observations, Xgps, Ygps, geometry)
    }

    # -- Tri entre les EPSG
    pos_L93 <- which(Coords$EPSG==2154)
    pos_WGS84 <- which(Coords$EPSG==4326)
    pos_UTM32 <- which(Coords$EPSG==32632 | Coords$EPSG=="32T")
    pos_UTM31 <- which(Coords$EPSG==32631 | Coords$EPSG=="31T")

    # Sécurité sur les EPSG reconnus
    pos_Autres <- which(
      !is.element(Coords$EPSG, c(2154, 4326, 32632, "32T", 32631, "31T")) &
        !is.na(Coords$EPSG)
    )
    if (length(pos_Autres) > 0) {
      Msg2 <- stop(
        message = "Attention, des syst\u00E8mes de projection (EPSG) ne sont pas reconnus dans la feuille 'Coords' du classeur d'inventaire.
        \nRappel - EPSG reconnus :\nLambert 93 = '2154'\nMercator = '4326'\nUTM32 = '32632' ou '32T'\nUTM31 = '32631' ou '31T'"
      )
    }
    # Sécurité sur les EPSG vides
    pos_Vides <- which(is.na(Coords$EPSG))
    if (length(pos_Vides) > 0) {
      Msg2 <- stop(
        message = "Attention, il y a des syst\u00E8mes de projection (EPSG) non renseignés (vides) dans la feuille 'Coords' du classeur d'inventaire.
        \nRappel - EPSG reconnus :\nLambert 93 = '2154'\nMercator = '4326'\nUTM32 = '32632' ou '32T'\nUTM31 = '32631' ou '31T'"
      )
    }
    # -- Création des shapes selon le/les EPSG détecté(s)
    # EPSG = Lambert 93
    if (length(pos_L93) > 0) {
      Plac_L93 <-
        filter(Coords, EPSG==2154 & !is.na(Xgps) & !is.na(Ygps)) %>%
        mutate(
          Xgps = as.numeric(Xgps),
          Ygps = as.numeric(Ygps)
        ) %>%
        convert2SHP()
      # Récupération du shape créé :
      Placettes_SHP <- Plac_L93
    }
    # EPSG = Mercator
    if (length(pos_WGS84) > 0) {
      Plac_WGS84 <-
        filter(Coords, EPSG==4326 & !is.na(Xgps) & !is.na(Ygps)) %>%
        mutate(
          Xgps = as.numeric(Xgps),
          Ygps = as.numeric(Ygps)
        ) %>%
        convert2SHP()
      # Récupération du shape créé :
      Placettes_SHP <-
        if (exists("Placettes_SHP") && dim(Placettes_SHP)[1] > 0) {
          rbind(Placettes_SHP, Plac_WGS84)
        } else Placettes_SHP <- Plac_WGS84
    }
    # EPSG = UTM 32
    if (length(pos_UTM32) > 0) {
      Plac_UTM32 <-
        filter(Coords, (EPSG == 32632 | EPSG == "32T") & !is.na(Xgps) & !is.na(Ygps)) %>%
        mutate(
          EPSG = 32632,
          Xgps = as.numeric(Xgps),
          Ygps = as.numeric(Ygps)
        ) %>%
        convert2SHP()
      # Récupération du shape créé :
      Placettes_SHP <-
        if (exists("Placettes_SHP") && dim(Placettes_SHP)[1] > 0) {
          rbind(Placettes_SHP, Plac_UTM32)
        } else Placettes_SHP <- Plac_UTM32
    }
    # EPSG = UTM 31
    if (length(pos_UTM31) > 0) {
      Plac_UTM31 <-
        filter(Coords, (EPSG == 32631 | EPSG == "31T") & !is.na(Xgps) & !is.na(Ygps)) %>%
        mutate(
          EPSG = 32631,
          Xgps = as.numeric(Xgps),
          Ygps = as.numeric(Ygps)
        ) %>%
        convert2SHP()
      # Récupération du shape créé :
      Placettes_SHP <-
        if (exists("Placettes_SHP") && dim(Placettes_SHP)[1] > 0) {
          rbind(Placettes_SHP, Plac_UTM31)
        } else Placettes_SHP <- Plac_UTM31
    }

    # -- Rassemblement des données
    Placettes_SHP <-
      Placettes_SHP %>%
      mutate(
        NumForet=as.numeric(NumForet),
        Xgps=st_coordinates(Placettes_SHP)[,1],
        Ygps=st_coordinates(Placettes_SHP)[,2]
      )
  } else {
    Placettes_SHP <- Coords # Cas où Coords vide
  }

  # retour de la fonction Coords2SHP
  return(Placettes_SHP)
}
