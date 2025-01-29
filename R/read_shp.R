
##### fonction choix du shape des placettes #####
read_shp <- function() {
  # -- choix des fichiers
  all_sf_path <- tk_choose.files( # sf_list = ListShp anciennement
    caption = "Choix du/des shape(s) des placettes",
    multi = T,
    filters = matrix(c("fichier shape", ".shp"), 1, 2, byrow = T)
  )
  # all_sf_path <- file.path(repPSDRF, "data/PSDRF_extract/SIG/Vecteurs/Placettes/Plac_Chalmessin_L93.shp") # debug

  # -- lecture des shapes
  all_sf <- c()
  # barre de progression
  pb <- tkProgressBar(
    title = "Progression",
    label = "Lecture des shapes de placettes en cours... (%)",
    min = 0, max = 100, width = 500
  )
  for (sf_path in all_sf_path) {
    # chemin d'accès du fichier
    # sf_path <- all_sf_path[1] # debug
    print(
      paste0(
        "Lecture du shape : ", basename(file_path_sans_ext(sf_path))
      )
    )
    sf <- st_read(
      sf_path,
      stringsAsFactors = FALSE,
      quiet = T
    ) %>%
      st_transform(crs = 2154) # reprojette en L93
    # TODO : ajouter sécurité sur le système de projection ? -> st_crs

    # vérification des colonnes
    sf <- check_sf(sf)

    # rassemble les données
    all_sf <- rbind(all_sf, sf)
    info <- round(match(sf_path, all_sf_path) / length(all_sf_path) * 100)
    setTkProgressBar(
      pb, value = info,
      title = paste0("Lecture (", info, " %)"),
      label = paste0("Lecture des shapes de placettes en cours : ", info, "% done")
    )
  } # end of all_sf_path loop
  close(pb)

  # retour de la fonction read_shp
  return(all_sf)
}
