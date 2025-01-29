##### fonction de nettoyage des noms #####
clean_names <- function(string) {
  string <- gsub("/", " sur ", string)
  string <- gsub("\\.", "_", string, fixed)
  string <- gsub(" ", "_", string, fixed = T)
  string <- gsub("'", "", string, fixed = T)
  string <- gsub("\u00EA", "e", string, fixed = T)
  string <- gsub("\u00E2", "a", string, fixed = T)
  string <- gsub("\u00E9", "e", string, fixed = T)
  string <- gsub("\u00E8", "e", string, fixed = T)
  string <- gsub("\u00FB", "u", string, fixed = T)
  string <- gsub("\u00EE", "i", string, fixed = T)
  string <- gsub("\u00F4", "o", string, fixed = T)
  string <- gsub("\u00E7", "c", string, fixed = T)

  # majuscules
  string <- gsub("\u00CA", "E", string, fixed = T)
  string <- gsub("\u00C2", "A", string, fixed = T)
  string <- gsub("\u00C9", "E", string, fixed = T)
  string <- gsub("\u00C8", "E", string, fixed = T)
  string <- gsub("\u00DB", "U", string, fixed = T)
  string <- gsub("\u00CE", "I", string, fixed = T)
  string <- gsub("\u00D4", "O", string, fixed = T)
  string <- gsub("\u00C7", "C", string, fixed = T)

  # retour de la fonction clean_names
  return(string)
}

##### fonction de nettoyage compilation pdf #####
clean_after_knit <- function(output) {
  file.remove(output)
  if (exists(gsub(".tex", ".aux", output))) file.remove(gsub(".tex", ".aux", output))
  if (exists(gsub(".tex", ".log", output))) file.remove(gsub(".tex", ".log", output))
  if (exists(gsub(".tex", ".out", output))) file.remove(gsub(".tex", ".out", output))
}

##### fonction pour créer des objets nuls (package) #####
create_null <- function(objects_list) {
  for (obj in objects_list) assign(obj, NULL, envir = parent.frame())
}

##### fonction pour créer des data.frame vides (package) #####
create_empty_df <- function(objects_list) {
  for (obj in objects_list) assign(obj, data.frame(), envir = parent.frame())
}

