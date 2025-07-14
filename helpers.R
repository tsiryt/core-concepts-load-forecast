#' Cree un df contenant les parametres des time series dont les id sont passes en parametres.
#' 
#' @param list_units int vector.
#' @param path_unit_ref string.
#' 
#' @return dataframe.
get_energy_unit_config <- function(list_units, path_unit_ref) {
  df_unit_ref <- read_delim(path_unit_ref, show_col_types = FALSE) %>%
    filter(id_unit %in% list_units) %>%
    mutate(id_unit = as.factor(id_unit)) %>%
    select(id_unit, !id_unit)

  msg_config <-
    map2(
      df_unit_ref,
      names(df_unit_ref),
      function(param, nom_param) {glue("{nom_param} : {param}")}
    ) %>%
    reduce(function(x, y) paste(x, y, sep = ", "))
  log_info("Parametres : {liste_config}", liste_config = msg_config)

  return(df_unit_ref)
}
