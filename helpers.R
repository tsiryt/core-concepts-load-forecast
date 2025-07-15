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

#' Rajoute des colonnes correspondant a des lags d'une des colonnes d'un df.
#'
#' @info Taken from https://stackoverflow.com/questions/55940655/how-to-mutate-for-loop-in-dplyr
#'
#' @param df dataframe.
#' @param col string.
#' @param lags int vector. Les lags a calculer.
#' 
#' @return dataframe.
lag_many <- function(df, col, lags) {

  df_lagged <- df

  for (lag_length in lags) {
    new_col <- paste0(col, "_lag_", lag_length)

    df_lagged <- df_lagged %>%
      mutate(!!sym(new_col) := lag(!!sym(col), lag_length))
    log_debug("Colonne {new_col} créée.")
  }

  return(df_lagged)
}