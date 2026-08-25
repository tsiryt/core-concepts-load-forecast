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
#' @param should_return_lag_names bool.
#'
#' @return dataframe ou list
lag_many <- function(df, col, lags, should_return_lag_names = FALSE) {

  df_lagged <- df
  lag_names <- c()

  for (lag_length in lags) {
    new_col <- paste0(col, "_lag_", lag_length)
    lag_names <- c(lag_names, new_col)

    df_lagged <- df_lagged %>%
      mutate(!!sym(new_col) := lag(!!sym(col), lag_length))
    log_debug("Colonne {new_col} créée.")
  }

  if (should_return_lag_names) {
    return(list(df_lagged = df_lagged, lag_names = lag_names))
  }

  return(df_lagged)
}

#' Calcule la moyenne glissante saisonnale.
#'
#' @param df
#' @param col string.
#' @param lag int. Saisonnalité (nb d'instants). Pour un df horaire, 168 = 24 * 7 correspond à l'ordre par semaine
#' @param order int. Le nombre d'instants à inclure dans la moyenne
#'
#' @examples
#' simple_moving_average(df_hourly, "value", 24 * 7, 4) # l'instant de lundi 8h est calculé en utilisant les 4 derniers lundi 8h
#'
#' @return df
simple_moving_average <- function(df, col, lag, order) {
  lags <- rep(lag, times = order) * 1:order
  original_cols <- colnames(df)

  result_lagging <- lag_many(df, col, lags, should_return_lag_names = TRUE)
  df_lagged <- result_lagging$df_lagged
  lag_names <- result_lagging$lag_names

  new_col <- glue("{col}_sma_{lag}_order_{order}")
  df_sma <- df_lagged %>%
    rowwise() %>%
    mutate(!!sym(new_col) := mean(c_across(all_of(lag_names)))) %>%
    ungroup() %>%
    select(all_of(original_cols), !!sym(new_col))
  log_info("Colonne {new_col} créée.")

  return(df_sma)
}

#' Rajoute des colonnes correspondant a la moyenne glissante, par instant et par saisonnalité.
#'
#' @info Taken from https://stackoverflow.com/questions/55940655/how-to-mutate-for-loop-in-dplyr
#'
#' @param df dataframe.
#' @param col string.
#' @param lag int. Saisonnalité (nb d'instants). Pour un df horaire, 168 = 24 * 7 correspond à l'ordre par semaine
#' @param sma_orders int vector. Différentes valeurs du nombre d'instants à intégrer dans la moyenne.
#'
#' @return dataframe.
sma_many <- function(df, col, lags, sma_orders) {
  df_sma <- df
  for (lag in lags) {
    for (sma_order in sma_orders) {
      df_sma <- simple_moving_average(df_sma, col, lag, sma_order)
    }
  }

  return(df_sma)
}

#' Calcule la performance d'un modèle en fonction d'un métrique.
#'
#' @param metric fonction du package yardstick.
#' @param test_pred_class df.
#' @param truth colonne de test_pred_class
#' @param estimate colonne de test_pred_class
#'
#' @return df.
measure_baseline <- function(metric, test_pred_class, truth, estimate){
  info_baseline <- test_pred_class %>%
    metric(truth = {{truth}}, estimate = {{estimate}})

  return(info_baseline)
}
