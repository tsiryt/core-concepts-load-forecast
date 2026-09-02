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

  df_with_lags <- df
  cols_lags <- c()

  for (lag_length in lags) {
    new_col <- paste0(col, "_lag_", lag_length)
    cols_lags <- c(cols_lags, new_col)

    df_with_lags <- df_with_lags %>%
      mutate(!!sym(new_col) := lag(!!sym(col), lag_length))
    log_info("Colonne {new_col} créée.")
  }

  if (should_return_lag_names) {
    return(list(df_lagged = df_with_lags, lag_names = cols_lags))
  }

  return(df_with_lags)
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

ses <- function(df_train, df_test, target, alpha, freq = 24, should_return_col_name = FALSE) {

  fit <- forecast::ets(ts(df_train[[target]], frequency = freq), alpha = alpha, model = "ANZ", opt.crit = "mse")

  new_col <- glue("{target}_ses_{alpha}")
  prev <- forecast::forecast(fit, h = nrow(df_test)) %>%
    magrittr::use_series(mean)
  df_test[[new_col]] <- prev
  log_info("Simple exponential smoothing : colonne {new_col} créée.")

  if (should_return_col_name) {
    return(list(df_prev = df_test, col_prev = new_col))
  }

  return(df_test)
}

#' Calcule la performance d'un modèle en fonction d'un métrique.
#'
#' @param metrics fonction ou liste de fonctions du package yardstick.
#' @param test_pred_class df.
#' @param truth colonne de test_pred_class
#' @param estimate colonne de test_pred_class
#'
#' @return df.
measure_baseline <- function(metrics, test_pred_class, truth, estimate){

  if (length(metrics) == 1) {
    metrics <- c(metrics)
  }
  info_baseline <- metrics %>%
    purrr::map(function(metric) metric(test_pred_class, truth = {{truth}}, estimate = {{estimate}})) %>%
    purrr::list_rbind()

  return(info_baseline)
}

make_calendar <- function(df, date_col, time_origin = NULL) {

  if (is.null(time_origin)) {
    time_origin <- df %>%
      pull({{date_col}}) %>%
      min()
    log_warn("La date d'origine n'est pas définie. On utilise {time_origin}, la date minimale du df.")
  }
  df_with_calendar <- df %>%
    mutate(
      is_weekend = if_else(wday({{date_col}}, week_start = 1) %in% c(6, 7), 1, 0),
      toy = yday({{date_col}}) / 365,
      heure = hour({{date_col}}),
      jour = wday({{date_col}}, week_start = 1),
      mois = month({{date_col}}),
      duration_since_origin = as.numeric({{date_col}} - time_origin) / 3600
    )

  date_col_name <- rlang::englue("{{date_col}}")
  log_info("Variables calendaires créées à partir de {date_col_name}")

  return(df_with_calendar)
}

compute_temp_seuil <- function(df, temp_col, temp_seuil) {
  temp_col_name <- rlang::englue("{{temp_col}}")
  col_seuil_inf <- glue("{temp_col_name}_seuil_inf")
  col_seuil_sup <- glue("{temp_col_name}_seuil_sup")
  df_with_ts <- df %>%
    mutate(
      !!sym(col_seuil_inf) := if_else({{temp_col}} <= temp_seuil, {{temp_col}}, temp_seuil),
      !!sym(col_seuil_sup) := if_else({{temp_col}} >= temp_seuil, {{temp_col}}, temp_seuil)
    )

  log_info("Calcul des températures seuillées {col_seuil_inf} / {col_seuil_sup} avec température seuil : {temp_seuil} à partir de la colonne {temp_col_name}")
  return(df_with_ts)
}

compute_weather_features <- function(df, temp_col, temp_seuil) {
  df_with_weather <- df %>%
    compute_temp_seuil({{temp_col}}, temp_seuil)

  return(df_with_weather)
}
