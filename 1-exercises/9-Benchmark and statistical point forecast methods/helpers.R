sum_sinus <- function(t, df_sine_coefs) {

  sine_sum <-
    purrr::map2(
      df_sine_coefs$sine_coefs,
      df_sine_coefs$sine_order,
      function(coef, order) {
        logger::log_debug("Coef = {coef} | t = {t} | order = {order}")
        coef * sin(t * order)
      }
    ) %>%
    purrr::reduce(sum)

  return(sine_sum)
}
# pour pouvoir utiliser la somme de sinus dans un dplyr pipe
vec_sum_sinus <- Vectorize(sum_sinus, vectorize.args = c("t"))

sine_many <- function(df, col, sine_orders = 10, should_return_new_cols = FALSE) {

  df_with_sin <- df
  cols_sin <- c()

  for (sine_order in sine_orders) {
    new_col <- paste0(col, "_sin_", sine_order)
    cols_sin <- c(cols_sin, new_col)

    df_with_sin <- df_with_sin %>%
      mutate(!!sym(new_col) := sin(!!sym(col) * sine_order))
    log_info("Colonne {new_col} créée.")
  }

  if (should_return_new_cols) {
    return(list(df_many_sins = df_with_sin, new_col_names = cols_sin))
  }

  return(df_with_sin)
}

make_sinus_data <- function(df_sine_coefs, nb_sine_values, min_x_value, max_x_value) {
  x_values <- runif(n = nb_sine_values, min_x_value, max_x_value)
  df <-
    tibble(t = x_values) %>%
    mutate(
      y = vec_sum_sinus(t, df_sine_coefs)
    )
  return(df)
}
