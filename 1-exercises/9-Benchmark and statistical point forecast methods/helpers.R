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
