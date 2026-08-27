current_folder <- "9-Benchmark and statistical point forecast methods"
here::i_am(glue::glue("1-exercises/{current_folder}/main.R"))

source(here::here("libraries.R"))
source(here::here("config.R"))
source(here::here("helpers.R"))
source(here::here("time_series_analysis.R"))

source(here::here(glue("1-exercises/{current_folder}/config.R")))
source(here::here(glue("1-exercises/{current_folder}/helpers.R")))
source(here::here("0.1-get-data/config.R"))

# Take a demand time series.
ref_units <- get_energy_unit_config(chosen_units, here::here(path_ref_units))

household_data_long <-
  read_delim(here::here(path_hourly_data_long), show_col_types = FALSE) %>%
  right_join(
    ref_units,
    by = c("household_type", "id_household", "energy_type", "id_ener_source")
  ) %>%
  filter(!is.na(ener_kWh), utc_timestamp <= date_fin)
splits <- rsample::initial_time_split(household_data_long, c(0.8))
df_train <- rsample::training(splits)
df_test <- rsample::testing(splits)

splits_2 <- rsample::initial_validation_time_split(household_data_long, c(0.6, 0.2))
df_train_2 <- rsample::training(splits_2)
df_val_2 <- rsample::validation(splits_2)
df_test_2 <- rsample::testing(splits_2)


# 1. Select a demand time series. Analyse the seasonalities. Generate some
# simple becnhmark forecasts for the test set, including the persistence
# forecast and seasonal persistence forecasts, one for each seasonality
# you found. Calculate the RMSE errors. Which one is lower ? How does this
# compare to the seasonalities you found ? Compare thse results to the ACCF
# and PACF for the time series.
household_data_with_lags <- household_data_long %>%
  group_by(across(all_of(cols_grouping))) %>%
  lag_many("ener_kWh", lags_to_create) %>%
  select(utc_timestamp, id_unit, all_of(cols_grouping), starts_with("ener_kWh"))
household_data_with_lags %>%
  pivot_longer(names_to = "model", values_to = "forecast", cols = starts_with("ener_kWh_lag")) %>%
  group_by(across(
    c(all_of(cols_grouping), "model")
  )) %>%
  measure_baseline(
    yardstick::rmse,
    test_pred_class = .,
    truth = ener_kWh,
    estimate = forecast
  ) %>%
  print(width = Inf)
# A tibble: 3 x 8
#   household_type id_household energy_type id_ener_source model            .metric .estimator .estimate
#   <chr>                 <dbl> <chr>                <dbl> <chr>            <chr>   <chr>          <dbl>
# 1 residential               5 grid_import             NA ener_kWh_lag_1   rmse    standard       0.239          
# 2 residential               5 grid_import             NA ener_kWh_lag_168 rmse    standard       0.290          
# 3 residential               5 grid_import             NA ener_kWh_lag_24  rmse    standard       0.283  

# 2. Continuing the experiment from the previous section, generate seasonal
# moving averages using the identified seasonalities. Using a validation set
# identify the optimal value of seasonal terms, p, to include in the average.
# If there is multiple seasonalities, which one has the smallest errors
# overall ? How does the RMSE error on a tests set for the optimal average
#  forecasts compare to the persistence forecasts in the previous section ?

log_info("Choix d'hyperparamètres pour simple moving average")
df_train_sma <- sma_many(df_train, "ener_kWh", lags_sma_train, sma_orders_train)
df_train_sma %>%
  pivot_longer(names_to = "model", values_to = "forecast", cols = starts_with("ener_kWh_sma")) %>%
  group_by(across(
    c(all_of(cols_grouping), "model")
  )) %>%
  measure_baseline(
    yardstick::rmse,
    test_pred_class = .,
    truth = ener_kWh,
    estimate = forecast
  ) %>%
  print(width = Inf)

# Meilleurs params
df_sma_daily_seasonality <- simple_moving_average(household_data_long, "ener_kWh", 24, 6)
df_sma_weekly_seasonality <- simple_moving_average(household_data_long, "ener_kWh", 24 * 7, 6)

# rmse = 0.224
df_sma_daily_seasonality %>%
  pivot_longer(names_to = "model", values_to = "forecast", cols = starts_with("ener_kWh_sma")) %>%
  group_by(across(
    c(all_of(cols_grouping), "model")
  )) %>%
  measure_baseline(
    yardstick::rmse,
    test_pred_class = .,
    truth = ener_kWh,
    estimate = forecast
  ) %>%
  print(width = Inf)
# rmse = 0.230
df_sma_weekly_seasonality %>%
  pivot_longer(names_to = "model", values_to = "forecast", cols = starts_with("ener_kWh_sma")) %>%
  group_by(across(
    c(all_of(cols_grouping), "model")
  )) %>%
  measure_baseline(
    yardstick::rmse,
    test_pred_class = .,
    truth = ener_kWh,
    estimate = forecast
  ) %>%
  print(width = Inf)

# 3. Generate a simple 1-step ahead exponential smoothing forecasts for a load
# forecast time series (prefarably one which has double seasonal patterns,
# usually daily and weekly). Manual select different values of the smoothing
# parameter, alpha. Plot the RMSE against the smoothing parameters. Do a grid
# search to find the optimal smoothing parameter. How does the optimal forecast
# compare to a simple persistence forecast ? Now consider
# the Holt-Winters-Taylor forecast and perform a grid search for the four
# parameters phi, lambda, delta, omega.
log_info("Simple exponential smoothing.")
df_alpha_rmse <- purrr::map(
  alphas,
  function(alpha) {
    fit <- ses(df_train_2, df_val_2, "ener_kWh", alpha, should_return_col_name = TRUE)
    df_prev <- fit$df_prev
    col_prev <- fit$col_prev

    estimate <-
      yardstick::rmse(
        df_prev,
        ener_kWh,
        !!col_prev
      ) %>%
      magrittr::use_series(.estimate)
    tibble(alpha = alpha, metric = estimate)
  }
) %>%
list_rbind()
df_alpha_rmse %>%
  ggplot() +
  geom_point(aes(x = alpha, y = metric)) +
  labs(title = "Simple exponential smoothing.", y = "RMSE")

# 4. Investigate a LASSO fit for a linear model. Set the coefficients of a
# model with a few sine terms, for about N=5 elements, and x in [0, 4pi].
# Sample 20 points from this data (and add a small amount of Gaussian noise)
# Now fit a multiple linear equation using least squares regression. Now plot
# the trained model on 20 new points. Is it a good fit ? Now try and minimize
# the LASSO function using different values of the regularisation parameter.
# How does the fit change as you change the parameter ? How many coefficients
# are zero (or negligible) ? Use glmnet (R) or sklearn (python)
withr::with_seed(
  seed = seed,
  {
    sine_coefs <- runif(n = nb_sine_terms, min_coef_sin, max_coef_sin)
    df_sine_coefs <- tibble(sine_coefs = sine_coefs, sine_order = 1:nb_sine_terms)
    df_real <- make_sinus_data(df_sine_coefs, nb_sine_values, min_x_value, max_x_value)
  }
)

log_info("Fit glm model a la somme de sinus")
df_test <- sine_many(df_real, "t", 1:nb_sine_terms_fit)
lm_form <- tidyformula::tidyformula(y ~ starts_with("t_sin"), data = df_test)
glm_spec_init <- parsnip::linear_reg(penalty = 1) %>%
  parsnip::set_engine("glmnet")
glm_fit_init <- glm_spec_init %>%
  parsnip::fit(lm_form, data = df_test)
withr::with_seed(
  seed = seed_test,
  {
    df_test <- make_sinus_data(df_sine_coefs, nb_sine_values, min_x_value, max_x_value) %>%
      sine_many("t", 1:nb_sine_terms_fit)

  }
)
log_info("Predict somme de sinus")
df_prev <- augment(glm_fit_init, df_test) %>%
  select(t, y, .pred) %>%
  rename(real = y, prev = .pred) %>%
  pivot_longer(names_to = "type", values_to = "valeur", cols = c("real", "prev"))
df_prev %>%
  ggplot() +
  geom_point(aes(x = t, y = valeur, color = type)) +
  labs(title = "Regression lineaire sur la somme de sinus.", subtitle = glue("Nb de termes initiaux : {nb_sine_terms}. Nb de termes du modèle : {nb_sine_terms_fit}"))

# 6. Try and generate a linear model that fits a demand profile.

