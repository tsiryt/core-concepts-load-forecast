current_folder <- "9-Benchmark and statistical point forecast methods"
here::i_am(glue::glue("1-exercises/{current_folder}/main.R"))

source(here::here("libraries.R"))
source(here::here("config.R"))
source(here::here("helpers.R"))
source(here::here("time_series_analysis.R"))

source(here::here(glue("1-exercises/{current_folder}/config.R")))
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
# A tibble: 3 x 5
#   household_type energy_type model            .metric .estimate
#   <chr>          <chr>       <chr>            <chr>       <dbl>
# 1 industrial     grid_import ener_kWh_lag_1   rmse         6.70
# 2 industrial     grid_import ener_kWh_lag_168 rmse        11.4
# 3 industrial     grid_import ener_kWh_lag_24  rmse        12.9

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
df_sma_weekly_seasonality <- simple_moving_average(household_data_long, "ener_kWh", 24 * 7, 2)

# rmse = 11.7
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
# rmse = 11.6
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


# 4. Investigate a LASSO fit for a linear model. Set the coefficients of a
# model with a few sine terms, for about N=5 elements, and x in [0, 4pi].
# Sample 20 points from this data (and add a small amount of Gaussian noise)
# Now fit a multiple linear equation using least squares regression. Now plot
# the trained model on 20 new points. Is it a good fit ? Now try and minimize
# the LASSO function using different values of the regularisation parameter.
# How does the fit change as you change the parameter ? How many coefficients
# are zero (or negligible) ? Use glmnet (R) or sklearn (python)

# 6. Try and generate a linear model that fits a demand profile.

