current_folder <- "9-Benchmark and statistical point forecast methods"
here::i_am(glue::glue("1-exercises/{current_folder}/9_gefcom_2014.R"))

source(here::here("libraries.R"))
source(here::here("config.R"))
source(here::here("helpers.R"))
source(here::here("time_series_analysis.R"))

source(here::here(glue("1-exercises/{current_folder}/config.R")))
source(here::here(glue("1-exercises/{current_folder}/helpers.R")))

df_gefcom <- readr::read_delim(file_gefcom)
date_deb_raw <- lubridate::ymd_hms("2001-01-01 01:00:00")
date_fin_raw <- date_deb_raw + (nrow(df_gefcom) - 1) * lubridate::dhours(1)
timeline <- seq(date_deb_raw, date_fin_raw, by = "1 hour")
df_gefcom$datetime <- timeline
df_covars <- df_gefcom %>%
  filter(datetime >= date_deb_analyse, datetime <= date_fin_analyse) %>%
  rename_with(str_to_lower) %>%
  select(datetime, load, starts_with("w")) %>%
  make_calendar(datetime) %>%
  lag_many("load", lags_to_create_gefcom)

plot_correlation(
  df_covars,
  c("load"),
  "datetime",
  lag_max = 24 * 14
)

metrics <- c(yardstick::rmse, yardstick::mape)
df_covars %>%
  lag_many("load", lags_to_create_gefcom) %>%
  select(datetime, starts_with("load")) %>%
  pivot_longer(names_to = "model", values_to = "forecast", cols = starts_with("load_")) %>%
  group_by(model) %>%
  measure_baseline(
    metrics,
    test_pred_class = .,
    truth = load,
    estimate = forecast
  ) %>%
  print(width = Inf)

splits_tt <- rsample::initial_time_split(df_covars, c(0.8))
df_train_tt <- rsample::training(splits_tt)
df_test_tt <- rsample::testing(splits_tt)

splits_tvt <- rsample::initial_validation_time_split(df_covars, c(0.6, 0.2))
df_train_tvt <- rsample::training(splits_tvt)
df_val_tvt <- rsample::validation(splits_tvt)
df_test_tvt <- rsample::testing(splits_tvt)
