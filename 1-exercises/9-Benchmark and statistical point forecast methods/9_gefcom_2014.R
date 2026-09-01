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
  select(datetime, load, starts_with("w"))

plot_correlation(
  df_covars,
  c("load"),
  "datetime",
  lag_max = 24 * 14
)
# lags avec un coef de correlation > 168
lags_current_day <- c(24)
lags_weekly <- c(144, 168)
lags_to_create <- c(lags_current_day, lags_weekly)

metrics <- c(yardstick::rmse, yardstick::mape)
df_covars %>%
  lag_many("load", lags_to_create) %>%
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