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
  lag_many("load", lags_to_create_gefcom) %>%
  filter(datetime >= date_deb_analyse + dhours(max(lags_to_create_gefcom))) # on ne veut pas de valeurs NA


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
splits_lm <- rsample::make_splits(
  x = df_train_tvt,
  assessment = df_val_tvt
)

### Modèle linéaire ###
lm_form <- tidyformula::tidyformula(load ~ starts_with("load_lag") + starts_with("w") + is_weekend + toy + heure + jour + mois + duration_since_origin, data = df_train_tt)
glm_spec <- linear_reg(penalty = 0.98) %>%
  set_engine("glmnet")
glm_fit_init <- glm_spec %>%
  fit(lm_form, data = df_train_tt)
df_prev <-
  augment(glm_fit_init, df_test_tt) %>%
  select(datetime, load, .pred) %>%
  rename(real = load, prev = .pred)
glm_fit_init %>%
  tidy() %>%
  print(n = Inf)
df_prev %>%
  measure_baseline(metrics, ., real, prev)

my_plot <- df_prev %>%
  pivot_longer(names_to = "type", values_to = "valeur", cols = c("real", "prev")) %>%
  ggplot() +
  geom_point(aes(x = datetime, y = valeur, color = type)) +
  labs(title = "Regression lineaire GEFCOM")
plotly::ggplotly(my_plot)

glm_spec_tune <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")
grid_values <- tibble(penalty = lambdas)
metrics <- yardstick::metric_set(yardstick::rmse, yardstick::mape)
ctrl <- tune::control_grid(verbose = FALSE, save_pred = TRUE)
grid_search <- tune::tune_grid(
  glm_spec_tune,
  preprocessor = lm_form,
  resamples = rsample::manual_rset(list(splits_lm), c("split 0")),
  grid = grid_values,
  metrics = metrics,
  control = ctrl
)
best_glm_params <- tune::select_best(grid_search, metric = "rmse")
best_glm_model <- tune::finalize_model(glm_spec_tune, best_glm_params)
best_glm_model_fit <- fit(best_glm_model, lm_form, data = df_train_tt)
df_prev_final <- augment(best_glm_model_fit, df_test_tt) %>%
  select(datetime, load, .pred) %>%
  rename(real = load, prev = .pred)
df_prev_final %>%
  pivot_longer(names_to = "type", values_to = "valeur", cols = c("real", "prev")) %>%
  ggplot() +
  geom_point(aes(x = datetime, y = valeur, color = type)) +
  labs(title = "Regression lineaire")
best_glm_model_fit %>%
  tidy() %>%
  print(n = Inf)
df_prev_final %>%
  measure_baseline(metrics, ., real, prev)
