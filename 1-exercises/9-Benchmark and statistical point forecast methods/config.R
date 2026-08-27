chosen_units <- 59
cols_grouping <- c("household_type", "id_household", "energy_type", "id_ener_source", "id_unit")
date_fin <- ymd_hms("2018-05-01 00:00:00")
lags_to_create <- c(
  1, # persistence
  24, # daily
  24 * 7 # weekly
)

### TRAIN/VALIDATION/TESTING ###
prop_train_val <- c(0.6, 0.2)

# SIMPLE MOVING AVERAGE
sma_orders_train <- 2:6
lags_sma_train <- c(24, 24 * 7)

## EXPONENTIAL SMOOTHING
# Simple exponential smoothing
alphas <- seq(0.25, 0.975, length.out = 20)

## LASSO
nb_sine_terms <- 5
nb_sine_values <- 20
x_values <- seq(0, 4 * pi, length.out = nb_sine_values)
seed <- 42
min_coef_sin <- -2
max_coef_sin <- 2
