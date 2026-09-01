chosen_units <- 59
cols_grouping <- c("household_type", "id_household", "energy_type", "id_ener_source", "id_unit")
date_fin <- ymd_hms("2018-05-01 00:00:00")
lags_to_create <- c(
  1, # persistence
  24, # daily
  24 * 7 # weekly
)
file_gefcom <- "C:/Users/rasen/Documents/r-projects/data/GEFCom2014/GEFCom2014 Data/GEFCom2014-L_V2/Load/Task 1/L1-train.csv"

### GEFCOM ###
# on se limite a 3 ans de données
date_deb_analyse <- lubridate::ymd_hms("2007-01-01 01:00:00")
date_fin_analyse <- date_deb_analyse + dyears(3)

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
seed <- 42
seed_test <- 13
min_coef_sin <- -2
max_coef_sin <- 2
min_x_value <- 0
max_x_value <- 4 * pi
nb_sine_terms_fit <- 50
lambdas <- seq(1e-4, 2, length.out = 100)
