chosen_units <- 1
cols_grouping <- c("household_type", "id_household", "energy_type", "id_ener_source", "id_unit")
lags_to_create <- c(
  1, # persistence
  24, # daily
  24 * 7 # weekly
)

sma_orders_train <- 2:6
lags_sma_train <- c(24, 24 * 7)

### TRAIN/VALIDATION/TESTING ###
prop_train_val <- c(0.6, 0.2)
