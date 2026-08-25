chosen_units <- 1
cols_grouping <- c("household_type", "id_household", "energy_type", "id_ener_source", "id_unit")
lags_to_create <- c(
  1, # persistence
  24, # daily
  24 * 7 # weekly
)