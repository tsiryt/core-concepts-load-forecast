here::i_am("1-exercises/6-prep_analysis_feature_generation/main.R")

source(here::here("libraries.R"))
source(here::here("config.R"))

source(here::here("1-exercises/6-prep_analysis_feature_generation/config.R"))
source(here::here("0.1-get-data/config.R"))

# Take a demand time series. 
log_info("Parametres : ")
household_data <-
  read_delim(here::here(path_hourly_data_long))
  filter(
    household_type %in% filter_household[["household_type"]],
    id_household %in% filter_household[["id_household"]],
    energy_type %in% filter_household[["energy_type"]],
    id_ener_source %in% filter_household[["id_ener_source"]]
  )

## Plot the autocorr & partial autocorr
## At what lags is the autocorr strongest ?
## Plot a scatter plot of the demand against lags of the demand series.
## Are relationships linear ? If so, calculate adjusted coeffs of determination.
## Which lags give the biggest values ?
