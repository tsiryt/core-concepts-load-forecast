here::i_am("1-exercises/6-prep_analysis_feature_generation/main.R")

source(here::here("libraries.R"))
source(here::here("config.R"))
source(here::here("helpers.R"))

source(here::here("1-exercises/6-prep_analysis_feature_generation/config.R"))
source(here::here("0.1-get-data/config.R"))

# Take a demand time series. 
ref_units <- get_energy_unit_config(chosen_units, here::here(path_ref_units))

household_data <-
  read_delim(here::here(path_hourly_data_long), show_col_types = FALSE) %>%
  right_join(
    ref_units,
    by = c("household_type", "id_household", "energy_type", "id_ener_source")
  ) %>%
  filter(!is.na(ener_kWh))

## Simple plot
household_data %>%
  ggplot(aes(x = utc_timestamp)) +
  geom_line(aes(y = ener_kWh, color = id_unit, linetype = energy_type), linewidth = 1.6) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(7),
    labels = scales::label_number()
  ) +
  theme_light()

## Plot the autocorr & partial autocorr
household_data %>%
  select(ener_kWh) %>%
  acf(type = "correlation", 2400)

household_data %>%
  select(ener_kWh) %>%
  acf(type = "partial", 2400)
## At what lags is the autocorr strongest ?
# Data is highly trended

## Plot a scatter plot of the demand against lags of the demand series.
## Are relationships linear ? If so, calculate adjusted coeffs of determination.
## Which lags give the biggest values ?
