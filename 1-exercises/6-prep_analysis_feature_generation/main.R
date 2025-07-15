here::i_am("1-exercises/6-prep_analysis_feature_generation/main.R")

source(here::here("libraries.R"))
source(here::here("config.R"))
source(here::here("helpers.R"))
source(here::here("time_series_analysis.R"))

source(here::here("1-exercises/6-prep_analysis_feature_generation/config.R"))
source(here::here("0.1-get-data/config.R"))

# Take a demand time series. 
ref_units <- get_energy_unit_config(chosen_units, here::here(path_ref_units))

household_data_long <-
  read_delim(here::here(path_hourly_data_long), show_col_types = FALSE) %>%
  right_join(
    ref_units,
    by = c("household_type", "id_household", "energy_type", "id_ener_source")
  ) %>%
  filter(!is.na(ener_kWh))

log_debug("To use acf and pacf func with multiple time series, wide format is better.")
log_debug("The selected columns were chosen after deciding the value of chosen_units")
household_data <-
  read_delim(here::here(path_hourly_data), show_col_types = FALSE) %>%
  select(utc_timestamp, DE_KN_industrial1_grid_import, DE_KN_industrial1_pv_1, DE_KN_industrial1_pv_2) %>%
  mutate(across(where(is.numeric), ~ . - lag(., 1))) %>% # l'energie dans la donnee brute est cumulee
  filter(if_all(where(is.numeric), ~ !is.na(.)))


## Simple plot
household_data_long %>%
  ggplot(aes(x = utc_timestamp)) +
  geom_line(aes(y = ener_kWh, color = id_unit, linetype = energy_type), linewidth = 1.6) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(7),
    labels = scales::label_number()
  ) +
  scale_x_datetime(date_breaks = "3 month") +
  theme_light()

## Plot the autocorr & partial autocorr
plot_correlation_fun(
  household_data,
  c("DE_KN_industrial1_grid_import", "DE_KN_industrial1_pv_1", "DE_KN_industrial1_pv_2"),
  "utc_timestamp",
  lag_max = 24 * 14,
  type = "acf"
)

plot_correlation_fun(
  household_data,
  c("DE_KN_industrial1_grid_import", "DE_KN_industrial1_pv_1", "DE_KN_industrial1_pv_2"),
  "utc_timestamp",
  lag_max = 24 * 14,
  type = "pacf"
)


## At what lags is the autocorr strongest ?
log_trace("ACF / Grid import : max lag a J-0 etau J-7 + decroissance. Tres correle a H+24")
log_trace("ACF / Grid import : max lag a H-24, min a H-12. Ce pattern cycle")
log_trace("ACF / Grid import : max lag a H-24, min a H-12. Ce pattern cycle")

log_trace("PACF / Grid import : Lag positif fort jusqu'a H-3. Negatif de H+4 a H-6. Positif jusqu'a H-24. Peu significatif au dela de J-8.")
log_trace("PACF / Grid PV 1 : Negatif de H-1 a H-12 puis positif jusqu'a H-23. Ce pattern cycle. Dernier lag significatif a J-10")
log_trace("PACF / Grid PV 2 : Negatif de H-1 a H-12 puis positif jusqu'a H-23. Ce pattern cycle. Dernier lag significatif a J-10")

## Plot a scatter plot of the demand against lags of the demand series.
household_data_with_lags <- household_data_long %>%
  group_by(across(all_of(cols_grouping))) %>%
  lag_many("ener_kWh", lags_to_create) %>%
  select(utc_timestamp, id_unit, all_of(cols_grouping), starts_with("ener_kWh"))

household_data_with_lags %>%
  ungroup() %>%
  filter(id_unit == 1) %>%
  select(starts_with("ener_kWh"), !ener_kWh_cumul) %>%
  GGally::ggpairs()

## Are relationships linear ? If so, calculate adjusted coeffs of determination.
## Which lags give the biggest values ?
