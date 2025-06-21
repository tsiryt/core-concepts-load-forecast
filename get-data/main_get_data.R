here::i_am("get-data/main_get_data.R")

source(here::here("libraries.R"))
source(here::here("get-data", "config.R"))

household_data <- read_delim(here::here(path_hourly_data))

household_data_long <- household_data %>%
  pivot_longer(
    cols = all_of(cols_ener),
    names_to = c("household_type", "id_household", "energy_type", "id_ener_source"),
    values_to = "ener_kWh",
    names_pattern = regex_col_names
  ) %>%
  mutate(
    energy_type = str_replace(energy_type, "(.+)_$", "\\1") # on retire les _ a la fin des strings
  )
