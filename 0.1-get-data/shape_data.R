here::i_am("0.1-get-data/shape_data.R")

source(here::here("libraries.R"))
source(here::here("config.R"))
source(here::here("0.1-get-data", "config.R"))

tryCatch(
  {
    if (!file.exists(here::here(path_hourly_data_long))) {
      if (!file.exists(here::here(path_hourly_data))) {
        log_error("Le fichier '{path_hourly_data}' n'existe pas. Veuillez le telecharger.")
        stop()
      }
      log_tictoc("Lecture du fichier {path_hourly_data}")
      household_data <- read_delim(here::here(path_hourly_data))
      household_data_long <- household_data %>%
        pivot_longer(
          cols = all_of(cols_ener),
          names_to = cols_grouping,
          values_to = "ener_kWh",
          names_pattern = regex_col_names
        ) %>%
        mutate(
          energy_type = str_replace(energy_type, "(.+)_$", "\\1") # on retire les _ a la fin des strings
        ) %>%
        group_by(across(all_of(cols_grouping))) %>%
        mutate(
          ener_kWh_cumul = ener_kWh,
          ener_kWh = ener_kWh_cumul - lag(ener_kWh_cumul, 1) # l'energie dans la donnee telechargee depuis le site est l'energie cumulee
        )
      household_data_long %>%
        write_csv(here::here(path_hourly_data_long))
      log_tictoc("Donnees au pas horaire ecrites dans le fichier {path_hourly_data_long}")
      beep("complete")
    } else {
      log_info("Fichier {path_hourly_data_long} deja existant. Rien a faire.")
      beep("ready")
    }
  },
  error = function(cond) {
    msg_error = "Erreur en ecrivant le fichier {path_hourly_data_long}.
    Voici le message d'erreur initial : {cond}"
    log_error(msg_error)
    beep("wilhelm")
  }
)

tryCatch(
  {
    if (!file.exists(here::here(path_ref_units))) {
      if (!file.exists(here::here(path_hourly_data_long))) {
        log_error("Le fichier '{path_hourly_data_long}' n'existe pas. Veuillez le creer.")
        stop()
      }
      log_tictoc("Lecture du fichier {path_hourly_data_long}")
      household_data_long <- read_delim(here::here(path_hourly_data_long))

      ref_units <- household_data_long %>%
        group_by(household_type, id_household, energy_type, id_ener_source) %>%
        mutate(id_ener_source = if_else(is.na(id_ener_source), "", as.character(id_ener_source))) %>%
        summarise(nb_rows = n()) %>%
        select(household_type, id_household, energy_type, id_ener_source) %>%
        ungroup() %>%
        mutate(id_unit = row_number())
      ref_units %>%
        write_csv(here::here(path_ref_units))
      log_tictoc("Reference des unites ecrite dans {path_ref_units}")
      beep("complete")
    } else {
      log_info("Fichier {path_ref_units} deja existant. Rien a faire.")
      beep("ready")
    }
  },
  error = function(cond) {
    msg_error = "Erreur en ecrivant le fichier {path_ref_units}.
    Voici le message d'erreur initial : {cond}"
    log_error(msg_error)
    beep("wilhelm")
  }
)
