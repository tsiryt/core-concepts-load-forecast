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
          names_to = c("household_type", "id_household", "energy_type", "id_ener_source"),
          values_to = "ener_kWh",
          names_pattern = regex_col_names
        ) %>%
        mutate(
          energy_type = str_replace(energy_type, "(.+)_$", "\\1") # on retire les _ a la fin des strings
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
