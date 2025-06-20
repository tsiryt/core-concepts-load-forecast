here::i_am("get-data/main_get_data.R")

source(here::here("libraries.R"))
source(here::here("get-data", "config.R"))

household_data <- read_delim(here::here(path_hourly_data))
