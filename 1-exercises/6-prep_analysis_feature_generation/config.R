filter_household <- list(
  household_type = "industrial",
  id_household = 1,
  energy_type = "grid_import",
  id_ener_source = c(NA)
)

filter_household_as_str <-
  map2(
    filter_household,
    names(filter_household),
    function(param, nom_param) {glue("{nom_param} : {param}")}
  ) %>%
  reduce(function(x, y) paste(x, y, sep = ", "))
