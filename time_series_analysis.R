library(feasts)
library(tsibble)
library(cowplot)

#' Trace des ACF/PACF plot pour plusieurs colonnes d'un df.
#' 
#' @info Taken from https://stackoverflow.com/questions/66605361/how-to-calculate-acf-for-multiple-series-and-plot-them-all-at-once
#' 
#' @param data df. Doit posseder un colonne POSIXct
#' @param vars string vector. Liste des colonnes que l'on souhaite analyser
#' @param time_index string. Nom de la colonne horodate
#' @param lag_max int.
#' @param rows TODO : voir l'utilite.
#' 
#' @return plot. Potentiellement, plusieurs plots.
plot_correlation_fun <- function(data, vars, time_index, lag_max = 7, rows = NULL, type = "acf"){
  if(is.null(rows)){
    rows <- ceiling(sqrt(length(vars)))
  }
  correlation_function <- list(
    acf = feasts::ACF,
    pacf = feasts::PACF
  )[[type]]
  glist <- list()
  for(i in 1:length(vars)){
    glist[[i]] <- data%>%
      as_tsibble(index = .data[[time_index]])%>%
      correlation_function(.data[[vars[i]]],lag_max = lag_max)%>%
      autoplot() + ggtitle(vars[i])
  }
  glist[["nrow"]] = rows
  do.call(cowplot::plot_grid, glist)
}
