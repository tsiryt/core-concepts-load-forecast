chosen_units <- c(1,2,3)

lag_nb <- 13 # nombre de valeurs laggees a creer
lag_step <- 1 # le pas de lags sur lequel on boucle

# rep(1, times = 10) * 1:10 ==> 10 lags, espaces de 6 heures chacun
lags_to_create <- rep(lag_step, times = lag_nb) * 1:lag_nb
