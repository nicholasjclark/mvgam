#' Latent trend model file modifications
#'
#'
#' @export
#' @param model_file A template `JAGS` model file to be modified
#' @param use_lv Logical (use latent variable trends or not)
#' @param trend_model The type of trend model to be added to the model file
#' @param drift Logical (add drift or not)
#' @return A modified `JAGS` model file
add_trend_lines = function(model_file, use_lv, trend_model, drift){

  if(trend_model == 'None'){
    model_file[grep('mu\\[i, s\\] <- exp', model_file)] <- 'mu[i, s] <- exp(eta[ytimes[i, s]])'
    model_file <- model_file[-c(grep('## trend estimates', model_file):
                                  (grep('## trend estimates', model_file) + 27))]
  }

  if(use_lv){
    if(trend_model == 'RW'){
      model_file <- model_file[-c((grep('## latent factors evolve', model_file) + 6):
                                    (grep('## latent factors evolve', model_file) + 19))]

      if(drift){
        model_file[grep('## latent factors evolve', model_file) + 5] <-
          '\nfor (i in 2:n) {\nfor (j in 1:n_lv){\nLV[i, j] ~ dnorm(phi[j] + LV[i - 1, j], penalty[j])\n}\n}\n'
      } else {
        model_file[grep('## latent factors evolve', model_file) + 5] <-
          '\nfor (i in 2:n) {\nfor (j in 1:n_lv){\nLV[i, j] ~ dnorm(LV[i - 1, j], penalty[j])\n}\n}\n'
        model_file <- model_file[-grep('phi\\[s\\] ~', model_file)]
      }

      model_file <- readLines(textConnection(model_file), n = -1)
      model_file <- model_file[-c((grep('## AR components', model_file)):
                                    (grep('## AR components', model_file) + 5))]
    }

    if(trend_model == 'AR1'){
      model_file <- model_file[-c((grep('## latent factors evolve', model_file) + 6):
                                    (grep('## latent factors evolve', model_file) + 12))]
      model_file[grep('## latent factors evolve', model_file) + 7] <-
        'for (i in 2:n) {'
      model_file <- model_file[-c((grep('## latent factors evolve', model_file) + 9):
                                    (grep('## latent factors evolve', model_file) + 10))]

      if(drift){

      } else {
        model_file[grep('## latent factors evolve', model_file) + 9] <-
          'LV[i, j] ~ dnorm(ar1[j]*LV[i - 1, j], penalty[j])\n}'
        model_file <- model_file[-grep('phi\\[s\\] ~', model_file)]
      }

      model_file <- readLines(textConnection(model_file), n = -1)
      model_file <- model_file[-grep('ar2\\[s\\] ~', model_file)]
      model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
    }

    if(trend_model == 'AR2'){
      model_file <- model_file[-c((grep('## latent factors evolve', model_file) + 10):
                                    (grep('## latent factors evolve', model_file) + 12))]
      model_file[grep('## latent factors evolve', model_file) + 11] <-
        'for (i in 3:n) {'

      if(drift){
        model_file[grep('## latent factors evolve', model_file) + 14] <-
          'ar2[j]*LV[i - 2, j], penalty[j])'
      } else {
        model_file[grep('## latent factors evolve', model_file) + 7] <-
          'LV[2, j] ~ dnorm(ar1[j]*LV[1, j], penalty[j])'
        model_file[grep('## latent factors evolve', model_file) + 13] <-
        'LV[i, j] ~ dnorm(ar1[j]*LV[i - 1, j] +'
        model_file[grep('## latent factors evolve', model_file) + 14] <-
          'ar2[j]*LV[i - 2, j], penalty[j])'
        model_file <- model_file[-grep('phi\\[s\\] ~', model_file)]
      }
      model_file <- readLines(textConnection(model_file), n = -1)
      model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
    }

    if(trend_model == 'AR3'){

      if(drift){

      } else {
        model_file[grep('## latent factors evolve', model_file) + 7] <-
          'LV[2, j] ~ dnorm(ar1[j]*LV[1, j], penalty[j])'
        model_file[grep('## latent factors evolve', model_file) + 11] <-
          'LV[3, j] ~ dnorm(ar1[j]*LV[2, j] + ar2[j]*LV[1, j], penalty[j])'
        model_file[grep('## latent factors evolve', model_file) + 16] <-
          'LV[i, j] ~ dnorm(ar1[j]*LV[i - 1, j] +'
        model_file <- model_file[-grep('phi\\[s\\] ~', model_file)]
      }
      model_file <- readLines(textConnection(model_file), n = -1)
    }
  }

  if(!use_lv){

    if(trend_model == 'RW'){
    model_file <- model_file[-c((grep('## trend estimates', model_file) + 4):
                                  (grep('## trend estimates', model_file) + 17))]

    if(drift){
      model_file[grep('## trend estimates', model_file) + 4] <-
        '\nfor (i in 2:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(phi[s] + trend[i - 1, s], tau[s])\n}\n}\n'
    } else {
      model_file[grep('## trend estimates', model_file) + 4] <-
        '\nfor (i in 2:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(trend[i - 1, s], tau[s])\n}\n}\n'
      model_file <- model_file[-grep('phi\\[s\\] ~', model_file)]
    }

    model_file <- readLines(textConnection(model_file), n = -1)
    model_file <- model_file[-grep('ar1\\[s\\] ~', model_file)]
    model_file <- model_file[-grep('ar2\\[s\\] ~', model_file)]
    model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
  }

  if(trend_model == 'AR1'){
    model_file <- model_file[-c((grep('## trend estimates', model_file) + 4):
                                  (grep('## trend estimates', model_file) + 17))]

    if(drift){
      model_file[grep('## trend estimates', model_file) + 4] <-
        '\nfor (i in 2:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(phi[s] + ar1[s]*trend[i - 1, s], tau[s])\n}\n}\n'
    } else {
      model_file[grep('## trend estimates', model_file) + 4] <-
        '\nfor (i in 2:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(ar1[s]*trend[i - 1, s], tau[s])\n}\n}\n'
      model_file <- model_file[-grep('phi\\[s\\] ~', model_file)]
    }

    model_file <- readLines(textConnection(model_file), n = -1)
    model_file <- model_file[-grep('ar2\\[s\\] ~', model_file)]
    model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
  }

  if(trend_model == 'AR2'){
    model_file <- model_file[-c((grep('## trend estimates', model_file) + 9):
                                  (grep('## trend estimates', model_file) + 17))]

    if(drift){
      model_file[grep('## trend estimates', model_file) + 9] <-
        '\nfor (i in 3:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(phi[s] + ar1[s]*trend[i - 1, s] + ar2[s]*trend[i - 2, s], tau[s])\n}\n}\n'
    } else {
      model_file[grep('## trend estimates', model_file) + 6] <-
        'trend[2, s] ~ dnorm(ar1[s]*trend[1, s], tau[s])'
      model_file[grep('## trend estimates', model_file) + 9] <-
        '\nfor (i in 3:n) {\nfor (s in 1:n_series){\ntrend[i, s] ~ dnorm(ar1[s]*trend[i - 1, s] + ar2[s]*trend[i - 2, s], tau[s])\n}\n}\n'
      model_file <- model_file[-grep('phi\\[s\\] ~', model_file)]
    }
    model_file <- readLines(textConnection(model_file), n = -1)
    model_file <- model_file[-grep('ar3\\[s\\] ~', model_file)]
  }

  if(trend_model == 'AR3'){

    if(drift){

      } else {
      model_file[grep('## trend estimates', model_file) + 6] <-
        'trend[2, s] ~ dnorm(ar1[s]*trend[1, s], tau[s])'
      model_file[grep('## trend estimates', model_file) + 10] <-
        'trend[3, s] ~ dnorm(ar1[s]*trend[2, s] + ar2[s]*trend[1, s], tau[s])'
      model_file[grep('## trend estimates', model_file) + 15] <-
      'trend[i, s] ~ dnorm(ar1[s]*trend[i - 1, s] + ar2[s]*trend[i - 2, s] + ar3[s]*trend[i - 3, s], tau[s])'
      model_file <- model_file[-grep('phi\\[s\\] ~', model_file)]
    }
    model_file <- readLines(textConnection(model_file), n = -1)
  }
  }

  model_file
}
