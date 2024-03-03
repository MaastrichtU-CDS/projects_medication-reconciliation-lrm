# model_1 ----

model_1 <- function(age,
                    sex.male_1,
                    n_prescribed_medicines,
                    high_risk_medicines.yes_1,
                    eGFR.below_60_1,
                    out_specialty_current_MUMC.group_A_1,
                    out_specialty_current_MUMC.group_B_1,
                    n_specialty_MUMC,
                    health_literacy_composite.insufficient_1) {
  lp <- 
    5.1277 * -1 +
    0.1406 * ((age - 59.2184) / 18.5225) +
    0.4051 * -1 * sex.male_1 + 
    0.4206 * ((n_prescribed_medicines - 5.9345) / 4.7231) +
    0.4878 * high_risk_medicines.yes_1 +  
    0.3966 * -1 * eGFR.below_60_1 +
    1.6124 * out_specialty_current_MUMC.group_A_1 +
    1.6189 * out_specialty_current_MUMC.group_B_1 +
    0.2674 * -1 * ((n_specialty_MUMC - 2.8139) / 1.8980) +
    0.7364 * health_literacy_composite.insufficient_1
  
  phat <- 1 / (1 + exp(-lp))
  
  return(phat)
}

# model 2 ----

model_2 <- function(age,
                    sex.male_1,
                    n_prescribed_medicines,
                    high_risk_medicines.yes_1,
                    health_literacy_composite.insufficient_1) {
  lp <- 
    3.9586 * -1 +
    0.0406 * ((age - 59.2184) / 18.5225) +
    0.3084 * ((n_prescribed_medicines - 5.9345) / 4.7231) +
    0.6339 * high_risk_medicines.yes_1 +  
    0.7827 * health_literacy_composite.insufficient_1
  
  phat <- 1 / (1 + exp(-lp))
  
  return(phat)
}
