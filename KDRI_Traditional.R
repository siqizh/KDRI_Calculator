KDRI_RAO = function(age, height, weight, race_black, history_hypertension, 
                    history_diabetes, cod_cva, creatinine, hcv_positive, dcd,
                    hypertension_prevalence, diabetes_prevalence) {
  # Age terms
  age_term1 = 0.0128 * (age - 40)
  age_term2 = ifelse(age < 18, -0.0194 * (age - 18), 0)
  age_term3 = ifelse(age > 50, 0.0107 * (age - 50), 0)
  
  # Height and weight terms
  height_term = -0.0464 * ((height - 170) / 10)
  weight_term = ifelse(weight < 80, -0.0199 * ((weight - 80) / 5), 0)
  
  # Other terms
  race_term = ifelse(race_black == 2, 0.1794, 0)
  cod_cva_term = ifelse(cod_cva == 2, 0.0881, 0)
  
  # hypertension and diabetes
  hypertension_term = ifelse(history_hypertension == "Y", 0.1262, 
                             ifelse(history_hypertension == "N", 0, 
                                    0.1262 * hypertension_prevalence))
  diabetes_term = ifelse(history_diabetes == "Y", 0.1301, 
                         ifelse(history_diabetes == "N", 0, 
                                0.1301 * diabetes_prevalence))
  
  
  creatinine_term1 = 0.2198 * (creatinine - 1)
  creatinine_term2 = ifelse(creatinine > 1.5, -0.2093 * (creatinine - 1.5), 0)
  
  hcv_term = ifelse(hcv_positive == "NAT +" | hcv_positive == "Antibody +", 0.2403, 0)
  dcd_term = ifelse(dcd == "Y", 0.1329, 0)
  
  # Total score
  score = age_term1 + age_term2 + age_term3 + height_term + weight_term +
    race_term + hypertension_term + diabetes_term + cod_cva_term +
    creatinine_term1 + creatinine_term2 + hcv_term + dcd_term
  
  kdri_rao = exp(score)
  
  return(kdri_rao)
}
