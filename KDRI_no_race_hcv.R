KDRI_no_race_hcv = function(age, height, weight, history_hypertension, 
                            history_diabetes, cod, creatinine, dcd,
                            hypertension_prevalence = NULL, diabetes_prevalence = NULL) {
  
  #' Calculate KDRI without race and HCV
  #'
  #' @param age Age of the donor
  #' @param height Height of the donor in cm
  #' @param weight Weight of the donor in kg
  #' @param history_hypertension Indicator for history of hypertension (1: Yes, 0: No)
  #' @param history_diabetes Indicator for history of diabetes (1: Yes, 0: No)
  #' @param cod Cause of death indicator (1 for CVA, 0 otherwise)
  #' @param creatinine Serum creatinine level
  #' @param dcd Indicator for DCD status (1: Yes, 0: No)
  #' @param hypertension_prevalence Proportion of hypertension each year (optional)
  #' @param diabetes_prevalence Proportion of diabetes each year (optional)
  #'
  #' @return KDRI calculated without race and HCV
  #' @export
  
  # Input validation
  if (age < 0 || height <= 0 || weight <= 0 || creatinine < 0) {
    stop("Invalid input: Age, height, weight, and creatinine must be positive values.")
  }
  
  if (!is.null(hypertension_prevalence) && 
      (hypertension_prevalence < 0 || hypertension_prevalence > 1)) {
    stop("Invalid input: hypertension_prevalence should be between 0 and 1.")
  }
  
  if (!is.null(diabetes_prevalence) && 
      (diabetes_prevalence < 0 || diabetes_prevalence > 1)) {
    stop("Invalid input: diabetes_prevalence should be between 0 and 1.")
  }
  
  # Age terms
  age_term1 = 0.0092 * (age - 40)
  age_term2 = ifelse(age < 18, 0.0113 * (age - 18), 0)
  age_term3 = ifelse(age > 50, 0.0067 * (age - 50), 0)
  
  # Height and weight terms
  height_term = -0.0557 * ((height - 170) / 10)
  weight_term = ifelse(weight < 80, -0.0333 * ((weight - 80) / 5), 0)
  
  # Other terms
  cod_cva_term = ifelse(cod == 1, 0.0743, 0)
  
  # Creatinine terms
  creatinine_term1 = 0.2128 * (creatinine - 1)
  creatinine_term2 = ifelse(creatinine > 1.5, -0.2199 * (creatinine - 1.5), 0)
  
  # DCD term
  dcd_term = ifelse(dcd == 1, 0.1966, 0)
  
  # Hypertension term
  if (history_hypertension %in% c(0, 1)) {
    hypertension_term = ifelse(history_hypertension == 1, 0.1106, 0)
  } else {
    if (is.null(hypertension_prevalence)) {
      stop("Please provide hypertension_prevalence when history_hypertension contains missingness or values other than 0 and 1.")
    }
    hypertension_term = 0.1106 * hypertension_prevalence
  }
  
  # Diabetes term
  if (history_diabetes %in% c(0, 1)) {
    diabetes_term = ifelse(history_diabetes == 1, 0.2577, 0)
  } else {
    if (is.null(diabetes_prevalence)) {
      stop("Please provide diabetes_prevalence when history_diabetes contains missingness or values other than 0 and 1.")
    }
    diabetes_term = 0.2577 * diabetes_prevalence
  }
  
  # Total score
  score = age_term1 + age_term2 + age_term3 + height_term + weight_term +
    hypertension_term + diabetes_term + cod_cva_term +
    creatinine_term1 + creatinine_term2 + dcd_term
  
  # Calculate KDRO
  kdri_rao = exp(score)
  
  return(kdri_rao)
}

#' @examples
#' # Standard use case
# kdri_score = KDRI_no_race_hcv(age = 45, height = 175, weight = 75,
#                                  history_hypertension = 1,
#                                  history_diabetes = 0, cod = 1,
#                                  creatinine = 1.2, dcd = 0)
#'
#' # Use case with missing history
# kdri_score = KDRI_no_race_hcv(age = 45, height = 175, weight = 75,
#                                  history_hypertension = NA,
#                                  history_diabetes = NA, cod = 1,
#                                  creatinine = 1.2, dcd = 0,
#                                  hypertension_prevalence = 0.25,
#                                  diabetes_prevalence = 0.15)
#'
#' # Example with specific prevalence values
# kdri_score = KDRI_no_race_hcv(age = 60, height = 180, weight = 85,
#                                  history_hypertension = 2,  # Value other than 0/1
#                                  history_diabetes = 1, cod = 0,
#                                  creatinine = 1.5, dcd = 1,
#                                  hypertension_prevalence = 0.30,
#                                  diabetes_prevalence = 0.20)