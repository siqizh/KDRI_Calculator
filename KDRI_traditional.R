KDRI_traditional <- function(age, height, weight, race, history_hypertension, 
                             history_diabetes, cod, creatinine, hcv, dcd,
                             hypertension_prevalence = NULL, diabetes_prevalence = NULL) {
  
  #' Calculate KDRI using traditional method
  #'
  #' @param age Age of the donor
  #' @param height Height of the donor in cm
  #' @param weight Weight of the donor in kg
  #' @param race Indicator if donor is African American
  #' @param history_hypertension Indicator for history of hypertension
  #' @param history_diabetes Indicator for history of diabetes
  #' @param cod Cause of death indicator
  #' @param creatinine Serum creatinine level
  #' @param hcv Indicator for HCV status
  #' @param dcd Indicator for DCD status
  #' @param hypertension_prevalence Proportion of hypertension each year
  #' @param diabetes_prevalence Proportion of diabetes each year
  #'
  #' @return KDRI calculated using the traditional method
  #' @export
  
  # Input validation
  if (age < 0 || height <= 0 || weight <= 0 || creatinine < 0) {
    stop("Invalid input: Age, height, weight, and creatinine must be positive values.")
  }
  
  if (!is.null(hypertension_prevalence) && (hypertension_prevalence < 0 || hypertension_prevalence > 1)) {
    stop("Invalid input: hypertension_prevalence should be between 0 and 1.")
  }
  
  if (!is.null(diabetes_prevalence) && (diabetes_prevalence < 0 || diabetes_prevalence > 1)) {
    stop("Invalid input: diabetes_prevalence should be between 0 and 1.")
  }
  
  # Age terms
  age_term1 <- 0.0128 * (age - 40)
  age_term2 <- ifelse(age < 18, -0.0194 * (age - 18), 0)
  age_term3 <- ifelse(age > 50, 0.0107 * (age - 50), 0)
  
  # Height and weight terms
  height_term <- -0.0464 * ((height - 170) / 10)
  weight_term <- ifelse(weight < 80, -0.0199 * ((weight - 80) / 5), 0)
  
  # Other terms
  race_term <- ifelse(race == 1, 0.1790, 0)
  cod_cva_term <- ifelse(cod == 1, 0.0881, 0)
  
  # Hypertension term
  if (history_hypertension %in% c(0, 1)) {
    hypertension_term <- ifelse(history_hypertension == 1, 0.1260, 0)
  } else {
    if (is.null(hypertension_prevalence)) {
      stop("Please provide hypertension_prevalence when history_hypertension contains missingness or values other than 0 and 1.")
    }
    hypertension_term <- 0.1260 * hypertension_prevalence
  }
  
  # Diabetes term
  if (history_diabetes %in% c(0, 1)) {
    diabetes_term <- ifelse(history_diabetes == 1, 0.1300, 0)
  } else {
    if (is.null(diabetes_prevalence)) {
      stop("Please provide diabetes_prevalence when history_diabetes contains missingness or values other than 0 and 1.")
    }
    diabetes_term <- 0.1300 * diabetes_prevalence
  }
  
  # Creatinine terms
  creatinine_term1 <- 0.2200 * (creatinine - 1)
  creatinine_term2 <- ifelse(creatinine > 1.5, -0.2090 * (creatinine - 1.5), 0)
  
  # HCV and DCD terms
  hcv_term <- ifelse(hcv == 1, 0.2400, 0)
  dcd_term <- ifelse(dcd == 1, 0.1330, 0)
  
  # Total score
  score <- age_term1 + age_term2 + age_term3 + height_term + weight_term +
    race_term + hypertension_term + diabetes_term + cod_cva_term +
    creatinine_term1 + creatinine_term2 + hcv_term + dcd_term
  
  # Calculate KDRI
  kdri_rao <- exp(score)
  
  return(kdri_rao)
}

#' @examples
#' # Standard use case
# kdri_score = KDRI_traditional(age = 45, height = 175, weight = 75,
#                                 race = 1, history_hypertension = 1,
#                                 history_diabetes = 0, cod = 1,
#                                 creatinine = 1.2, hcv = 0, dcd = 0)
#'
#' # Use case with missing history
# kdri_score = KDRI_traditional(age = 45, height = 175, weight = 75,
#                                 race = 1, history_hypertension = NA,
#                                 history_diabetes = NA, cod = 1,
#                                 creatinine = 1.2, hcv = 0, dcd = 0,
#                                 hypertension_prevalence = 0.25,
#                                 diabetes_prevalence = 0.15)
#'
#' # Example with specific prevalence values
#' kdri_score = KDRI_traditional(age = 60, height = 180, weight = 85,
#'                                 race = 0, history_hypertension = 2,  # Value other than 0/1
#'                                 history_diabetes = 1, cod = 0,
#'                                 creatinine = 1.5, hcv = 1, dcd = 1,
#'                                 hypertension_prevalence = 0.30,
#'                                 diabetes_prevalence = 0.20)