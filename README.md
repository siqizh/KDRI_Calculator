# KDRI_Calculator

A repository containing R code for calculating the Kidney Donor Risk Index (KDRI) using both the traditional KDRI_RAO calculation and the updated KDRI as stated by OPTN. This updated KDRI calculation excludes race and HCV donor characteristics.

## Overview

The Kidney Donor Risk Index (KDRI) is a metric used to assess the risk associated with kidney transplantation based on donor characteristics. This repository aims to assist researchers and practitioners in utilizing R for KDRI calculations.

### Features
- **KDRI_traditional**: The original KDRI calculation method, which includes race and HCV.
- **KDRI_no_race_hcv**: The new KDRI calculation method that reflects updated guidelines, excluding race and HCV.

## Installation

To use this code, you will need to have R installed. Please ensure that the donor characteristics are coded as follows:

- **age**: Continuous variable.
- **race**:
  - For **KDRI_traditional**: If the donor is African American, set `race = 1`.
  - For **KDRI_no_race_hcv**: Ignore race.
- **creatinine**: Continuous variable.
- **history of hypertension**: 
  - Set to `1` if yes, and `0` if no. 
  - If your data contains missingness in hypertension, please calculate and provide the **hypertension_prevalence**.
- **hypertension_prevalence**: The proportion of hypertension in each year.
- **history of diabetes**: 
  - Set to `1` if yes, and `0` if no. 
  - If your data contains missingness in diabetes, please calculate and provide the **diabetes_prevalence**.
- **diabetes_prevalence**: The proportion of diabetes in each year.
- **cause of death**: 
  - For donors with cerebrovascular accident as the cause of death, set `COD = 1`.
- **height**: Continuous variable.
- **weight**: Continuous variable.
- **donor type**: For DCD donors, set `dcd = 1`.
- **HCV status**: 
  - For **KDRI_traditional**: If the donor is HCV positive, set `hcv = 1`.
  - For **KDRI_no_race_hcv**: Ignore HCV status.
- 



