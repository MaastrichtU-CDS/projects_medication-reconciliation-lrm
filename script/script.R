# Set seed for reproducibility ----

set.seed(813)

# Packages ----

library(readxl)
library(ggplot2)
library(cowplot)
library(pmsampsize)
library(rms)

# Data ---- 

data <- ...

# Variables ----

## Patient ----

### Age ----

names(data)[names(data) == "A2a"] <- "age"

summary(data$age)
sd(data$age)
ggplot(data, aes(x = age)) + 
  geom_histogram(binwidth = 1) + 
  scale_x_continuous(breaks = seq(0, 100, 10),
                     labels = seq(0, 100, 10)) +
  theme_minimal()

data$age.centered <- data$age - mean(data$age)
data$age.std <- data$age.centered / sd(data$age)

### Sex ----

names(data)[names(data) == "A1a"] <- "sex.male_1"
data$sex.male_1 <- ifelse(data$sex.male_1 == 2,
                          0, 
                          data$sex.male_1)

table(data$sex.male_1, exclude = NULL)
prop.table(table(data$sex.male_1, exclude = NULL))

### Housing ----

names(data)[names(data) == "A3b"] <- "housing"
data$housing <- ordered(data$housing, 
                        levels = c(1, 
                                   2, 
                                   3), 
                        labels = c("independent", 
                                   "home care", 
                                   "institution"))
data$housing.independent_1 <- ifelse(data$housing == "independent", 
                                     1, 
                                     0) 
data$housing.home_care_1 <- ifelse(data$housing == "home care", 
                                   1, 
                                   0) 
data$housing.institution_1 <- ifelse(data$housing == "institution", 
                                     1, 
                                     0) 

table(data$housing, exclude = NULL)
prop.table(table(data$housing, exclude = NULL))

data$housing.home_care_or_institution_1 <-ifelse((data$housing == "home care")
                                                 |
                                                 (data$housing == "institution"),
                                                 1,
                                                 0)

### Person responsible for medicines ----

names(data)[names(data) == "A5b"] <- "person_responsible_for_medicines"
data$person_responsible_for_medicines <- factor(data$person_responsible_for_medicines, 
                                                levels = c(1, 
                                                           2, 
                                                           3), 
                                                labels = c("patient", 
                                                           "partner or caregiver", 
                                                           "health professional"))
data$person_responsible_for_medicines.patient_1 <- ifelse(data$person_responsible_for_medicines == "patient", 
                                                          1, 
                                                          0) 
data$person_responsible_for_medicines.partner_or_caregiver_1 <- ifelse(data$person_responsible_for_medicines == "partner or caregiver", 
                                                                       1, 
                                                                       0) 
data$person_responsible_for_medicines.health_professional_1 <- ifelse(data$person_responsible_for_medicines == "health professional", 
                                                                      1,
                                                                      0) 

table(data$person_responsible_for_medicines, exclude = NULL)
prop.table(table(data$person_responsible_for_medicines, exclude = NULL))

data$person_responsible_for_medicines.partner_or_caregiver_or_health_professional_1 <- ifelse((data$person_responsible_for_medicines == "partner or caregiver")
                                                                                              |
                                                                                              (data$person_responsible_for_medicines == "health professional"),
                                                                                              1,
                                                                                              0)

### Independence ----

data$independent_composite.yes_1 <- ifelse(is.na(data$housing.independent_1)
                                           |
                                           is.na(data$person_responsible_for_medicines.patient_1),
                                           NA,
                                           0)
data$independent_composite.yes_1 <- ifelse((data$housing.independent_1 == 1)
                                           &
                                           (data$person_responsible_for_medicines.patient_1 == 1),
                                           1,
                                           data$independent_composite.yes_1)

table(data$independent_composite.yes_1, exclude = NULL)
prop.table(table(data$independent_composite.yes_1, exclude = NULL))

### Health literacy ---- 

names(data)[names(data) == "A7c"] <- "health_literacy"
data$health_literacy <- factor(data$health_literacy, 
                               levels = c(1, 
                                          2, 
                                          3), 
                               labels = c("adequate", 
                                          "suboptimal", 
                                          "insufficient"))
data$health_literacy.adequate_1 <- ifelse(data$health_literacy == "adequate", 
                                          1, 
                                          0)  
data$health_literacy.suboptimal_1 <- ifelse(data$health_literacy == "suboptimal", 
                                            1, 
                                            0)  
data$health_literacy.insufficient_1 <- ifelse(data$health_literacy == "insufficient", 
                                              1, 
                                              0)  

table(data$health_literacy, exclude = NULL)
prop.table(table(data$health_literacy, exclude = NULL))

data$health_literacy.suboptimal_or_insufficient_1 <- ifelse((data$health_literacy == "suboptimal")
                                                            |
                                                            (data$health_literacy == "insufficient"),
                                                            1,
                                                            0)

data$health_literacy_composite.insufficient_1 <- ifelse(is.na(data$independent_composite.yes_1)
                                                        |
                                                        is.na(data$health_literacy.adequate_1),
                                                        NA,
                                                        0)
data$health_literacy_composite.insufficient_1 <- ifelse((data$independent_composite.yes_1 == 1)
                                                        &
                                                        (data$health_literacy.adequate_1 == 0),
                                                        1,
                                                        data$health_literacy_composite.insufficient_1)

table(data$health_literacy_composite.insufficient_1, exclude = NULL)
prop.table(table(data$health_literacy_composite.insufficient_1, exclude = NULL))

### Education ----

names(data)[names(data) == "A6b"] <- "education"
data$education <- ordered(data$education, 
                          levels = c(1, 
                                     2, 
                                     3, 
                                     4), 
                          labels = c("type 1",
                                     "type 2", 
                                     "type 3",
                                     "type 4"))
data$education.type_1_1 <- ifelse(data$education == "type 1", 
                                  1, 
                                  0) 
data$education.type_2_1 <- ifelse(data$education == "type 2", 
                                  1, 
                                  0) 
data$education.type_3_1 <- ifelse(data$education == "type 3", 
                                  1, 
                                  0) 
data$education.type_4_1 <- ifelse(data$education == "type 4", 
                                  1, 
                                  0) 

table(data$education, exclude = NULL)
prop.table(table(data$education, exclude = NULL))

### Current diseases ----

current_diseases.list <- strsplit(data$A10a, ",") 
data$current_diseases.diabetes_1 <- ifelse(lapply(current_diseases.list, function(x) { "1" %in% x}),
                                           1, 
                                           0) 
data$current_diseases.rheumatoid_arthritis_1 <- ifelse(lapply(current_diseases.list, function(x) { "2" %in% x}),
                                                       1,
                                                       0) 
data$current_diseases.asthma_COPD_1 <- ifelse(lapply(current_diseases.list, function(x) { "3" %in% x}),
                                              1,
                                              0) 
data$current_diseases.cardiovascular_disease_1 <- ifelse(lapply(current_diseases.list, function(x) { "4" %in% x}),
                                                         1, 
                                                         0) 
data$current_diseases.cardiac_arrhythmia_1 <- ifelse(lapply(current_diseases.list, function(x) { "5" %in% x}),
                                                     1,
                                                     0) 
data$current_diseases.heart_failure_1 <- ifelse(lapply(current_diseases.list, function(x) { "6" %in% x}),
                                                1,
                                                0) 
data$current_diseases.cancer_1 <- ifelse(lapply(current_diseases.list, function(x) { "7" %in% x}),
                                         1,
                                         0) 
data$current_diseases.none_of_the_above_1 <- ifelse(lapply(current_diseases.list, function(x) { "8" %in% x}),
                                                    1,
                                                    0) 

table(data$current_diseases.diabetes_1)
prop.table(table(data$current_diseases.diabetes_1))
table(data$current_diseases.rheumatoid_arthritis_1)
prop.table(table(data$current_diseases.rheumatoid_arthritis_1))
table(data$current_diseases.asthma_COPD_1)
prop.table(table(data$current_diseases.asthma_COPD_1))
table(data$current_diseases.cardiovascular_disease_1)
prop.table(table(data$current_diseases.cardiovascular_disease_1))
table(data$current_diseases.cardiac_arrhythmia_1)
prop.table(table(data$current_diseases.cardiac_arrhythmia_1))
table(data$current_diseases.heart_failure_1)
prop.table(table(data$current_diseases.heart_failure_1))
table(data$current_diseases.cancer_1)
prop.table(table(data$current_diseases.cancer_1))
table(data$current_diseases.none_of_the_above_1) # This option was never selected. 
prop.table(table(data$current_diseases.none_of_the_above_1))

names(data)[names(data) == "A9a"] <- "n_current_diseases"
data$n_current_diseases <- ifelse(is.na(data$n_current_diseases),
                                  0,
                                  data$n_current_diseases)  

table(data$n_current_diseases, exclude = NULL)
prop.table(table(data$n_current_diseases, exclude = NULL))
summary(data$n_current_diseases)
sd(data$n_current_diseases, na.rm = TRUE)
ggplot(data, aes(x = n_current_diseases)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 6, 1),
                     labels = seq(0, 6, 1)) +
  theme_minimal() 

data$n_current_diseases.centered <- data$n_current_diseases - mean(data$n_current_diseases) 
data$n_current_diseases.std <- data$n_current_diseases.centered / sd(data$n_current_diseases)

### eGFR ----

names(data)[names(data) == "A12a"] <- "eGFR"
data$eGFR <- ifelse(is.na(data$eGFR), 
                    6, 
                    data$eGFR)
data$eGFR <- factor(data$eGFR, 
                    levels = c(1, 
                               2, 
                               3, 
                               4, 
                               5, 
                               6), 
                    labels = c("> 90 ml/min", 
                               "60-89 ml/min", 
                               "30-59 ml/min", 
                               "15-29 ml/min", 
                               "< 15 ml/min", 
                               "unknown or measured more than 12 months ago"))
data$eGFR.above_90_1 <- ifelse(data$eGFR == "> 90 ml/min", 
                               1, 
                               0)
data$eGFR.above_90_1 <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                               NA, 
                               data$eGFR.above_90_1)
data$eGFR.60_89_1 <- ifelse(data$eGFR == "60-89 ml/min", 
                            1, 
                            0)
data$eGFR.60_89_1 <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                            NA, 
                            data$eGFR.60_89_1)
data$eGFR.30_59_1 <- ifelse(data$eGFR == "30-59 ml/min", 
                            1, 
                            0)
data$eGFR.30_59_1 <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                            NA, 
                            data$eGFR.30_59_1)
data$eGFR.15_29_1 <- ifelse(data$eGFR == "15-29 ml/min", 
                            1, 
                            0)
data$eGFR.15_29_1 <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                            NA, 
                            data$eGFR.15_29_1)
data$eGFR.below_15_1 <- ifelse(data$eGFR == "< 15 ml/min", 
                               1, 
                               0)
data$eGFR.below_15_1 <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                               NA, 
                               data$eGFR.below_15_1)
data$eGFR.NA_1 <- ifelse(data$eGFR == "unknown or measured more than 12 months ago", 
                         1, 
                         0)

table(data$eGFR, exclude = NULL)
prop.table(table(data$eGFR, exclude = NULL))

data$eGFR.above_60_1 <- ifelse((data$eGFR.60_89_1 == 1)
                               |
                               (data$eGFR.above_90_1 == 1),
                               1,
                               0)
data$eGFR.below_60_1 <- ifelse((data$eGFR.30_59_1 == 1)
                               |
                               (data$eGFR.15_29_1 == 1)
                               |
                               (data$eGFR.below_15_1 == 1),
                               1,
                               0) 

## Medicines ----

### Number of prescribed medicines ----

names(data)[names(data) == "B1a"] <- "n_prescribed_medicines"

table(data$n_prescribed_medicines, exclude = NULL)
prop.table(table(data$n_prescribed_medicines, exclude = NULL))
summary(data$n_prescribed_medicines)
sd(data$n_prescribed_medicines)
ggplot(data, aes(x = n_prescribed_medicines)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, 1),
                     labels = seq(0, 20, 1)) +
  theme_minimal() 

data$n_prescribed_medicines.centered <- data$n_prescribed_medicines - mean(data$n_prescribed_medicines)
data$n_prescribed_medicines.std <- data$n_prescribed_medicines.centered / sd(data$n_prescribed_medicines)

### High-risk medicines ----

high_risk_medicines.list <- strsplit(data$B7a, ",") 
data$high_risk_medicines.platelet_aggregation_inhibitors_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "1" %in% x }),
                                                                     1, 
                                                                     0)  
data$high_risk_medicines.anticoagulants_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "2" %in% x }),
                                                    1, 
                                                    0) 
data$high_risk_medicines.NSAIDs_1 <-ifelse(lapply(high_risk_medicines.list, function(x) { "3" %in% x }),
                                           1, 
                                           0) 
data$high_risk_medicines.diuretics_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "4" %in% x }),
                                               1, 
                                               0) 
data$high_risk_medicines.RAS_inhibitors_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "5" %in% x }),
                                                    1, 
                                                    0) 
data$high_risk_medicines.systemic_corticosteroids_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "6" %in% x }), 
                                                              1, 
                                                              0) 
data$high_risk_medicines.opioids_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "7" %in% x }),
                                             1, 
                                             0) 
data$high_risk_medicines.glucose_lowering_medicines_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "8" %in% x }),
                                                                1, 
                                                                0) 
data$high_risk_medicines.psychotropics_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "9" %in% x }),
                                                   1, 
                                                   0) 
data$high_risk_medicines.cardiac_medicines_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "10" %in% x }),
                                                       1, 
                                                       0) 
data$high_risk_medicines.immunosuppressants_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "11" %in% x }),
                                                        1, 
                                                        0) 
data$high_risk_medicines.oncolytics_1 <- ifelse(lapply(high_risk_medicines.list, function(x) { "12" %in% x }),
                                                1, 
                                                0) 

table(data$high_risk_medicines.platelet_aggregation_inhibitors_1)
prop.table(table(data$high_risk_medicines.platelet_aggregation_inhibitors_1))
table(data$high_risk_medicines.anticoagulants_1)
prop.table(table(data$high_risk_medicines.anticoagulants_1))
table(data$high_risk_medicines.NSAIDs_1)
prop.table(table(data$high_risk_medicines.NSAIDs_1))
table(data$high_risk_medicines.diuretics_1)
prop.table(table(data$high_risk_medicines.diuretics_1))
table(data$high_risk_medicines.RAS_inhibitors_1)
prop.table(table(data$high_risk_medicines.RAS_inhibitors_1))
table(data$high_risk_medicines.opioids_1)
prop.table(table(data$high_risk_medicines.opioids_1))
table(data$high_risk_medicines.systemic_corticosteroids_1) 
prop.table(table(data$high_risk_medicines.systemic_corticosteroids_1))
table(data$high_risk_medicines.glucose_lowering_medicines_1)
prop.table(table(data$high_risk_medicines.glucose_lowering_medicines_1))
table(data$high_risk_medicines.psychotropics_1)
prop.table(table(data$high_risk_medicines.psychotropics_1))
table(data$high_risk_medicines.cardiac_medicines_1)
prop.table(table(data$high_risk_medicines.cardiac_medicines_1))
table(data$high_risk_medicines.immunosuppressants_1) 
prop.table(table(data$high_risk_medicines.immunosuppressants_1)) 
table(data$high_risk_medicines.oncolytics_1) 
prop.table(table(data$high_risk_medicines.oncolytics_1)) 

names(data)[names(data) == "B6a"] <- "high_risk_medicines.yes_1"
data$high_risk_medicines.yes_1 <- ifelse(data$high_risk_medicines.yes_1 == 2, 
                                         0, 
                                         data$high_risk_medicines.yes_1)

table(data$high_risk_medicines.yes_1, exclude = NULL)
prop.table(table(data$high_risk_medicines.yes_1, exclude = NULL))

### Medicines without prescription ----

names(data)[names(data) == "B2b"] <- "medicines_wo_prescription.list"
medicines_wo_prescription.list <- strsplit(data$medicines_wo_prescription.list, ",") 
data$medicines_wo_prescription.NSAIDs_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "1" %in% x }), 
                                                         1, 
                                                         0)
data$medicines_wo_prescription.proton_pump_inhibitors_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "2" %in% x }), 
                                                                         1, 
                                                                         0)
data$medicines_wo_prescription.hypericum_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "3" %in% x }), 
                                                            1, 
                                                            0)
data$medicines_wo_prescription.red_yeast_rice_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "4" %in% x }), 
                                                                 1, 
                                                                 0)
data$medicines_wo_prescription.multivitamin_dietary_supplement_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "5" %in% x }), 
                                                                                  1, 
                                                                                  0)
data$medicines_wo_prescription.other_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "6" %in% x }), 
                                                        1, 
                                                        0)
data$medicines_wo_prescription.none_1 <- ifelse(lapply(medicines_wo_prescription.list, function(x) { "7" %in% x }), 
                                                       1, 
                                                       0)

table(data$medicines_wo_prescription.NSAIDs_1)
prop.table(table(data$medicines_wo_prescription.NSAIDs_1))
table(data$medicines_wo_prescription.proton_pump_inhibitors_1)
prop.table(table(data$medicines_wo_prescription.proton_pump_inhibitors_1))
table(data$medicines_wo_prescription.hypericum_1)
prop.table(table(data$medicines_wo_prescription.hypericum_1))
table(data$medicines_wo_prescription.red_yeast_rice_1)
prop.table(table(data$medicines_wo_prescription.red_yeast_rice_1))
table(data$medicines_wo_prescription.multivitamin_dietary_supplement_1)
prop.table(table(data$medicines_wo_prescription.multivitamin_dietary_supplement_1))
table(data$medicines_wo_prescription.other_1)
prop.table(table(data$medicines_wo_prescription.other_1))
table(data$medicines_wo_prescription.none_1)
prop.table(table(data$medicines_wo_prescription.none_1))

### Patient-reported medicine use ---- 

names(data)[names(data) == "A21b"] <- "patient_reported_medicine_use.yes_1"
data$patient_reported_medicine_use.yes_1 <- ifelse(data$patient_reported_medicine_use.yes_1 == 2, 
                                                   0, 
                                                   data$patient_reported_medicine_use.yes_1)

table(data$patient_reported_medicine_use.yes_1, exclude = NULL)
prop.table(table(data$patient_reported_medicine_use.yes_1, exclude = NULL))

### Pill box ---- 

names(data)[names(data) == "A4b"] <- "pill_box.yes_1"
data$pill_box.yes_1 <- ifelse(data$pill_box.yes_1 == 2,
                              0, 
                              data$pill_box.yes_1)

table(data$pill_box.yes_1, exclude = NULL)
prop.table(table(data$pill_box.yes_1, exclude = NULL))

### Allergy to medicine ----

names(data)[names(data) == "A13b"] <- "allergy_medicine.yes_1"
data$allergy_medicine.yes_1 <- ifelse(data$allergy_medicine.yes_1 == 2, 
                                      0, 
                                      data$allergy_medicine.yes_1)

table(data$allergy_medicine.yes_1, exclude = NULL)
prop.table(table(data$allergy_medicine.yes_1, exclude = NULL))

names(data)[names(data) == "A20b"] <- "allergy_medicine.symptoms"
#View(as.data.frame(subset(data, !is.na(allergy_medicine.symptoms))$allergy_medicine.symptoms))

## Hospital use ----

### Maastricht University Medical Centre+ ----

data$out_specialty_current_MUMC <- data$SPEC
data$out_specialty_current_MUMC <- factor(data$out_specialty_current_MUMC, 
                                          levels = c("CARDIO", 
                                                     "CHI", 
                                                     "CHI HPB",
                                                     "CHI TRAU",
                                                     "CHI VAAT",
                                                     "DERMA",
                                                     "INTE VASC",
                                                     "INTERN-D", 
                                                     "INTERN ALG", 
                                                     "INTERN GER",
                                                     "KNO-ALG",
                                                     "MDL-IBD",
                                                     "MDL-Lever",
                                                     "MDL NGM",
                                                     "NEU VASC",
                                                     "NEURO",
                                                     "ORTHO",
                                                     "REUM JICHT",
                                                     "REUM SPA",
                                                     "REUMA",
                                                     "UROL FU",
                                                     "UROL ONCO"), 
                                          labels = c("cardiology",
                                                     "surgery",
                                                     "surgery: hepato-pancreato-biliary", 
                                                     "surgery: trauma",
                                                     "surgery: vascular", 
                                                     "dermatology",
                                                     "internal medicine: vascular",
                                                     "internal medicine: D", 
                                                     "internal medicine: general",
                                                     "internal medicine: geriatrics",
                                                     "otorhinolaryngology",
                                                     "gastroenterology: inflammatory bowel diseases",
                                                     "gastroenterology: liver",
                                                     "gastroenterology: neuro",
                                                     "neurology: vascular",
                                                     "neurology",
                                                     "orthopedics",
                                                     "rheumatology: gout",
                                                     "rheumatology: spondyloarthritis",
                                                     "rheumatology",
                                                     "urology: functional",
                                                     "urology: oncology"))
data$out_specialty_current_MUMC.cardiology_1 <- ifelse(data$out_specialty_current_MUMC == "cardiology", 
                                                       1, 
                                                       0)
data$out_specialty_current_MUMC.surgery_1 <- ifelse(data$out_specialty_current_MUMC == "surgery", 
                                                    1, 
                                                    0)
data$out_specialty_current_MUMC.surgery_hpb_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: hepato-pancreato-biliary", 
                                                        1, 
                                                        0)
data$out_specialty_current_MUMC.surgery_trauma_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: trauma", 
                                                           1, 
                                                           0)
data$out_specialty_current_MUMC.surgery_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "surgery: vascular", 
                                                             1, 
                                                             0)
data$out_specialty_current_MUMC.dermatology_1 <- ifelse(data$out_specialty_current_MUMC == "dermatology", 
                                                        1, 
                                                        0)
data$out_specialty_current_MUMC.internal_medicine_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: vascular", 
                                                                       1, 
                                                                       0)
data$out_specialty_current_MUMC.internal_medicine_D_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: D", 
                                                                1, 
                                                                0)
data$out_specialty_current_MUMC.internal_medicine_general_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: general", 
                                                                      1, 
                                                                      0)
data$out_specialty_current_MUMC.internal_medicine_geriatrics_1 <- ifelse(data$out_specialty_current_MUMC == "internal medicine: geriatrics", 
                                                                         1, 
                                                                         0)
data$out_specialty_current_MUMC.otorhinolaryngology_1 <- ifelse(data$out_specialty_current_MUMC == "otorhinolaryngology",
                                                                1,
                                                                0)
data$out_specialty_current_MUMC.gastroenterology_ibd_1 <- ifelse(data$out_specialty_current_MUMC == "gastroenterology: inflammatory bowel diseases",
                                                                 1,
                                                                 0)
data$out_specialty_current_MUMC.gastroenterology_liver_1 <- ifelse(data$out_specialty_current_MUMC == "gastroenterology: liver",
                                                                   1,
                                                                   0)
data$out_specialty_current_MUMC.gastroenterology_neuro_1 <- ifelse(data$out_specialty_current_MUMC == "gastroenterology: neuro",
                                                                   1,
                                                                   0)
data$out_specialty_current_MUMC.neurology_vascular_1 <- ifelse(data$out_specialty_current_MUMC == "neurology: vascular",
                                                               1,
                                                               0)
data$out_specialty_current_MUMC.neurology_1 <- ifelse(data$out_specialty_current_MUMC == "neurology", 
                                                      1, 
                                                      0)
data$out_specialty_current_MUMC.orthopedics_1 <- ifelse(data$out_specialty_current_MUMC == "orthopedics",
                                                        1,
                                                        0)
data$out_specialty_current_MUMC.rheumatology_gout_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology: gout",
                                                              1,
                                                              0)
data$out_specialty_current_MUMC.rheumatology_spondyloarthritis_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology: spondyloarthritis",
                                                                           1,
                                                                           0)
data$out_specialty_current_MUMC.rheumatology_1 <- ifelse(data$out_specialty_current_MUMC == "rheumatology",
                                                         1,
                                                         0)
data$out_specialty_current_MUMC.urology_functional_1 <- ifelse(data$out_specialty_current_MUMC == "urology: functional",
                                                               1,
                                                               0)
data$out_specialty_current_MUMC.urology_oncology_1 <- ifelse(data$out_specialty_current_MUMC == "urology: oncology",
                                                             1,
                                                             0)

table(data$out_specialty_current_MUMC, exclude = NULL)
prop.table(table(data$out_specialty_current_MUMC, exclude = NULL))

data$out_specialty_current_MUMC.surgery_composite_1 <- ifelse((data$out_specialty_current_MUMC.surgery_1 == 1) 
                                                              | 
                                                              (data$out_specialty_current_MUMC.surgery_hpb_1 == 1) 
                                                              | 
                                                              (data$out_specialty_current_MUMC.surgery_trauma_1 == 1) 
                                                              | 
                                                              (data$out_specialty_current_MUMC.surgery_vascular_1 == 1), 
                                                              1, 
                                                              0)
data$out_specialty_current_MUMC.internal_medicine_composite_1 <- ifelse((data$out_specialty_current_MUMC.internal_medicine_vascular_1 == 1)
                                                                        |
                                                                        (data$out_specialty_current_MUMC.internal_medicine_D_1 == 1) 
                                                                        | 
                                                                        (data$out_specialty_current_MUMC.internal_medicine_general_1 == 1) 
                                                                        | 
                                                                        (data$out_specialty_current_MUMC.internal_medicine_geriatrics_1 == 1),
                                                                        1, 
                                                                        0)
data$out_specialty_current_MUMC.gastroenterology_composite_1 <- ifelse((data$out_specialty_current_MUMC.gastroenterology_ibd_1 == 1)
                                                                       |
                                                                       (data$out_specialty_current_MUMC.gastroenterology_liver_1 == 1)
                                                                       |
                                                                       (data$out_specialty_current_MUMC.gastroenterology_neuro_1 == 1),
                                                                       1,
                                                                       0)
data$out_specialty_current_MUMC.neurology_composite_1 <- ifelse((data$out_specialty_current_MUMC.neurology_vascular_1 == 1) 
                                                                | 
                                                                (data$out_specialty_current_MUMC.neurology_1 == 1),
                                                                1, 
                                                                0)
data$out_specialty_current_MUMC.rheumatology_composite_1 <- ifelse((data$out_specialty_current_MUMC.rheumatology_gout_1 == 1)
                                                                   |
                                                                   (data$out_specialty_current_MUMC.rheumatology_spondyloarthritis_1 == 1)
                                                                   |   
                                                                   (data$out_specialty_current_MUMC.rheumatology_1 == 1),
                                                                   1,
                                                                   0)
data$out_specialty_current_MUMC.urology_composite_1 <- ifelse((data$out_specialty_current_MUMC.urology_functional_1 == 1) 
                                                              | 
                                                              (data$out_specialty_current_MUMC.urology_oncology_1 == 1),
                                                              1,
                                                              0)

data$out_specialty_current_MUMC.group_A_1 <- ifelse((data$out_specialty_current_MUMC.cardiology_1 == 1)
                                                    |
                                                    (data$out_specialty_current_MUMC.internal_medicine_composite_1 == 1)
                                                    |
                                                    (data$out_specialty_current_MUMC.gastroenterology_composite_1 == 1) 
                                                    |
                                                    (data$out_specialty_current_MUMC.neurology_composite_1 == 1)
                                                    |
                                                    (data$out_specialty_current_MUMC.rheumatology_composite_1 == 1),
                                                    1, 
                                                    0)
data$out_specialty_current_MUMC.group_B_1 <- ifelse((data$out_specialty_current_MUMC.surgery_composite_1 == 1)
                                                    |
                                                    (data$out_specialty_current_MUMC.orthopedics_1 == 1)
                                                    |
                                                    (data$out_specialty_current_MUMC.urology_oncology_1 == 1),
                                                    1, 
                                                    0)
data$out_specialty_current_MUMC.group_C_1 <- ifelse((data$out_specialty_current_MUMC.dermatology_1 == 1)
                                                    |
                                                    (data$out_specialty_current_MUMC.otorhinolaryngology_1 == 1)
                                                    |
                                                    (data$out_specialty_current_MUMC.urology_functional_1 == 1),
                                                    1, 
                                                    0)

names(data)[names(data) == "C3a"] <- "n_visits_out_current_specialty_36m_MUMC.ord"
data$n_visits_out_current_specialty_36m_MUMC.ord <- factor(data$n_visits_out_current_specialty_36m_MUMC.ord,
                                                               levels = c("1",
                                                                          "2",
                                                                          "3"),
                                                               labels = c("1",
                                                                          "2",
                                                                          "3+"))
data$n_visits_out_current_specialty_36m_MUMC.1_1 <- ifelse(data$n_visits_out_current_specialty_36m_MUMC.ord == 1,
                                                           1,
                                                           0)
data$n_visits_out_current_specialty_36m_MUMC.2_1 <- ifelse(data$n_visits_out_current_specialty_36m_MUMC.ord == 2,
                                                           1,
                                                           0)
data$n_visits_out_current_specialty_36m_MUMC.3_or_above_1 <- ifelse(data$n_visits_out_current_specialty_36m_MUMC.ord == 3,
                                                                    1,
                                                                    0)

table(data$n_visits_out_current_specialty_36m_MUMC.ord, exclude = NULL)
prop.table(table(data$n_visits_out_current_specialty_36m_MUMC.ord, exclude = NULL))

names(data)[names(data) == "A14a"] <- "n_visits_out_all_12m_MUMC.ord"
data$n_visits_out_all_12m_MUMC.ord <- factor(data$n_visits_out_all_12m_MUMC.ord,
                                             levels = c(1, 
                                                        2, 
                                                        3, 
                                                        4), 
                                             labels = c("0", 
                                                        "1", 
                                                        "2-5", 
                                                        "> 5"))
data$n_visits_out_all_12m_MUMC.0_1 <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "0",
                                             1, 
                                             0)
data$n_visits_out_all_12m_MUMC.1_1 <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "1", 
                                             1, 
                                             0)
data$n_visits_out_all_12m_MUMC.2_to_5_1 <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "2-5",
                                                  1, 
                                                  0)
data$n_visits_out_all_12m_MUMC.above_5_1 <- ifelse(data$n_visits_out_all_12m_MUMC.ord == "> 5",
                                                   1, 
                                                   0)

table(data$n_visits_out_all_12m_MUMC.ord, exclude = NULL)
prop.table(table(data$n_visits_out_all_12m_MUMC.ord, exclude = NULL))

inout_specialty_12m_MUMC.list <- strsplit(data$A150a, ",") 
data$inout_specialty_12m_MUMC.cardiology_1 <-ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "1" %in% x }), 
                                                    1, 
                                                    0) 
data$inout_specialty_12m_MUMC.urology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "2" %in% x }),
                                                  1, 
                                                  0)  
data$inout_specialty_12m_MUMC.psychiatry_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "3" %in% x }), 
                                                     1, 
                                                     0) 
data$inout_specialty_12m_MUMC.otorhinolaryngology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "4" %in% x }),
                                                              1, 
                                                              0) 
data$inout_specialty_12m_MUMC.ophthalmology_1 <-ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "5" %in% x }),
                                                       1, 
                                                       0) 
data$inout_specialty_12m_MUMC.internal_medicine_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "6" %in% x }),
                                                            1, 
                                                            0) 
data$inout_specialty_12m_MUMC.surgery_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "7" %in% x }),
                                                  1, 
                                                  0) 
data$inout_specialty_12m_MUMC.orthopedics_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "8" %in% x }),
                                                      1, 
                                                      0) 
data$inout_specialty_12m_MUMC.plastic_surgery_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "9" %in% x }),
                                                          1, 
                                                          0) 
data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "10" %in% x }),
                                                                     1,
                                                                     0) 
data$inout_specialty_12m_MUMC.neurology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "11" %in% x }),
                                                    1, 
                                                    0) 
data$inout_specialty_12m_MUMC.dermatology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "12" %in% x }),
                                                      1, 
                                                      0) 
data$inout_specialty_12m_MUMC.gastroenterology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "13" %in% x }),
                                                           1, 
                                                           0) 
data$inout_specialty_12m_MUMC.pneumology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "14" %in% x }),
                                                     1, 
                                                     0) 
data$inout_specialty_12m_MUMC.rheumatology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "15" %in% x }),
                                                       1, 
                                                       0) 
data$inout_specialty_12m_MUMC.do_not_select_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "16" %in% x }), 
                                                        1, 
                                                        0) 
data$inout_specialty_12m_MUMC.pediatrics_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "17" %in% x }), 
                                                     1, 
                                                     0) 
data$inout_specialty_12m_MUMC.anesthesiology_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "18" %in% x }), 
                                                         1, 
                                                         0) 
data$inout_specialty_12m_MUMC.other_1 <- ifelse(lapply(inout_specialty_12m_MUMC.list, function(x) { "19" %in% x }),
                                                1, 
                                                0) 

table(data$inout_specialty_12m_MUMC.cardiology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.cardiology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.urology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.urology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.psychiatry_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.psychiatry_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.otorhinolaryngology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.otorhinolaryngology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.ophthalmology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.ophthalmology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.internal_medicine_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.internal_medicine_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.surgery_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.surgery_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.orthopedics_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.orthopedics_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.plastic_surgery_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.plastic_surgery_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.neurology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.neurology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.dermatology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.dermatology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.gastroenterology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.gastroenterology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.pneumology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.pneumology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.rheumatology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.rheumatology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.do_not_select_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.do_not_select_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.pediatrics_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.pediatrics_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.anesthesiology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.anesthesiology_1, exclude = NULL))
table(data$inout_specialty_12m_MUMC.other_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_MUMC.other_1, exclude = NULL))

data$inout_specialty_12m_MUMC.surgery_composite_1 <- ifelse((data$inout_specialty_12m_MUMC.surgery_1 == 1) 
                                                            | 
                                                            (data$inout_specialty_12m_MUMC.plastic_surgery_1 == 1),
                                                            1,
                                                            0)

data$specialty_MUMC.cardiology_1 <- ifelse((data$out_specialty_current_MUMC.cardiology_1 == 1)
                                           |
                                           (data$inout_specialty_12m_MUMC.cardiology_1 == 1),
                                           1,
                                           0)
data$specialty_MUMC.surgery_composite_1 <- ifelse((data$out_specialty_current_MUMC.surgery_composite_1 == 1)
                                                  |
                                                  (data$inout_specialty_12m_MUMC.surgery_composite_1 == 1),
                                                  1,
                                                  0)
data$specialty_MUMC.dermatology_1 <- ifelse((data$out_specialty_current_MUMC.dermatology_1 == 1)
                                            |
                                            (data$inout_specialty_12m_MUMC.dermatology_1 == 1),
                                            1,
                                            0)
data$specialty_MUMC.internal_medicine_composite_1 <- ifelse((data$out_specialty_current_MUMC.internal_medicine_composite_1 == 1)
                                                            |
                                                            (data$inout_specialty_12m_MUMC.internal_medicine_1 == 1),
                                                            1,
                                                            0)
data$specialty_MUMC.otorhinolaryngology_1 <- ifelse((data$out_specialty_current_MUMC.otorhinolaryngology_1 == 1)
                                                    |
                                                    (data$inout_specialty_12m_MUMC.otorhinolaryngology_1 == 1),
                                                    1,
                                                    0)
data$specialty_MUMC.gastroenterology_composite_1 <- ifelse((data$out_specialty_current_MUMC.gastroenterology_composite_1 == 1)
                                                           |
                                                           (data$inout_specialty_12m_MUMC.gastroenterology_1 == 1),
                                                           1,
                                                           0)
data$specialty_MUMC.neurology_composite_1 <- ifelse((data$out_specialty_current_MUMC.neurology_composite_1 == 1)
                                                    |
                                                    (data$inout_specialty_12m_MUMC.neurology_1 == 1),
                                                    1,
                                                    0)
data$specialty_MUMC.orthopedics_1 <- ifelse((data$out_specialty_current_MUMC.orthopedics_1 == 1)
                                            |
                                            (data$inout_specialty_12m_MUMC.orthopedics_1 == 1),
                                            1,
                                            0)
data$specialty_MUMC.rheumatology_composite_1 <- ifelse((data$out_specialty_current_MUMC.rheumatology_composite_1 == 1)
                                                       |
                                                       (data$inout_specialty_12m_MUMC.rheumatology_1 == 1),
                                                       1,
                                                       0)
data$specialty_MUMC.urology_composite_1 <- ifelse((data$out_specialty_current_MUMC.urology_composite_1 == 1)
                                                  |
                                                  (data$inout_specialty_12m_MUMC.urology_1 == 1),
                                                  1,
                                                  0)
data$n_specialty_MUMC <- data$specialty_MUMC.cardiology_1 +
                         data$specialty_MUMC.surgery_composite_1 +
                         data$specialty_MUMC.dermatology_1 +
                         data$specialty_MUMC.internal_medicine_composite_1 +
                         data$specialty_MUMC.otorhinolaryngology_1 +
                         data$specialty_MUMC.gastroenterology_composite_1 +
                         data$specialty_MUMC.neurology_composite_1 +
                         data$specialty_MUMC.orthopedics_1 +
                         data$specialty_MUMC.rheumatology_composite_1 +
                         data$specialty_MUMC.urology_composite_1 +
                         data$inout_specialty_12m_MUMC.psychiatry_1 +
                         data$inout_specialty_12m_MUMC.ophthalmology_1 +
                         data$inout_specialty_12m_MUMC.pneumology_1 +
                         data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 +
                         data$inout_specialty_12m_MUMC.pediatrics_1 +
                         data$inout_specialty_12m_MUMC.anesthesiology_1 +
                         data$inout_specialty_12m_MUMC.other_1

table(data$n_specialty_MUMC, exclude = NULL)
prop.table(table(data$n_specialty_MUMC, exclude = NULL))
summary(data$n_specialty_MUMC)
sd(data$n_specialty_MUMC)
ggplot(data, aes(x = n_specialty_MUMC)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 12, 1),
                     labels = seq(0, 12, 1)) +
  theme_minimal() 

data$n_specialty_MUMC.centered <- data$n_specialty_MUMC - mean(data$n_specialty_MUMC)
data$n_specialty_MUMC.std <- data$n_specialty_MUMC.centered / sd(data$n_specialty_MUMC)

names(data)[names(data) == "A16a"] <- "in_all_12m_MUMC.yes_1"
data$in_all_12m_MUMC.yes_1 <- ifelse(data$in_all_12m_MUMC.yes_1 == 2, 
                                     0, 
                                     data$in_all_12m_MUMC.yes_1)

table(data$in_all_12m_MUMC.yes_1, exclude = NULL)
prop.table(table(data$in_all_12m_MUMC.yes_1, exclude = NULL))

names(data)[names(data) == "A18a"] <- "n_ER_12m_MUMC"

table(data$n_ER_12m_MUMC, exclude = NULL)
prop.table(table(data$n_ER_12m_MUMC, exclude = NULL))
summary(data$n_ER_12m_MUMC)
sd(data$n_ER_12m_MUMC)
ggplot(data, aes(x = n_ER_12m_MUMC)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, 1),
                     labels = seq(0, 20, 1)) +
  theme_minimal() 

data$n_ER_12m_MUMC.centered <- data$n_ER_12m_MUMC - mean(data$n_ER_12m_MUMC)
data$n_ER_12m_MUMC.std <- data$n_ER_12m_MUMC.centered / sd(data$n_ER_12m_MUMC)

### External hospital ----

names(data)[names(data) == "A140b"] <- "inoutER_12m_external_hospital.yes_1"
data$inoutER_12m_external_hospital.yes_1 <- ifelse(data$inoutER_12m_external_hospital.yes_1 == 2,
                                                   0, 
                                                   data$inoutER_12m_external_hospital.yes_1) 

table(data$inoutER_12m_external_hospital.yes_1, exclude = NULL)
prop.table(table(data$inoutER_12m_external_hospital.yes_1, exclude = NULL))

inout_specialty_12m_external_hospital.list <- strsplit(data$A150b, ",") 
data$inout_specialty_12m_external_hospital.cardiology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "1" %in% x }),
                                                                  1, 
                                                                  0) 
data$inout_specialty_12m_external_hospital.urology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "2" %in% x }),
                                                               1, 
                                                               0) 
data$inout_specialty_12m_external_hospital.psychiatry_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "3" %in% x }),
                                                                  1, 
                                                                  0) 
data$inout_specialty_12m_external_hospital.otorhinolaryngology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "4" %in% x }),
                                                                           1, 
                                                                           0) 
data$inout_specialty_12m_external_hospital.ophthalmology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "5" %in% x }), 
                                                                     1, 
                                                                     0) 
data$inout_specialty_12m_external_hospital.internal_medicine_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "6" %in% x }), 
                                                                         1, 
                                                                         0) 
data$inout_specialty_12m_external_hospital.surgery_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "7" %in% x }),
                                                               1, 
                                                               0) 
data$inout_specialty_12m_external_hospital.orthopedics_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "8" %in% x }), 
                                                                   1, 
                                                                   0) 
data$inout_specialty_12m_external_hospital.plastic_surgery_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "9" %in% x }),
                                                                       1, 
                                                                       0) 
data$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "10" %in% x }),
                                                                                  1, 
                                                                                  0) 
data$inout_specialty_12m_external_hospital.neurology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "11" %in% x }),
                                                                 1, 
                                                                 0) 
data$inout_specialty_12m_external_hospital.dermatology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "12" %in% x }),
                                                                   1, 
                                                                   0) 
data$inout_specialty_12m_external_hospital.gastroenterology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "13" %in% x }),
                                                                        1, 
                                                                        0) 
data$inout_specialty_12m_external_hospital.pneumology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "14" %in% x }), 
                                                                  1, 
                                                                  0) 
data$inout_specialty_12m_external_hospital.rheumatology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "15" %in% x }),
                                                                    1, 
                                                                    0) 
data$inout_specialty_12m_external_hospital.do_not_select_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "16" %in% x }), 
                                                                     1, 
                                                                     0) 
data$inout_specialty_12m_external_hospital.pediatrics_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "17" %in% x }), 
                                                                  1, 
                                                                  0) 
data$inout_specialty_12m_external_hospital.anesthesiology_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "18" %in% x }),
                                                                      1, 
                                                                      0) 
data$inout_specialty_12m_external_hospital.other_1 <- ifelse(lapply(inout_specialty_12m_external_hospital.list, function(x) { "19" %in% x }),
                                                             1, 
                                                             0) 

table(data$inout_specialty_12m_external_hospital.cardiology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.cardiology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.urology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.urology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.psychiatry_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.psychiatry_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.otorhinolaryngology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.otorhinolaryngology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.ophthalmology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.ophthalmology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.internal_medicine_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.internal_medicine_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.surgery_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.surgery_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.orthopedics_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.orthopedics_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.plastic_surgery_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.plastic_surgery_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.neurology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.neurology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.dermatology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.dermatology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.gastroenterology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.gastroenterology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.pneumology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.pneumology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.rheumatology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.rheumatology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.do_not_select_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.do_not_select_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.pediatrics_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.pediatrics_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.anesthesiology_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.anesthesiology_1, exclude = NULL))
table(data$inout_specialty_12m_external_hospital.other_1, exclude = NULL)
prop.table(table(data$inout_specialty_12m_external_hospital.other_1, exclude = NULL))

data$inout_specialty_12m_external_hospital.surgery_composite_1 <- ifelse((data$inout_specialty_12m_external_hospital.surgery_1 == 1)
                                                                         |
                                                                         (data$inout_specialty_12m_external_hospital.plastic_surgery_1 == 1),
                                                                         1,
                                                                         0)

names(data)[names(data) == "A16b"] <- "in_all_12m_external_hospital.yes_1"
data$in_all_12m_external_hospital.yes_1 <- ifelse(data$in_all_12m_external_hospital.yes_1 == 2, 
                                                  0, 
                                                  data$in_all_12m_MUMC.yes_1)  

table(data$in_all_12m_external_hospital.yes_1, exclude = NULL)
prop.table(table(data$in_all_12m_external_hospital.yes_1, exclude = NULL))

names(data)[names(data) == "A18b"] <- "n_ER_12m_external_hospital" 

table(data$n_ER_12m_external_hospital, exclude = NULL)
prop.table(table(data$n_ER_12m_external_hospital, exclude = NULL))
summary(data$n_ER_12m_external_hospital)
sd(data$n_ER_12m_external_hospital)
ggplot(data, aes(x = n_ER_12m_external_hospital)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 15, 1),
                     labels = seq(0, 15, 1)) +
  theme_minimal()

data$n_ER_12m_external_hospital.centered <- data$n_ER_12m_external_hospital - mean(data$n_ER_12m_external_hospital)
data$n_ER_12m_external_hospital.std <- data$n_ER_12m_external_hospital.centered / sd(data$n_ER_12m_external_hospital)

### Maastricht University Medical Centre+ & external hospital ----

data$specialty_MUMC_external_hospital.cardiology_1 <- ifelse((data$specialty_MUMC.cardiology_1 == 1)
                                                             |
                                                             (data$inout_specialty_12m_external_hospital.cardiology_1 == 1),
                                                             1,
                                                             0)
data$specialty_MUMC_external_hospital.surgery_composite_1 <- ifelse((data$specialty_MUMC.surgery_composite_1 == 1)
                                                                    |
                                                                    (data$inout_specialty_12m_external_hospital.surgery_composite_1 == 1),
                                                                    1,
                                                                    0)
data$specialty_MUMC_external_hospital.dermatology_1 <- ifelse((data$specialty_MUMC.dermatology_1 == 1)
                                                              |
                                                              (data$inout_specialty_12m_external_hospital.dermatology_1 == 1),
                                                              1,
                                                              0)
data$specialty_MUMC_external_hospital.internal_medicine_composite_1 <- ifelse((data$specialty_MUMC.internal_medicine_composite_1 == 1)
                                                                              |
                                                                              (data$inout_specialty_12m_external_hospital.internal_medicine_1 == 1),
                                                                              1,
                                                                              0)
data$specialty_MUMC_external_hospital.otorhinolaryngology_1 <- ifelse((data$specialty_MUMC.otorhinolaryngology_1 == 1)
                                                                      |
                                                                      (data$inout_specialty_12m_external_hospital.otorhinolaryngology_1 == 1),
                                                                      1,
                                                                      0)
data$specialty_MUMC_external_hospital.gastroenterology_composite_1 <- ifelse((data$specialty_MUMC.gastroenterology_composite_1 == 1)
                                                                             |
                                                                             (data$inout_specialty_12m_external_hospital.gastroenterology_1 == 1),
                                                                             1,
                                                                             0)
data$specialty_MUMC_external_hospital.neurology_composite_1 <- ifelse((data$specialty_MUMC.neurology_composite_1 == 1)
                                                                      |
                                                                      (data$inout_specialty_12m_external_hospital.neurology_1 == 1),
                                                                      1,
                                                                      0)
data$specialty_MUMC_external_hospital.orthopedics_1 <- ifelse((data$specialty_MUMC.orthopedics_1 == 1)
                                                              |
                                                              (data$inout_specialty_12m_external_hospital.orthopedics_1 == 1),
                                                              1,
                                                              0)
data$specialty_MUMC_external_hospital.rheumatology_composite_1 <- ifelse((data$specialty_MUMC.rheumatology_composite_1 == 1)
                                                                         |
                                                                         (data$inout_specialty_12m_external_hospital.rheumatology_1 == 1),
                                                                         1,
                                                                         0)
data$specialty_MUMC_external_hospital.urology_composite_1 <- ifelse((data$specialty_MUMC.urology_composite_1 == 1)
                                                                    |
                                                                    (data$inout_specialty_12m_external_hospital.urology_1 == 1),
                                                                    1,
                                                                    0)
data$specialty_MUMC_external_hospital.psychiatry_1 <- ifelse((data$inout_specialty_12m_MUMC.psychiatry_1 == 1)
                                                             |
                                                             (data$inout_specialty_12m_external_hospital.psychiatry_1 == 1),
                                                             1,
                                                             0)
data$specialty_MUMC_external_hospital.ophthalmology_1 <- ifelse((data$inout_specialty_12m_MUMC.ophthalmology_1 == 1)
                                                                |
                                                                (data$inout_specialty_12m_external_hospital.ophthalmology_1 == 1),
                                                                1,
                                                                0)
data$specialty_MUMC_external_hospital.pneumology_1 <- ifelse((data$inout_specialty_12m_MUMC.pneumology_1 == 1)
                                                             |
                                                             (data$inout_specialty_12m_external_hospital.pneumology_1 == 1),
                                                             1,
                                                             0)
data$specialty_MUMC_external_hospital.obstetrics_and_gynaecology_1 <- ifelse((data$inout_specialty_12m_MUMC.obstetrics_and_gynaecology_1 == 1)
                                                                             |
                                                                             (data$inout_specialty_12m_external_hospital.obstetrics_and_gynaecology_1 == 1),
                                                                             1,
                                                                             0)
data$specialty_MUMC_external_hospital.pediatrics_1 <- ifelse((data$inout_specialty_12m_MUMC.pediatrics_1 == 1)
                                                             |
                                                             (data$inout_specialty_12m_external_hospital.pediatrics_1 == 1),
                                                             1,
                                                             0)
data$specialty_MUMC_external_hospital.anesthesiology_1 <- ifelse((data$inout_specialty_12m_MUMC.anesthesiology_1 == 1)
                                                                 |
                                                                 (data$inout_specialty_12m_external_hospital.anesthesiology_1 == 1),
                                                                 1,
                                                                 0)
data$specialty_MUMC_external_hospital.other_1 <- ifelse((data$inout_specialty_12m_MUMC.other_1 == 1)
                                                        |
                                                        (data$inout_specialty_12m_external_hospital.other_1 == 1),
                                                        1,
                                                        0)
data$n_specialty_MUMC_external_hospital <- data$specialty_MUMC_external_hospital.cardiology_1 +
                                           data$specialty_MUMC_external_hospital.surgery_composite_1 +
                                           data$specialty_MUMC_external_hospital.dermatology_1 +
                                           data$specialty_MUMC_external_hospital.internal_medicine_composite_1 +
                                           data$specialty_MUMC_external_hospital.otorhinolaryngology_1 +
                                           data$specialty_MUMC_external_hospital.gastroenterology_composite_1 +
                                           data$specialty_MUMC_external_hospital.neurology_composite_1 +
                                           data$specialty_MUMC_external_hospital.orthopedics_1 +
                                           data$specialty_MUMC_external_hospital.rheumatology_composite_1 +
                                           data$specialty_MUMC_external_hospital.urology_composite_1 +
                                           data$specialty_MUMC_external_hospital.psychiatry_1 +
                                           data$specialty_MUMC_external_hospital.ophthalmology_1 +
                                           data$specialty_MUMC_external_hospital.pneumology_1 +
                                           data$specialty_MUMC_external_hospital.obstetrics_and_gynaecology_1 +
                                           data$specialty_MUMC_external_hospital.pediatrics_1 +
                                           data$specialty_MUMC_external_hospital.anesthesiology_1 +
                                           data$specialty_MUMC_external_hospital.other_1

table(data$n_specialty_MUMC_external_hospital, exclude = NULL)
prop.table(table(data$n_specialty_MUMC_external_hospital, exclude = NULL))
summary(data$n_specialty_MUMC_external_hospital)
sd(data$n_specialty_MUMC_external_hospital)
ggplot(data, aes(x = n_specialty_MUMC_external_hospital)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 12, 1),
                     labels = seq(0, 12, 1)) +
  theme_minimal() 

data$n_specialty_MUMC_external_hospital.centered <- data$n_specialty_MUMC_external_hospital - mean(data$n_specialty_MUMC_external_hospital)
data$n_specialty_MUMC_external_hospital.std <- data$n_specialty_MUMC_external_hospital.centered / sd(data$n_specialty_MUMC_external_hospital)

## Consultation ----

names(data)[names(data) == "B10c"] <- "medicine_prescribed_during_consult.yes_1"
data$medicine_prescribed_during_consult.yes_1 <- ifelse(is.na(data$medicine_prescribed_during_consult.yes_1),
                                                        0,
                                                        data$medicine_prescribed_during_consult.yes_1)

data$medicine_prescribed_during_consult.yes_1 <- ifelse(data$medicine_prescribed_during_consult.yes_1 == 2,
                                                        0,
                                                        data$medicine_prescribed_during_consult.yes_1) 

table(data$medicine_prescribed_during_consult.yes_1, exclude = NULL)
prop.table(table(data$medicine_prescribed_during_consult.yes_1, exclude = NULL))

names(data)[names(data) == "B11c"] <- "medicine_prescribed_during_consult.name"
#View(as.data.frame(subset(data, !is.na(medicine_prescribed_during_consult.name))$medicine_prescribed_during_consult.name))

names(data)[names(data) == "B12c"] <- "medicine_prescribed_during_consult.new_1"
data$medicine_prescribed_during_consult.new_1 <- ifelse(data$medicine_prescribed_during_consult.new_1 == 2,
                                                        0,
                                                        data$medicine_prescribed_during_consult.new_1)

table(data$medicine_prescribed_during_consult.new_1, exclude = NULL)
prop.table(table(data$medicine_prescribed_during_consult.new_1, exclude = NULL))

names(data)[names(data) == "B20c"] <- "medicine_stopped_during_consult.yes_1"
data$medicine_stopped_during_consult.yes_1 <- ifelse(is.na(data$medicine_stopped_during_consult.yes_1),
                                                     0,
                                                     data$medicine_stopped_during_consult.yes_1) 
data$medicine_stopped_during_consult.yes_1 <- ifelse(data$medicine_stopped_during_consult.yes_1 == 2,
                                                     0,
                                                     data$medicine_stopped_during_consult.yes_1) 

table(data$medicine_stopped_during_consult.yes_1, exclude = NULL)
prop.table(table(data$medicine_stopped_during_consult.yes_1, exclude = NULL))

names(data)[names(data) == "B21c"] <- "medicine_stopped_during_consult.name"
#View(as.data.frame(subset(data, !is.na(medicine_stopped_during_consult.name))$medicine_stopped_during_consult.name))

names(data)[names(data) == "B13c"] <- "non_medicine_intervention_during_consult.yes_1"
data$non_medicine_intervention_during_consult.yes_1 <- ifelse(data$non_medicine_intervention_during_consult.yes_1 == 2,
                                                              0,
                                                              data$non_medicine_intervention_during_consult.yes_1) 

table(data$non_medicine_intervention_during_consult.yes_1, exclude = NULL)
prop.table(table(data$non_medicine_intervention_during_consult.yes_1, exclude = NULL))

## Outcome ----

### Review by pharmacy technician ----

names(data)[names(data) == "S1c"] <- "warnings.yes_1"
data$warnings.yes_1 <- ifelse(is.na(data$warnings.yes_1), 
                              0, 
                              data$warnings.yes_1) 
data$warnings.yes_1 <- ifelse(data$warnings.yes_1 == 1, 
                              1, 
                              0)

table(data$warnings.yes_1, exclude = NULL)
prop.table(table(data$warnings.yes_1, exclude = NULL))

names(data)[names(data) == "S2c"] <- "medicines_interaction.yes_1"
data$medicines_interaction.yes_1 <- ifelse(is.na(data$medicines_interaction.yes_1),
                                           0, 
                                           data$medicines_interaction.yes_1) 
data$medicines_interaction.yes_1 <- ifelse(data$medicines_interaction.yes_1 == 1,
                                           1, 
                                           0)

table(data$medicines_interaction.yes_1, exclude = NULL)
prop.table(table(data$medicines_interaction.yes_1, exclude = NULL))

names(data)[names(data) == "S31c"] <- "medicines_interaction_1.G_number"

table(data$medicines_interaction_1.G_number, exclude = NULL)
prop.table(table(data$medicines_interaction_1.G_number, exclude = NULL))

names(data)[names(data) == "S32c"] <- "medicines_interaction_2.G_number"

table(data$medicines_interaction_2.G_number, exclude = NULL)
prop.table(table(data$medicines_interaction_2.G_number, exclude = NULL))

names(data)[names(data) == "S32c"] <- "medicines_interaction_3.G_number"

table(data$medicines_interaction_3.G_number, exclude = NULL)
prop.table(table(data$medicines_interaction_3.G_number, exclude = NULL))

names(data)[names(data) == "S32c"] <- "medicines_interaction_4.G_number"

table(data$medicines_interaction_4.G_number, exclude = NULL)
prop.table(table(data$medicines_interaction_4.G_number, exclude = NULL))

names(data)[names(data) == "S41c"] <- "medicines_interaction_1.review_pharmacy_technician"
data$medicines_interaction_1.review_pharmacy_technician <- factor(data$medicines_interaction_1.review_pharmacy_technician, 
                                                                  levels = c(1, 
                                                                             2, 
                                                                             3), 
                                                                  labels = c("yes/yes", 
                                                                             "yes/no", 
                                                                             "no/no"))
data$medicines_interaction_1.review_pharmacy_technician.yes_yes_1 <- ifelse(data$medicines_interaction_1.review_pharmacy_technician == "yes/yes", 
                                                                            1, 
                                                                            0) 
data$medicines_interaction_1.review_pharmacy_technician.yes_no_1 <- ifelse(data$medicines_interaction_1.review_pharmacy_technician == "yes/no", 
                                                                           1, 
                                                                           0) 
data$medicines_interaction_1.review_pharmacy_technician.no_no_1 <- ifelse(data$medicines_interaction_1.review_pharmacy_technician == "no/no", 
                                                                          1, 
                                                                          0) 

table(data$medicines_interaction_1.review_pharmacy_technician, exclude = NULL)
prop.table(table(data$medicines_interaction_1.review_pharmacy_technician, exclude = NULL))

names(data)[names(data) == "S42c"] <- "medicines_interaction_2.review_pharmacy_technician"
data$medicines_interaction_2.review_pharmacy_technician <- factor(data$medicines_interaction_2.review_pharmacy_technician, 
                                                                  levels = c(1, 
                                                                             2, 
                                                                             3), 
                                                                  labels = c("yes/yes", 
                                                                             "yes/no", 
                                                                             "no/no"))
data$medicines_interaction_2.review_pharmacy_technician.yes_yes_1 <- ifelse(data$medicines_interaction_2.review_pharmacy_technician == "yes/yes", 
                                                                            1, 
                                                                            0) 
data$medicines_interaction_2.review_pharmacy_technician.yes_no_1 <- ifelse(data$medicines_interaction_2.review_pharmacy_technician == "yes/no", 
                                                                           1, 
                                                                           0) 
data$medicines_interaction_2.review_pharmacy_technician.no_no_1 <- ifelse(data$medicines_interaction_2.review_pharmacy_technician == "no/no", 
                                                                          1, 
                                                                          0) 

table(data$medicines_interaction_2.review_pharmacy_technician, exclude = NULL)
prop.table(table(data$medicines_interaction_2.review_pharmacy_technician, exclude = NULL))

names(data)[names(data) == "S43c"] <- "medicines_interaction_3.review_pharmacy_technician"
data$medicines_interaction_3.review_pharmacy_technician <- factor(data$medicines_interaction_3.review_pharmacy_technician, 
                                                                  levels = c(1, 
                                                                             2, 
                                                                             3), 
                                                                  labels = c("yes/yes", 
                                                                             "yes/no", 
                                                                             "no/no"))
data$medicines_interaction_3.review_pharmacy_technician.yes_yes_1 <- ifelse(data$medicines_interaction_3.review_pharmacy_technician == "yes/yes", 
                                                                            1, 
                                                                            0) 
data$medicines_interaction_3.review_pharmacy_technician.yes_no_1 <- ifelse(data$medicines_interaction_3.review_pharmacy_technician == "yes/no", 
                                                                           1, 
                                                                           0) 
data$medicines_interaction_3.review_pharmacy_technician.no_no_1 <- ifelse(data$medicines_interaction_3.review_pharmacy_technician == "no/no", 
                                                                          1, 
                                                                          0) 

table(data$medicines_interaction_3.review_pharmacy_technician, exclude = NULL)
prop.table(table(data$medicines_interaction_3.review_pharmacy_technician, exclude = NULL))

names(data)[names(data) == "S44c"] <- "medicines_interaction_4.review_pharmacy_technician"
data$medicines_interaction_4.review_pharmacy_technician <- factor(data$medicines_interaction_4.review_pharmacy_technician, 
                                                                  levels = c(1, 
                                                                             2, 
                                                                             3), 
                                                                  labels = c("yes/yes", 
                                                                             "yes/no", 
                                                                             "no/no"))
data$medicines_interaction_4.review_pharmacy_technician.yes_yes_1 <- ifelse(data$medicines_interaction_4.review_pharmacy_technician == "yes/yes", 
                                                                            1, 
                                                                            0) 
data$medicines_interaction_4.review_pharmacy_technician.yes_no_1 <- ifelse(data$medicines_interaction_4.review_pharmacy_technician == "yes/no", 
                                                                           1, 
                                                                           0) 
data$medicines_interaction_4.review_pharmacy_technician.no_no_1 <- ifelse(data$medicines_interaction_4.review_pharmacy_technician == "no/no", 
                                                                          1, 
                                                                          0) 

table(data$medicines_interaction_4.review_pharmacy_technician, exclude = NULL)
prop.table(table(data$medicines_interaction_4.review_pharmacy_technician, exclude = NULL))

names(data)[names(data) == "S51c"] <- "medicines_interaction_1.revision_pharmacy_technician.yes_1"
data$medicines_interaction_1.revision_pharmacy_technician.yes_1 <- ifelse(data$medicines_interaction_1.revision_pharmacy_technician.yes_1 == 1,
                                                                          1, 
                                                                          0)

table(data$medicines_interaction_1.revision_pharmacy_technician.yes_1, exclude = NULL)
prop.table(table(data$medicines_interaction_1.revision_pharmacy_technician.yes_1, exclude = NULL))

names(data)[names(data) == "S52c"] <- "medicines_interaction_2.revision_pharmacy_technician.yes_1"
data$medicines_interaction_2.revision_pharmacy_technician.yes_1 <- ifelse(data$medicines_interaction_2.revision_pharmacy_technician.yes_1 == 1,
                                                                          1, 
                                                                          0)

table(data$medicines_interaction_2.revision_pharmacy_technician.yes_1, exclude = NULL)
prop.table(table(data$medicines_interaction_2.revision_pharmacy_technician.yes_1, exclude = NULL))

names(data)[names(data) == "S53c"] <- "medicines_interaction_3.revision_pharmacy_technician.yes_1"
data$medicines_interaction_3.revision_pharmacy_technician.yes_1 <- ifelse(data$medicines_interaction_3.revision_pharmacy_technician.yes_1 == 1,
                                                                          1, 
                                                                          0)

table(data$medicines_interaction_3.revision_pharmacy_technician.yes_1, exclude = NULL)
prop.table(table(data$medicines_interaction_3.revision_pharmacy_technician.yes_1, exclude = NULL))

names(data)[names(data) == "S54c"] <- "medicines_interaction_4.revision_pharmacy_technician.yes_1"
data$medicines_interaction_4.revision_pharmacy_technician.yes_1 <- ifelse(data$medicines_interaction_4.revision_pharmacy_technician.yes_1 == 1,
                                                                          1, 
                                                                          0)

table(data$medicines_interaction_4.revision_pharmacy_technician.yes_1, exclude = NULL)
prop.table(table(data$medicines_interaction_4.revision_pharmacy_technician.yes_1, exclude = NULL))

### Revision by physician ----

names(data)[names(data) == "S6c"] <- "revision_physician"
data$revision_physician <- ifelse(is.na(data$revision_physician),
                                  3,
                                  data$revision_physician) 
data$revision_physician <- factor(data$revision_physician,
                                  levels = c(1,                              
                                             2, 
                                             3), 
                                  labels = c("yes: medicine", 
                                             "yes: non-medicine", 
                                             "no"))

table(data$revision_physician, exclude = NULL)
prop.table(table(data$revision_physician, exclude = NULL))

data$revision_physician.medicine_1 <- ifelse(data$revision_physician == "yes: medicine", 
                                             1, 
                                             0)
data$revision_physician.non_medicine_1 <- ifelse(data$revision_physician == "yes: non-medicine",
                                                 1, 
                                                 0)
data$revision_physician.none_1 <- ifelse(data$revision_physician == "no", 
                                         1, 
                                         0)

data$y <- ifelse((data$revision_physician.medicine_1 == 1) 
                 | 
                 (data$revision_physician.non_medicine_1 == 1),
                 1,
                 0)

table(data$y, exclude = NULL)
prop.table(table(data$y, exclude = NULL))

### National Coordinating Council for  Medication Error Reporting and Prevention index ----

names(data)[names(data) == "C5c"] <- "NCC_MERP_index"
data$NCC_MERP_index <- factor(data$NCC_MERP_index, 
                              levels = c(1, 
                                         2, 
                                         3,
                                         4,
                                         5,
                                         6,
                                         7,
                                         8,
                                         9),
                              labels = c("A",
                                         "B",
                                         "C",
                                         "D",
                                         "E",
                                         "F",
                                         "G",
                                         "H",
                                         "I")) 

table(data$NCC_MERP_index, exclude = NULL)
prop.table(table(data$NCC_MERP_index, exclude = NULL))

data$NCC_MERP_index.A_1 <- ifelse(data$NCC_MERP_index == "A",
                                  1,
                                  0)
data$NCC_MERP_index.B_1 <- ifelse(data$NCC_MERP_index == "B",
                                  1,
                                  0)
data$NCC_MERP_index.C_1 <- ifelse(data$NCC_MERP_index == "C",
                                  1,
                                  0)
data$NCC_MERP_index.D_1 <- ifelse(data$NCC_MERP_index == "D",
                                  1,
                                  0)
data$NCC_MERP_index.E_1 <- ifelse(data$NCC_MERP_index == "E",
                                  1,
                                  0)
data$NCC_MERP_index.F_1 <- ifelse(data$NCC_MERP_index == "F",
                                  1,
                                  0)
data$NCC_MERP_index.G_1 <- ifelse(data$NCC_MERP_index == "G",
                                  1,
                                  0)
data$NCC_MERP_index.H_1 <- ifelse(data$NCC_MERP_index == "H",
                                  1,
                                  0)
data$NCC_MERP_index.I_1 <- ifelse(data$NCC_MERP_index == "I",
                                  1,
                                  0)
data$NCC_MERP_index.J_1 <- ifelse(data$NCC_MERP_index == "J",
                                  1,
                                  0)

data$NCC_MERP_index.E_or_above_1 <- ifelse((data$NCC_MERP_index.E_1 == "1")
                                           |
                                           (data$NCC_MERP_index.F_1 == "1")
                                           | 
                                           (data$NCC_MERP_index.G_1 == "1")
                                           |
                                           (data$NCC_MERP_index.H_1 == "1")
                                           | 
                                           (data$NCC_MERP_index.I_1 == "1"),
                                           1,
                                           0)

### Information gained ----

names(data)[names(data) == "S8c"] <- "information_gained"
table(data$information_gained, exclude = NULL)
data$information_gained <- factor(data$information_gained,
                                  levels = c(1,
                                             2,
                                             3),
                                  labels = c("yes: direct",
                                             "yes: indirect",
                                             "no"))

table(data$information_gained, exclude = NULL)
prop.table(table(data$information_gained, exclude = NULL))

data$information_gained.yes_direct_1 <- ifelse(data$information_gained == "yes: direct",
                                               1,
                                               0)
data$information_gained.yes_indirect_1 <- ifelse(data$information_gained == "yes: indirect",
                                                 1,
                                                 0)
data$information_gained.no_1 <- ifelse(data$information_gained == "no",
                                       1,
                                       0)

# Determination number of candidate parameters via method by Riley et al. 2020 (doi: 10.1136/bmj.m441) ----

## prevalence_values ----

prop.table(table(data$y))

prevalence_values <- c(0.03, 0.04, 0.05)

## RsqCS_values ----

RsqN_values <- c(0.05, 0.10, 0.15, 0.20)
data_pmsampsize <- expand.grid(prevalence_values,
                               RsqN_values)
colnames(data_pmsampsize) <- c("prevalence",
                               "RsqN")
data_pmsampsize$max_RsqCS <- NA
  fun.lnLnull <- function(n_events, n_participants) {
    n_events * log(n_events / n_participants) + (n_participants - n_events) * log(1 - n_events / n_participants)
  }
  
  fun.max_RsqCS <- function(lnLnull, n_participants) {
    1 - exp((2 * lnLnull) / n_participants)
  }
for (i in 1:nrow(data_pmsampsize)) {
  data_pmsampsize[i, "max_RsqCS"] <- fun.max_RsqCS(lnLnull = fun.lnLnull(n_events = data_pmsampsize[i, "prevalence"] * 100,
                                                                         n_participants = 100),
                                                   n_participants = 100)
}

data_pmsampsize$RsqCS <- data_pmsampsize$RsqN * data_pmsampsize$max_RsqCS

## parameter_values ----

parameter_values <- 1:30
data_pmsampsize <- merge(data_pmsampsize,
                         parameter_values)
colnames(data_pmsampsize)[5] <- "n_parameters" 

## Sample size calculation ----

data_pmsampsize$n_participants <- NA
for (i in 1:nrow(data_pmsampsize)) {
  data_pmsampsize[i, "n_participants"] <- pmsampsize(type = "b", 
                                                     rsquared = data_pmsampsize[i, "RsqCS"], 
                                                     parameters = data_pmsampsize[i, "n_parameters"], 
                                                     prevalence = data_pmsampsize[i, "prevalence"])[["sample_size"]]
}

plot.pmsampsize <- 
  ggplot(data_pmsampsize, aes(x = n_parameters, y = n_participants, colour = factor(prevalence), shape = factor(RsqN))) + 
    geom_point() +
    geom_hline(yintercept = 2289, linetype = 1) +
    annotate("text",
             x = 1,
             y = 2750,
             label = "2289") +
    scale_x_continuous(name = "Number of parameters",
                       breaks = seq(0, 30, 1),
                       labels = seq(0, 30, 1)) +
    scale_y_continuous(name = "Number of participants",
                       breaks = seq(0, 23000, 1000),
                       labels = seq(0, 23000, 1000)) +
    scale_colour_discrete(name = "Outcome proportion") +
    scale_shape_discrete(name = expression("R"^2*""["Nagelkerke"]),
                         labels = c("0.05",
                                    "0.10",
                                    "0.15",
                                    "0.20")) +
    theme_minimal() 

max(subset(data_pmsampsize, (n_participants <= 2289) 
                            & 
                            (prevalence == 0.03) 
                            & 
                            (RsqN == 0.05))$n_parameters)
max(subset(data_pmsampsize, (n_participants <= 2289) 
                            & 
                            (prevalence == 0.05) 
                            & 
                            (RsqN == 0.20))$n_parameters)
max(subset(data_pmsampsize, (n_participants <= 2289) 
                            & 
                            (prevalence == 0.04) 
                            & 
                            (RsqN == 0.15))$n_parameters)

# Modelling ----

## Creating final data object 'data' ----

data <- data[, c("id",
                 "y",
                 "age",
                 "age.std",
                 "sex.male_1",
                 "n_prescribed_medicines",
                 "n_prescribed_medicines.std",
                 "high_risk_medicines.yes_1",
                 "eGFR.below_60_1",
                 "out_specialty_current_MUMC.group_A_1",
                 "out_specialty_current_MUMC.group_B_1",
                 "out_specialty_current_MUMC.group_C_1",
                 "n_specialty_MUMC",
                 "n_specialty_MUMC.std",
                 "health_literacy_composite.insufficient_1",
                 "NCC_MERP_index.E_or_above_1")]

## Missing values ----

#eGFR.below_60_1

table(is.na(data$eGFR.below_60_1), exclude = NULL)
prop.table(table(is.na(data$eGFR.below_60_1), exclude = NULL))

table(subset(data, !is.na(eGFR.below_60_1))$y, exclude = NULL)
prop.table(table(subset(data, !is.na(eGFR.below_60_1))$y, exclude = NULL))
table(subset(data, is.na(eGFR.below_60_1))$y, exclude = NULL)
prop.table(table(subset(data, is.na(eGFR.below_60_1))$y, exclude = NULL))

summary(subset(data, !is.na(eGFR.below_60_1))$age)
sd(subset(data, !is.na(eGFR.below_60_1))$age)
summary(subset(data, is.na(eGFR.below_60_1))$age)
sd(subset(data, is.na(eGFR.below_60_1))$age)

table(subset(data, !is.na(eGFR.below_60_1))$sex.male_1, exclude = NULL)
prop.table(table(subset(data, !is.na(eGFR.below_60_1))$sex.male_1, exclude = NULL))
table(subset(data, is.na(eGFR.below_60_1))$sex.male_1, exclude = NULL)
prop.table(table(subset(data, is.na(eGFR.below_60_1))$sex.male_1, exclude = NULL))

summary(subset(data, !is.na(eGFR.below_60_1))$n_prescribed_medicines)
sd(subset(data, !is.na(eGFR.below_60_1))$n_prescribed_medicines)
summary(subset(data, is.na(eGFR.below_60_1))$n_prescribed_medicines)
sd(subset(data, is.na(eGFR.below_60_1))$n_prescribed_medicines)

table(subset(data, !is.na(eGFR.below_60_1))$high_risk_medicines.yes_1, exclude = NULL)
prop.table(table(subset(data, !is.na(eGFR.below_60_1))$high_risk_medicines.yes_1, exclude = NULL))
table(subset(data, is.na(eGFR.below_60_1))$high_risk_medicines.yes_1, exclude = NULL)
prop.table(table(subset(data, is.na(eGFR.below_60_1))$high_risk_medicines.yes_1, exclude = NULL))

table(subset(data, !is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_A_1, exclude = NULL)
prop.table(table(subset(data, !is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_A_1, exclude = NULL))
table(subset(data, is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_A_1, exclude = NULL)
prop.table(table(subset(data, is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_A_1, exclude = NULL))

table(subset(data, !is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_B_1, exclude = NULL)
prop.table(table(subset(data, !is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_B_1, exclude = NULL))
table(subset(data, is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_B_1, exclude = NULL)
prop.table(table(subset(data, is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_B_1, exclude = NULL))

table(subset(data, !is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_C_1, exclude = NULL)
prop.table(table(subset(data, !is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_C_1, exclude = NULL))
table(subset(data, is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_C_1, exclude = NULL)
prop.table(table(subset(data, is.na(eGFR.below_60_1))$out_specialty_current_MUMC.group_C_1, exclude = NULL))

summary(subset(data, !is.na(eGFR.below_60_1))$n_specialty_MUMC)
sd(subset(data, !is.na(eGFR.below_60_1))$n_specialty_MUMC)
summary(subset(data, is.na(eGFR.below_60_1))$n_specialty_MUMC)
sd(subset(data, is.na(eGFR.below_60_1))$n_specialty_MUMC)

table(subset(data, !is.na(eGFR.below_60_1))$health_literacy_composite.insufficient_1, exclude = NULL)
prop.table(table(subset(data, !is.na(eGFR.below_60_1))$health_literacy_composite.insufficient_1, exclude = NULL))
table(subset(data, is.na(eGFR.below_60_1))$health_literacy_composite.insufficient_1, exclude = NULL)
prop.table(table(subset(data, is.na(eGFR.below_60_1))$health_literacy_composite.insufficient_1, exclude = NULL))

#health_literacy_composite.insufficient_1

table(is.na(data$health_literacy_composite.insufficient_1), exclude = NULL)
prop.table(table(is.na(data$health_literacy_composite.insufficient_1), exclude = NULL))

table(subset(data, !is.na(health_literacy_composite.insufficient_1))$y, exclude = NULL)
prop.table(table(subset(data, !is.na(health_literacy_composite.insufficient_1))$y, exclude = NULL))
table(subset(data, is.na(health_literacy_composite.insufficient_1))$y, exclude = NULL)
prop.table(table(subset(data, is.na(health_literacy_composite.insufficient_1))$y, exclude = NULL))

summary(subset(data, !is.na(health_literacy_composite.insufficient_1))$age)
sd(subset(data, !is.na(health_literacy_composite.insufficient_1))$age)
summary(subset(data, is.na(health_literacy_composite.insufficient_1))$age)
sd(subset(data, is.na(health_literacy_composite.insufficient_1))$age)

table(subset(data, !is.na(health_literacy_composite.insufficient_1))$sex.male_1, exclude = NULL)
prop.table(table(subset(data, !is.na(health_literacy_composite.insufficient_1))$sex.male_1, exclude = NULL))
table(subset(data, is.na(health_literacy_composite.insufficient_1))$sex.male_1, exclude = NULL)
prop.table(table(subset(data, is.na(health_literacy_composite.insufficient_1))$sex.male_1, exclude = NULL))

summary(subset(data, !is.na(health_literacy_composite.insufficient_1))$n_prescribed_medicines)
sd(subset(data, !is.na(health_literacy_composite.insufficient_1))$n_prescribed_medicines)
summary(subset(data, is.na(health_literacy_composite.insufficient_1))$n_prescribed_medicines)
sd(subset(data, is.na(health_literacy_composite.insufficient_1))$n_prescribed_medicines)

table(subset(data, !is.na(health_literacy_composite.insufficient_1))$high_risk_medicines.yes_1, exclude = NULL)
prop.table(table(subset(data, !is.na(health_literacy_composite.insufficient_1))$high_risk_medicines.yes_1, exclude = NULL))
table(subset(data, is.na(health_literacy_composite.insufficient_1))$high_risk_medicines.yes_1, exclude = NULL)
prop.table(table(subset(data, is.na(health_literacy_composite.insufficient_1))$high_risk_medicines.yes_1, exclude = NULL))

table(subset(data, !is.na(health_literacy_composite.insufficient_1))$eGFR.below_60_1, exclude = NULL)
prop.table(table(subset(data, !is.na(health_literacy_composite.insufficient_1))$eGFR.below_60_1, exclude = NULL))
table(subset(data, is.na(health_literacy_composite.insufficient_1))$eGFR.below_60_1, exclude = NULL)
prop.table(table(subset(data, is.na(health_literacy_composite.insufficient_1))$eGFR.below_60_1, exclude = NULL))

table(subset(data, !is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_A_1, exclude = NULL)
prop.table(table(subset(data, !is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_A_1, exclude = NULL))
table(subset(data, is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_A_1, exclude = NULL)
prop.table(table(subset(data, is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_A_1, exclude = NULL))

table(subset(data, !is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_B_1, exclude = NULL)
prop.table(table(subset(data, !is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_B_1, exclude = NULL))
table(subset(data, is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_B_1, exclude = NULL)
prop.table(table(subset(data, is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_B_1, exclude = NULL))

table(subset(data, !is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_C_1, exclude = NULL)
prop.table(table(subset(data, !is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_C_1, exclude = NULL))
table(subset(data, is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_C_1, exclude = NULL)
prop.table(table(subset(data, is.na(health_literacy_composite.insufficient_1))$out_specialty_current_MUMC.group_C_1, exclude = NULL))

summary(subset(data, !is.na(health_literacy_composite.insufficient_1))$n_specialty_MUMC)
sd(subset(data, !is.na(health_literacy_composite.insufficient_1))$n_specialty_MUMC)
summary(subset(data, is.na(health_literacy_composite.insufficient_1))$n_specialty_MUMC)
sd(subset(data, is.na(health_literacy_composite.insufficient_1))$n_specialty_MUMC)

## Unadjusted odds ratios ----

### Imputation ----

n_imp <- 30

unadj.imp <- aregImpute(~ y +
                          age.std +
                          sex.male_1 +
                          n_prescribed_medicines.std +
                          high_risk_medicines.yes_1 +
                          eGFR.below_60_1 +
                          out_specialty_current_MUMC.group_A_1 +
                          out_specialty_current_MUMC.group_B_1 +
                          n_specialty_MUMC.std +
                          health_literacy_composite.insufficient_1,
                        x = TRUE,
                        data = data,
                        n.impute = n_imp,
                        nk = 3)

for(i in 1:n_imp) {
  print(paste("imputation", i))
  print(table(impute.transcan(unadj.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$eGFR.below_60_1))
  print(prop.table(table(impute.transcan(unadj.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$eGFR.below_60_1)))
}

for(i in 1:n_imp) {
  print(paste("imputation", i))
  print(table(impute.transcan(unadj.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$health_literacy_composite.insufficient_1))
  print(prop.table(table(impute.transcan(unadj.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$health_literacy_composite.insufficient_1)))
}

### age.std ----

unadj.age.std <- fit.mult.impute(y ~
                                   age.std,
                                 lrm,
                                 unadj.imp,
                                 data = data)

unadj.age.std
exp(unadj.age.std$coefficients)
exp(unadj.age.std$coefficients - 1.96 * sqrt(diag(unadj.age.std$var)))
exp(unadj.age.std$coefficients + 1.96 * sqrt(diag(unadj.age.std$var)))

### sex.male_1 ----

unadj.sex.male_1 <- fit.mult.impute(y ~
                                   sex.male_1,
                                 lrm,
                                 unadj.imp,
                                 data = data)

unadj.sex.male_1
exp(unadj.sex.male_1$coefficients)
exp(unadj.sex.male_1$coefficients - 1.96 * sqrt(diag(unadj.sex.male_1$var)))
exp(unadj.sex.male_1$coefficients + 1.96 * sqrt(diag(unadj.sex.male_1$var)))

### n_prescribed_medicines.std ----

unadj.n_prescribed_medicines.std <- fit.mult.impute(y ~
                                                      n_prescribed_medicines.std,
                                                    lrm,
                                                    unadj.imp,
                                                    data = data)

unadj.n_prescribed_medicines.std
exp(unadj.n_prescribed_medicines.std$coefficients)
exp(unadj.n_prescribed_medicines.std$coefficients - 1.96 * sqrt(diag(unadj.n_prescribed_medicines.std$var)))
exp(unadj.n_prescribed_medicines.std$coefficients + 1.96 * sqrt(diag(unadj.n_prescribed_medicines.std$var)))

### high_risk_medicines.yes_1 ----

unadj.high_risk_medicines.yes_1 <- fit.mult.impute(y ~
                                                     high_risk_medicines.yes_1,
                                                   lrm,
                                                   unadj.imp,
                                                   data = data)

unadj.high_risk_medicines.yes_1
exp(unadj.high_risk_medicines.yes_1$coefficients)
exp(unadj.high_risk_medicines.yes_1$coefficients - 1.96 * sqrt(diag(unadj.high_risk_medicines.yes_1$var)))
exp(unadj.high_risk_medicines.yes_1$coefficients + 1.96 * sqrt(diag(unadj.high_risk_medicines.yes_1$var)))

### eGFR.below_60_1 ----

unadj.eGFR.below_60_1 <- fit.mult.impute(y ~
                                           eGFR.below_60_1,
                                         lrm,
                                         unadj.imp,
                                         data = data)

unadj.eGFR.below_60_1
exp(unadj.eGFR.below_60_1$coefficients)
exp(unadj.eGFR.below_60_1$coefficients - 1.96 * sqrt(diag(unadj.eGFR.below_60_1$var)))
exp(unadj.eGFR.below_60_1$coefficients + 1.96 * sqrt(diag(unadj.eGFR.below_60_1$var)))

### out_specialty_current_MUMC.group_A_1 ----

unadj.out_specialty_current_MUMC.group_A_1 <- fit.mult.impute(y ~
                                                                out_specialty_current_MUMC.group_A_1,
                                                              lrm,
                                                              unadj.imp,
                                                              data = data)

unadj.out_specialty_current_MUMC.group_A_1
exp(unadj.out_specialty_current_MUMC.group_A_1$coefficients)
exp(unadj.out_specialty_current_MUMC.group_A_1$coefficients - 1.96 * sqrt(diag(unadj.out_specialty_current_MUMC.group_A_1$var)))
exp(unadj.out_specialty_current_MUMC.group_A_1$coefficients + 1.96 * sqrt(diag(unadj.out_specialty_current_MUMC.group_A_1$var)))

### out_specialty_current_MUMC.group_B_1 ----

unadj.out_specialty_current_MUMC.group_B_1 <- fit.mult.impute(y ~
                                                                out_specialty_current_MUMC.group_B_1,
                                                              lrm,
                                                              unadj.imp,
                                                              data = data)

unadj.out_specialty_current_MUMC.group_B_1
exp(unadj.out_specialty_current_MUMC.group_B_1$coefficients)
exp(unadj.out_specialty_current_MUMC.group_B_1$coefficients - 1.96 * sqrt(diag(unadj.out_specialty_current_MUMC.group_B_1$var)))
exp(unadj.out_specialty_current_MUMC.group_B_1$coefficients + 1.96 * sqrt(diag(unadj.out_specialty_current_MUMC.group_B_1$var)))

### n_specialty_MUMC.std ----

unadj.n_specialty_MUMC.std <- fit.mult.impute(y ~
                                   n_specialty_MUMC.std,
                                 lrm,
                                 unadj.imp,
                                 data = data)

unadj.n_specialty_MUMC.std
exp(unadj.n_specialty_MUMC.std$coefficients)
exp(unadj.n_specialty_MUMC.std$coefficients - 1.96 * sqrt(diag(unadj.n_specialty_MUMC.std$var)))
exp(unadj.n_specialty_MUMC.std$coefficients + 1.96 * sqrt(diag(unadj.n_specialty_MUMC.std$var)))

### health_literacy_composite.insufficient_1 ----

unadj.health_literacy_composite.insufficient_1 <- fit.mult.impute(y ~
                                   health_literacy_composite.insufficient_1,
                                 lrm,
                                 unadj.imp,
                                 data = data)

unadj.health_literacy_composite.insufficient_1
exp(unadj.health_literacy_composite.insufficient_1$coefficients)
exp(unadj.health_literacy_composite.insufficient_1$coefficients - 1.96 * sqrt(diag(unadj.health_literacy_composite.insufficient_1$var)))
exp(unadj.health_literacy_composite.insufficient_1$coefficients + 1.96 * sqrt(diag(unadj.health_literacy_composite.insufficient_1$var)))

## Model 1 ----

### Imputation ----

fit_1.imp <- aregImpute(~ y +
                          age.std +
                          sex.male_1 +
                          n_prescribed_medicines.std +
                          high_risk_medicines.yes_1 +
                          eGFR.below_60_1 +
                          out_specialty_current_MUMC.group_A_1 +
                          out_specialty_current_MUMC.group_B_1 +
                          n_specialty_MUMC.std +
                          health_literacy_composite.insufficient_1,
                        x = TRUE,
                        data = data,
                        n.impute = n_imp,
                        nk = 3)

for(i in 1:n_imp) {
  print(paste("imputation", i))
  print(table(impute.transcan(fit_1.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$eGFR.below_60_1))
  print(prop.table(table(impute.transcan(fit_1.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$eGFR.below_60_1)))
}

for(i in 1:n_imp) {
  print(paste("imputation", i))
  print(table(impute.transcan(fit_1.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$health_literacy_composite.insufficient_1))
  print(prop.table(table(impute.transcan(fit_1.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$health_literacy_composite.insufficient_1)))
}

fit_1.stacked_imp <- list()
for (i in 1:n_imp) {
  fit_1.stacked_imp[[i]] <- impute.transcan(fit_1.imp, 
                                            imputation = i, 
                                            data = data, 
                                            list.out = TRUE, 
                                            pr = FALSE)
}

### Fit ----

fit_1 <- fit.mult.impute(y ~
                         age.std +
                         sex.male_1 +
                         n_prescribed_medicines.std +
                         high_risk_medicines.yes_1 +
                         eGFR.below_60_1 +                  
                         out_specialty_current_MUMC.group_A_1 +
                         out_specialty_current_MUMC.group_B_1 +
                         n_specialty_MUMC.std +
                         health_literacy_composite.insufficient_1,
                       lrm,
                       fit_1.imp,
                       data = data)
fit_1
exp(fit_1$coefficients)
exp(fit_1$coefficients - 1.96 * sqrt(diag(fit_1$var)))
exp(fit_1$coefficients + 1.96 * sqrt(diag(fit_1$var)))

fit_1.stacked_imp.fit_1 <- list()
for(i in 1:n_imp) {
  fit_1.stacked_imp.fit_1[[i]] <- lrm(y ~ 
                                        age.std +
                                        sex.male_1 +
                                        n_prescribed_medicines.std +
                                        high_risk_medicines.yes_1 +
                                        eGFR.below_60_1 +                  
                                        out_specialty_current_MUMC.group_A_1 +
                                        out_specialty_current_MUMC.group_B_1 + 
                                        n_specialty_MUMC.std +
                                        health_literacy_composite.insufficient_1,
                                      data = as.data.frame(fit_1.stacked_imp[[i]]))
}

### Assessing linearity ----

fit_1.nl <- fit.mult.impute(y ~
                            rcs(age.std, 3) +
                            sex.male_1 +
                            rcs(n_prescribed_medicines.std, 3) +
                            high_risk_medicines.yes_1 +
                            eGFR.below_60_1 +                  
                            out_specialty_current_MUMC.group_A_1 +
                            out_specialty_current_MUMC.group_B_1 + 
                            rcs(n_specialty_MUMC.std, 3) +
                            health_literacy_composite.insufficient_1,
                          lrm,
                          fit_1.imp,
                          data = data)
lrtest(fit_1, fit_1.nl)

### Assessing additivity ----

fit_1.ia <- fit.mult.impute(y ~
                            age.std +
                            sex.male_1 +
                            n_prescribed_medicines.std +
                            high_risk_medicines.yes_1 +
                            eGFR.below_60_1 +
                            out_specialty_current_MUMC.group_A_1 +
                            out_specialty_current_MUMC.group_B_1 +
                            n_specialty_MUMC.std +
                            health_literacy_composite.insufficient_1 +
                            age.std:sex.male_1 +
                            age.std:n_prescribed_medicines.std +
                            age.std:high_risk_medicines.yes_1 +
                            age.std:eGFR.below_60_1 +
                            age.std:out_specialty_current_MUMC.group_A_1 +
                            age.std:out_specialty_current_MUMC.group_B_1 +
                            age.std:n_specialty_MUMC.std +
                            age.std:health_literacy_composite.insufficient_1 +
                            sex.male_1:n_prescribed_medicines.std +
                            sex.male_1:high_risk_medicines.yes_1 +
                            sex.male_1:eGFR.below_60_1 +
                            sex.male_1:out_specialty_current_MUMC.group_A_1 +
                            sex.male_1:out_specialty_current_MUMC.group_B_1 +
                            sex.male_1:n_specialty_MUMC.std +
                            sex.male_1:health_literacy_composite.insufficient_1,
                          lrm,
                          fit_1.imp,
                          data = data)
lrtest(fit_1, fit_1.ia)

### Predicted probabilities ----

data$fit_1.pred_prob <- predict(fit_1, data, type = "fitted.ind")

for (i in 1:n_imp) {
  fit_1.stacked_imp[[i]][["fit_1.pred_prob"]] <- as.vector(predict(fit_1, fit_1.stacked_imp[[i]], type = "fitted.ind"))
}#Pooled model

for (i in 1:n_imp) {
  fit_1.stacked_imp[[i]][["fit_1.stacked_imp.fit_1.pred_prob"]] <- as.vector(predict(fit_1.stacked_imp.fit_1[[i]], fit_1.stacked_imp[[i]], type = "fitted.ind"))
}#Respective model imputed dataset i

### Linear predictor ----

data$fit_1.lp <- predict(fit_1, data, type = "lp")

for (i in 1:n_imp) {
  fit_1.stacked_imp[[i]][["fit_1.lp"]] <- as.vector(predict(fit_1, fit_1.stacked_imp[[i]], type = "lp"))
}#Pooled model

for (i in 1:n_imp) {
  fit_1.stacked_imp[[i]][["fit_1.stacked_imp.fit_1.lp"]] <- as.vector(predict(fit_1.stacked_imp.fit_1[[i]], fit_1.stacked_imp[[i]], type = "lp"))
}#Respective model imputed dataset i

### Figure boxplots ----

for(i in 1:n_imp){
  fit_1.stacked_imp[[i]][["id"]] <- data$id
}
temp <- data.frame(matrix(nrow = 0,
                          ncol = 4))
colnames(temp) <- c("id_imp",
                    "id",
                    "y",
                    "fit_1.pred_prob")
for (i in 1:n_imp) {
  id_imp.temp <- rep(i, nrow(as.data.frame(fit_1.stacked_imp[[i]])))
  
  id.temp <- fit_1.stacked_imp[[i]][["id"]]
  
  y.temp <- fit_1.stacked_imp[[i]][["y"]]
  
  fit_1.pred_prob.temp <- fit_1.stacked_imp[[i]][["fit_1.pred_prob"]]
  
  temp.temp <- data.frame(id_imp = id_imp.temp,
                          id = id.temp,
                          y = y.temp,
                          fit_1.pred_prob = fit_1.pred_prob.temp)
  
  temp <- rbind(temp, 
                temp.temp)
}
y.fit_1.pred_prob.mean_imp <- aggregate(temp[, c("y",
                                                 "fit_1.pred_prob")],
                                        list(temp[, c("id")]),
                                        mean)
y.fit_1.pred_prob.mean_imp <- y.fit_1.pred_prob.mean_imp[, 2:3]
colnames(y.fit_1.pred_prob.mean_imp)[2] <- "fit_1.pred_prob.mean_imp"

summary(subset(y.fit_1.pred_prob.mean_imp, y == 0)$fit_1.pred_prob.mean_imp)
summary(subset(y.fit_1.pred_prob.mean_imp, y == 1)$fit_1.pred_prob.mean_imp)

plot.fit_1 <- 
  ggplot(y.fit_1.pred_prob.mean_imp, aes(x = fit_1.pred_prob.mean_imp, y = y, group = y)) +
    geom_boxplot(width = 0.5) +
    scale_x_continuous(name = "Predicted probability (%)",
                       limits = c(0, 0.30),
                       breaks = seq(0, 0.30, 0.01),
                       labels = seq(0, 0.30, 0.01) * 100) +
    scale_y_continuous(name = NULL,
                       breaks = c(0, 1),
                       labels = c("0" = "No medication policy change",
                                  "1" = "Medication policy change")) +
    ggtitle("Model 1") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))

### Screening rate using pt ----

pt <- c(0.0333,
        0.0500,
        0.1000)

for (p in pt) {
  print(p)
  print(prop.table(table(y.fit_1.pred_prob.mean_imp$fit_1.pred_prob.mean_imp >= p)))
}

### Apparent performance statistics ----

#Rsq Nagelkerke 

fit_1.app.RsqN.mean_imp <- fit_1[["stats"]][["R2"]]
fit_1.app.RsqN.mean_imp

#C-index 

fit_1.app.C.mean_imp <- fit_1[["stats"]][["C"]]
fit_1.app.C.mean_imp

#Performance statistics using pt 

fit_1.app.pt_stats <- as.data.frame(matrix(nrow = 0,
                                           ncol = 8))
colnames(fit_1.app.pt_stats) <- c("fit_1.app.id_imp",
                                  "fit_1.app.pt",
                                  "fit_1.app.Ey",
                                  "fit_1.app.tp",
                                  "fit_1.app.tn",
                                  "fit_1.app.fp",
                                  "fit_1.app.fn",
                                  "fit_1.app.n")
for (i in 1:n_imp){
  for (p in pt) {
    fit_1.app.id_imp.temp <- i
    
    fit_1.app.pt.temp <- p
    
    fit_1.app.Ey.temp <- sum(fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["y"]]) / nrow(as.data.frame(fit_1.stacked_imp[[fit_1.app.id_imp.temp]]))
    
    fit_1.app.tp.temp <- ifelse((fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["fit_1.stacked_imp.fit_1.pred_prob"]] >= fit_1.app.pt.temp)
                                &
                                (fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["y"]] == 1),
                                1,
                                0)
    fit_1.app.tp.temp <- sum(fit_1.app.tp.temp)
    
    fit_1.app.tn.temp <- ifelse((fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["fit_1.stacked_imp.fit_1.pred_prob"]] < fit_1.app.pt.temp)
                                &
                                (fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["y"]] == 0),
                                1,
                                0)
    fit_1.app.tn.temp <- sum(fit_1.app.tn.temp)
    
    fit_1.app.fp.temp <- ifelse((fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["fit_1.stacked_imp.fit_1.pred_prob"]] >= fit_1.app.pt.temp)
                                &
                                (fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["y"]] == 0),
                                1,
                                0)
    fit_1.app.fp.temp <- sum(fit_1.app.fp.temp)
    
    fit_1.app.fn.temp <- ifelse((fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["fit_1.stacked_imp.fit_1.pred_prob"]] < fit_1.app.pt.temp)
                                &
                                (fit_1.stacked_imp[[fit_1.app.id_imp.temp]][["y"]] == 1),
                                1,
                                0)
    fit_1.app.fn.temp <- sum(fit_1.app.fn.temp)
    
    fit_1.app.n.temp <- nrow(as.data.frame(fit_1.stacked_imp[[fit_1.app.id_imp.temp]]))
    
    fit_1.app.pt_stats.temp <- data.frame(fit_1.app.id_imp = fit_1.app.id_imp.temp,
                                          fit_1.app.pt = fit_1.app.pt.temp,
                                          fit_1.app.Ey = fit_1.app.Ey.temp,
                                          fit_1.app.tp = fit_1.app.tp.temp,
                                          fit_1.app.tn = fit_1.app.tn.temp,
                                          fit_1.app.fp = fit_1.app.fp.temp,
                                          fit_1.app.fn = fit_1.app.fn.temp,
                                          fit_1.app.n = fit_1.app.n.temp)
    
    fit_1.app.pt_stats <- rbind(fit_1.app.pt_stats, fit_1.app.pt_stats.temp)
  }
}
fit_1.app.pt_stats$fit_1.app.net_benefit <- NA
fit_1.app.pt_stats$fit_1.app.net_benefit.all <- NA
fit_1.app.pt_stats$fit_1.app.acc <- NA
fit_1.app.pt_stats$fit_1.app.tpr <- NA
fit_1.app.pt_stats$fit_1.app.tnr <- NA
fit_1.app.pt_stats$fit_1.app.ppv <- NA
fit_1.app.pt_stats$fit_1.app.npv <- NA
for(k in 1:nrow(fit_1.app.pt_stats)) {
  fit_1.app.pt_stats[k, "fit_1.app.net_benefit"] <- (fit_1.app.pt_stats[k, "fit_1.app.tp"] / fit_1.app.pt_stats[k, "fit_1.app.n"]) - 
                                                    (fit_1.app.pt_stats[k, "fit_1.app.fp"] / fit_1.app.pt_stats[k, "fit_1.app.n"]) * 
                                                    (fit_1.app.pt_stats[k, "fit_1.app.pt"] / (1 - fit_1.app.pt_stats[k, "fit_1.app.pt"]))
  
  fit_1.app.pt_stats[k, "fit_1.app.net_benefit.all"] <- fit_1.app.pt_stats[k, "fit_1.app.Ey"] - 
                                                        ( ( fit_1.app.pt_stats[k, "fit_1.app.pt"] / (1 - fit_1.app.pt_stats[k, "fit_1.app.pt"]) ) * (1 - fit_1.app.pt_stats[k, "fit_1.app.Ey"] ) )
  
  fit_1.app.pt_stats[k, "fit_1.app.acc"] <- (fit_1.app.pt_stats[k, "fit_1.app.tp"] +  fit_1.app.pt_stats[k, "fit_1.app.tn"]) /
                                            (fit_1.app.pt_stats[k, "fit_1.app.tp"] + fit_1.app.pt_stats[k, "fit_1.app.tn"] + fit_1.app.pt_stats[k, "fit_1.app.fp"] + fit_1.app.pt_stats[k, "fit_1.app.fn"])
  
  fit_1.app.pt_stats[k, "fit_1.app.tpr"] <- fit_1.app.pt_stats[k, "fit_1.app.tp"] /
                                            (fit_1.app.pt_stats[k, "fit_1.app.tp"] + fit_1.app.pt_stats[k, "fit_1.app.fn"])
  
  fit_1.app.pt_stats[k, "fit_1.app.tnr"] <- fit_1.app.pt_stats[k, "fit_1.app.tn"] /
                                            (fit_1.app.pt_stats[k, "fit_1.app.tn"] + fit_1.app.pt_stats[k, "fit_1.app.fp"])
  
  fit_1.app.pt_stats[k, "fit_1.app.ppv"] <- fit_1.app.pt_stats[k, "fit_1.app.tp"] /
                                            (fit_1.app.pt_stats[k, "fit_1.app.tp"] + fit_1.app.pt_stats[k, "fit_1.app.fp"])
  
  fit_1.app.pt_stats[k, "fit_1.app.npv"] <- fit_1.app.pt_stats[k, "fit_1.app.tn"] /
                                            (fit_1.app.pt_stats[k, "fit_1.app.tn"] + fit_1.app.pt_stats[k, "fit_1.app.fn"])
}  
fit_1.app.pt_stats.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                    ncol = 7))
colnames(fit_1.app.pt_stats.mean_imp) <- c("fit_1.app.pt", 
                                           "fit_1.app.net_benefit.mean_imp", 
                                           "fit_1.app.net_benefit.all.mean_imp",
                                           "fit_1.app.acc.mean_imp",
                                           "fit_1.app.tpr.mean_imp",
                                           "fit_1.app.tnr.mean_imp",
                                           "fit_1.app.ppv.mean_imp")  
for(p in pt) {
  fit_1.app.pt.temp <- p
  
  fit_1.app.net_benefit.mean_imp.temp <- mean(subset(fit_1.app.pt_stats, pt == fit_1.app.pt.temp)$fit_1.app.net_benefit)
  
  fit_1.app.net_benefit.all.mean_imp.temp <- mean(subset(fit_1.app.pt_stats, pt == fit_1.app.pt.temp)$fit_1.app.net_benefit.all)
  
  fit_1.app.acc.mean_imp.temp <- mean(subset(fit_1.app.pt_stats, pt == fit_1.app.pt.temp)$fit_1.app.acc)
  
  fit_1.app.tpr.mean_imp.temp <- mean(subset(fit_1.app.pt_stats, pt == fit_1.app.pt.temp)$fit_1.app.tpr)
  
  fit_1.app.tnr.mean_imp.temp <- mean(subset(fit_1.app.pt_stats, pt == fit_1.app.pt.temp)$fit_1.app.tnr)    
  
  fit_1.app.ppv.mean_imp.temp <- mean(subset(fit_1.app.pt_stats, pt == fit_1.app.pt.temp)$fit_1.app.ppv)  
  
  fit_1.app.npv.mean_imp.temp <- mean(subset(fit_1.app.pt_stats, pt == fit_1.app.pt.temp)$fit_1.app.npv)  
  
  fit_1.app.pt_stats.mean_imp.temp <- data.frame(fit_1.app.pt = fit_1.app.pt.temp,
                                                 fit_1.app.net_benefit.mean_imp = fit_1.app.net_benefit.mean_imp.temp,
                                                 fit_1.app.net_benefit.all.mean_imp = fit_1.app.net_benefit.all.mean_imp.temp,
                                                 fit_1.app.acc.mean_imp = fit_1.app.acc.mean_imp.temp,
                                                 fit_1.app.tpr.mean_imp = fit_1.app.tpr.mean_imp.temp,
                                                 fit_1.app.tnr.mean_imp = fit_1.app.tnr.mean_imp.temp,
                                                 fit_1.app.ppv.mean_imp = fit_1.app.ppv.mean_imp.temp,
                                                 fit_1.app.npv.mean_imp = fit_1.app.npv.mean_imp.temp)
  
  fit_1.app.pt_stats.mean_imp <- rbind(fit_1.app.pt_stats.mean_imp, fit_1.app.pt_stats.mean_imp.temp)
}
fit_1.app.pt_stats.mean_imp

### Optimism-corrected performance statistics ----

n_rep <- 1000

#Creating objects to capture output loop

##Rsq Nagelkerke

###Bootstrap performance

fit_1.boot.RsqN.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                 ncol = 3))
colnames(fit_1.boot.RsqN.mean_imp) <- c("fit_1.boot.id_rep",
                                        "fit_1.boot.RsqN.mean_imp.n_NA",
                                        "fit_1.boot.RsqN.mean_imp")

###Test performance

fit_1.test.RsqN.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                 ncol = 3))
colnames(fit_1.test.RsqN.mean_imp) <- c("fit_1.test.id_rep",
                                        "fit_1.test.RsqN.mean_imp.n_NA",
                                        "fit_1.test.RsqN.mean_imp")

##C-index

###Bootstrap performance

fit_1.boot.C.mean_imp <- as.data.frame(matrix(nrow = 0,
                                              ncol = 3))
colnames(fit_1.boot.C.mean_imp) <- c("fit_1.boot.id_rep",
                                     "fit_1.boot.C.mean_imp.n_NA",
                                     "fit_1.boot.C.mean_imp")

###Test performance

fit_1.test.C.mean_imp <- as.data.frame(matrix(nrow = 0,
                                              ncol = 3))
colnames(fit_1.test.C.mean_imp) <- c("fit_1.test.id_rep",
                                     "fit_1.test.C.mean_imp.n_NA",
                                     "fit_1.test.C.mean_imp")

##Performance statistics using pt 

###Bootstrap performance

fit_1.boot.pt_stats.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                     ncol = 16))
colnames(fit_1.boot.pt_stats.mean_imp) <- c("fit_1.boot.id_rep",
                                            "fit_1.boot.pt",
                                            "fit_1.boot.net_benefit.mean_imp.n_NA",
                                            "fit_1.boot.net_benefit.mean_imp",
                                            "fit_1.boot.net_benefit.all.mean_imp.n_NA",
                                            "fit_1.boot.net_benefit.all.mean_imp",
                                            "fit_1.boot.acc.mean_imp.n_NA",
                                            "fit_1.boot.acc.mean_imp",
                                            "fit_1.boot.tpr.mean_imp.n_NA",
                                            "fit_1.boot.tpr.mean_imp",
                                            "fit_1.boot.tnr.mean_imp.n_NA",
                                            "fit_1.boot.tnr.mean_imp",
                                            "fit_1.boot.ppv.mean_imp.n_NA",
                                            "fit_1.boot.ppv.mean_imp",
                                            "fit_1.boot.npv.mean_imp.n_NA",
                                            "fit_1.boot.npv.mean_imp")

###Test performance

fit_1.test.pt_stats.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                     ncol = 16))
colnames(fit_1.test.pt_stats.mean_imp) <- c("fit_1.test.id_rep",
                                            "fit_1.test.pt",
                                            "fit_1.test.net_benefit.mean_imp.n_NA",
                                            "fit_1.test.net_benefit.mean_imp",
                                            "fit_1.test.net_benefit.all.mean_imp.n_NA",
                                            "fit_1.test.net_benefit.all.mean_imp",
                                            "fit_1.test.acc.mean_imp.n_NA",
                                            "fit_1.test.acc.mean_imp",
                                            "fit_1.test.tpr.mean_imp.n_NA",
                                            "fit_1.test.tpr.mean_imp",
                                            "fit_1.test.tnr.mean_imp.n_NA",
                                            "fit_1.test.tnr.mean_imp",
                                            "fit_1.test.ppv.mean_imp.n_NA",
                                            "fit_1.test.ppv.mean_imp",
                                            "fit_1.test.npv.mean_imp.n_NA",
                                            "fit_1.test.npv.mean_imp")

#Loop

for(r in 1:n_rep) { 
  
  #Input data
  
  ##Bootstrap performance
  
  ###Bootstrap sample 
  
  fit_1.boot.index <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
  fit_1.boot.data <- data[fit_1.boot.index, ]
  
  ###Imputation 
  
  fit_1.boot.imp <- aregImpute(~ y +
                                 age.std +
                                 sex.male_1 +
                                 n_prescribed_medicines.std +
                                 high_risk_medicines.yes_1 +
                                 eGFR.below_60_1 +
                                 out_specialty_current_MUMC.group_A_1 +
                                 out_specialty_current_MUMC.group_B_1 +
                                 n_specialty_MUMC.std +
                                 health_literacy_composite.insufficient_1,
                               x = TRUE,
                               data = fit_1.boot.data,
                               n.impute = n_imp,
                               nk = 3,
                               pr = FALSE)
  
  fit_1.boot.fit_1.stacked_imp <- list()
  for (i in 1:n_imp) {
    fit_1.boot.fit_1.stacked_imp[[i]] <- impute.transcan(fit_1.boot.imp, 
                                                   imputation = i, 
                                                   data = fit_1.boot.data, 
                                                   list.out = TRUE, 
                                                   pr = FALSE)
  }
  
  ##Test performance
  
  fit_1.test.fit_1.stacked_imp <- fit_1.stacked_imp
  
  #Input model
  
  ##Bootstrap performance
  
  fit_1.boot.fit_1.stacked_imp.fit_1 <- list()
  for(i in 1:n_imp) {
    fit_1.boot.fit_1.stacked_imp.fit_1[[i]] <- lrm(y ~ 
                                               age.std +
                                               sex.male_1 +
                                               n_prescribed_medicines.std +
                                               high_risk_medicines.yes_1 +
                                               eGFR.below_60_1 +                  
                                               out_specialty_current_MUMC.group_A_1 +
                                               out_specialty_current_MUMC.group_B_1 + 
                                               n_specialty_MUMC.std +
                                               health_literacy_composite.insufficient_1,
                                             data = as.data.frame(fit_1.boot.fit_1.stacked_imp[[i]]))
  }
  
  ##Test performance
  
  fit_1.test.fit_1.stacked_imp.fit_1 <- fit_1.boot.fit_1.stacked_imp.fit_1
  
  #Predicted probabilities 
  
  ##Bootstrap performance
  
  for (i in 1:n_imp) {
    fit_1.boot.fit_1.stacked_imp[[i]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]] <- as.vector(predict(fit_1.boot.fit_1.stacked_imp.fit_1[[i]], fit_1.boot.fit_1.stacked_imp[[i]], type = "fitted.ind"))
  }
  
  ##Test performance
  
  for (i in 1:n_imp) {
    fit_1.test.fit_1.stacked_imp[[i]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]] <- as.vector(predict(fit_1.test.fit_1.stacked_imp.fit_1[[i]], fit_1.test.fit_1.stacked_imp[[i]], type = "fitted.ind"))
  }
  
  #Performance statistics
  
  ##Rsq Nagelkerke
  
  ###Bootstrap performance
  
  fit_1.boot.RsqN.mean_imp[r, "fit_1.boot.id_rep"] <- r
  
  fit_1.boot.id_imp.fit_1.boot.RsqN <- as.data.frame(matrix(nrow = 0,
                                                            ncol = 7))
  colnames(fit_1.boot.id_imp.fit_1.boot.RsqN) <- c("fit_1.boot.id_imp",
                                                   "fit_1.boot.deviance.null",
                                                   "fit_1.boot.deviance.fit_1.boot.fit_1.stacked_imp.fit_1",
                                                   "fit_1.boot.n",
                                                   "fit_1.boot.Ey",
                                                   "fit_1.boot.RsqCS",
                                                   "fit_1.boot.RsqN")
  for (i in 1:n_imp){
    fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.id_imp"] <- i
    
    fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.deviance.null"] <- - 2 * sum(log(mean(fit_1.boot.fit_1.stacked_imp[[i]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]]) * fit_1.boot.fit_1.stacked_imp[[i]][["y"]] + (1 - mean(fit_1.boot.fit_1.stacked_imp[[i]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]])) * (1 - fit_1.boot.fit_1.stacked_imp[[i]][["y"]])))
    
    fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.deviance.fit_1.boot.fit_1.stacked_imp.fit_1"] <- - 2 * sum(log(fit_1.boot.fit_1.stacked_imp[[i]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]] * fit_1.boot.fit_1.stacked_imp[[i]][["y"]] + (1 - fit_1.boot.fit_1.stacked_imp[[i]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]]) * (1 - fit_1.boot.fit_1.stacked_imp[[i]][["y"]])))
    
    fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.n"] <- nrow(as.data.frame(fit_1.boot.fit_1.stacked_imp[[i]]))
    
    fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.Ey"] <- sum(fit_1.boot.fit_1.stacked_imp[[i]][["y"]]) / nrow(as.data.frame(fit_1.boot.fit_1.stacked_imp[[i]]))
    
    fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.RsqCS"] <- 1 - exp((fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.deviance.fit_1.boot.fit_1.stacked_imp.fit_1"] - fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.deviance.null"]) / 
                                                                         nrow(as.data.frame(fit_1.boot.fit_1.stacked_imp[[i]])))
    
    fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.RsqN"] <- fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.RsqCS"] / 
                                                               (1 - ((fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.Ey"] ** fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.Ey"]) * 
                                                                      ((1 - fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.Ey"]) ** (1 - fit_1.boot.id_imp.fit_1.boot.RsqN[i, "fit_1.boot.Ey"])) 
                                                                     ) ** 2)
  }
  
  fit_1.boot.RsqN.mean_imp[r, "fit_1.boot.RsqN.mean_imp.n_NA"] <- sum(is.na(fit_1.boot.id_imp.fit_1.boot.RsqN$fit_1.boot.RsqN))
  
  fit_1.boot.RsqN.mean_imp[r, "fit_1.boot.RsqN.mean_imp"] <- mean(fit_1.boot.id_imp.fit_1.boot.RsqN$fit_1.boot.RsqN, na.rm = TRUE)
  
  ###Test performance
  
  fit_1.test.RsqN.mean_imp[r, "fit_1.test.id_rep"] <- r
  
  fit_1.test.id_imp.fit_1.test.RsqN <- as.data.frame(matrix(nrow = 0,
                                                            ncol = 7))
  colnames(fit_1.test.id_imp.fit_1.test.RsqN) <- c("fit_1.test.id_imp",
                                                   "fit_1.test.deviance.null",
                                                   "fit_1.test.deviance.fit_1.test.fit_1.stacked_imp.fit_1",
                                                   "fit_1.test.n",
                                                   "fit_1.test.Ey",
                                                   "fit_1.test.RsqCS",
                                                   "fit_1.test.RsqN")
  for (i in 1:n_imp){
    fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.id_imp"] <- i
    
    fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.deviance.null"] <- - 2 * sum(log(mean(fit_1.test.fit_1.stacked_imp[[i]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]]) * fit_1.test.fit_1.stacked_imp[[i]][["y"]] + (1 - mean(fit_1.test.fit_1.stacked_imp[[i]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]])) * (1 - fit_1.test.fit_1.stacked_imp[[i]][["y"]])))
    
    fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.deviance.fit_1.test.fit_1.stacked_imp.fit_1"] <- - 2 * sum(log(fit_1.test.fit_1.stacked_imp[[i]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]] * fit_1.test.fit_1.stacked_imp[[i]][["y"]] + (1 - fit_1.test.fit_1.stacked_imp[[i]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]]) * (1 - fit_1.test.fit_1.stacked_imp[[i]][["y"]])))
    
    fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.n"] <- nrow(as.data.frame(fit_1.test.fit_1.stacked_imp[[i]]))
    
    fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.Ey"] <- sum(fit_1.test.fit_1.stacked_imp[[i]][["y"]]) / nrow(as.data.frame(fit_1.test.fit_1.stacked_imp[[i]]))
    
    fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.RsqCS"] <- 1 - exp((fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.deviance.fit_1.test.fit_1.stacked_imp.fit_1"] - fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.deviance.null"]) / 
                                                                        nrow(as.data.frame(fit_1.test.fit_1.stacked_imp[[i]])))
    
    fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.RsqN"] <- fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.RsqCS"] / 
                                                               (1 - ((fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.Ey"] ** fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.Ey"]) * 
                                                                    ((1 - fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.Ey"]) ** (1 - fit_1.test.id_imp.fit_1.test.RsqN[i, "fit_1.test.Ey"])) 
                                                                ) ** 2)
  }
  
  fit_1.test.RsqN.mean_imp[r, "fit_1.test.RsqN.mean_imp.n_NA"] <- sum(is.na(fit_1.test.id_imp.fit_1.test.RsqN$fit_1.test.RsqN))
  
  fit_1.test.RsqN.mean_imp[r, "fit_1.test.RsqN.mean_imp"] <- mean(fit_1.test.id_imp.fit_1.test.RsqN$fit_1.test.RsqN, na.rm = TRUE)
  
  ##C-index
  
  ###Bootstrap performance
  
  fit_1.boot.C.mean_imp[r, "fit_1.boot.id_rep"] <- r
  
  fit_1.boot.id_imp.fit_1.boot.C <- as.data.frame(matrix(nrow = n_imp,
                                                         ncol = 2))
  colnames(fit_1.boot.id_imp.fit_1.boot.C) <- c("fit_1.boot.id_imp",
                                                "fit_1.boot.C")
  for (i in 1:n_imp) {
    fit_1.boot.id_imp.fit_1.boot.C[i, "fit_1.boot.id_imp"] <- i
    
    fit_1.boot.id_imp.fit_1.boot.C[i, "fit_1.boot.C"] <- val.prob(fit_1.boot.fit_1.stacked_imp[[i]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]], fit_1.boot.fit_1.stacked_imp[[i]][["y"]], pl = FALSE)[["C (ROC)"]]
  }
  
  fit_1.boot.C.mean_imp[r, "fit_1.boot.C.mean_imp.n_NA"] <- sum(is.na(fit_1.boot.id_imp.fit_1.boot.C$fit_1.boot.C))
  
  fit_1.boot.C.mean_imp[r, "fit_1.boot.C.mean_imp"] <- mean(fit_1.boot.id_imp.fit_1.boot.C$fit_1.boot.C, na.rm = TRUE)
  
  ###Test performance
  
  fit_1.test.C.mean_imp[r, "fit_1.test.id_rep"] <- r
  
  fit_1.test.id_imp.fit_1.test.C <- as.data.frame(matrix(nrow = n_imp,
                                                         ncol = 2))
  colnames(fit_1.test.id_imp.fit_1.test.C) <- c("fit_1.test.id_imp",
                                                "fit_1.test.C")
  for (i in 1:n_imp) {
    fit_1.test.id_imp.fit_1.test.C[i, "fit_1.test.id_imp"] <- i
    
    fit_1.test.id_imp.fit_1.test.C[i, "fit_1.test.C"] <- val.prob(fit_1.test.fit_1.stacked_imp[[i]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]], fit_1.test.fit_1.stacked_imp[[i]][["y"]], pl = FALSE)[["C (ROC)"]]
  }

  fit_1.test.C.mean_imp[r, "fit_1.test.C.mean_imp.n_NA"] <-  sum(is.na(fit_1.test.id_imp.fit_1.test.C$fit_1.test.C))
    
  fit_1.test.C.mean_imp[r, "fit_1.test.C.mean_imp"] <-  mean(fit_1.test.id_imp.fit_1.test.C$fit_1.test.C, na.rm = TRUE)
  
  ##Performance statistics using pt
  
  ###Bootstrap performance
  
  fit_1.boot.pt_stats <- as.data.frame(matrix(nrow = 0,
                                              ncol = 8))
  colnames(fit_1.boot.pt_stats) <- c("fit_1.boot.id_imp",
                                     "fit_1.boot.pt",
                                     "fit_1.boot.Ey",
                                     "fit_1.boot.tp",
                                     "fit_1.boot.tn",
                                     "fit_1.boot.fp",
                                     "fit_1.boot.fn",
                                     "fit_1.boot.n")
  for (i in 1:n_imp){
    for (p in pt) {
      fit_1.boot.id_imp.temp <- i
      
      fit_1.boot.pt.temp <- p
      
      fit_1.boot.Ey.temp <- sum(fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["y"]]) / nrow(as.data.frame(fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]]))
      
      fit_1.boot.tp.temp <- ifelse((fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]] >= fit_1.boot.pt.temp)
                                   &
                                   (fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["y"]] == 1),
                                   1,
                                   0)
      fit_1.boot.tp.temp <- sum(fit_1.boot.tp.temp)
      
      fit_1.boot.tn.temp <- ifelse((fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]] < fit_1.boot.pt.temp)
                                   &
                                   (fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["y"]] == 0),
                                   1,
                                   0)
      fit_1.boot.tn.temp <- sum(fit_1.boot.tn.temp)
      
      fit_1.boot.fp.temp <- ifelse((fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]] >= fit_1.boot.pt.temp)
                                   &
                                   (fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["y"]] == 0),
                                   1,
                                   0)
      fit_1.boot.fp.temp <- sum(fit_1.boot.fp.temp)
      
      fit_1.boot.fn.temp <- ifelse((fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["fit_1.boot.fit_1.stacked_imp.fit_1.pred_prob"]] < fit_1.boot.pt.temp)
                                   &
                                   (fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]][["y"]] == 1),
                                   1,
                                   0)
      fit_1.boot.fn.temp <- sum(fit_1.boot.fn.temp)
      
      fit_1.boot.n.temp <- nrow(as.data.frame(fit_1.boot.fit_1.stacked_imp[[fit_1.boot.id_imp.temp]]))
      
      fit_1.boot.pt_stats.temp <- data.frame(fit_1.boot.id_imp = fit_1.boot.id_imp.temp,
                                             fit_1.boot.pt = fit_1.boot.pt.temp,
                                             fit_1.boot.Ey = fit_1.boot.Ey.temp,
                                             fit_1.boot.tp = fit_1.boot.tp.temp,
                                             fit_1.boot.tn = fit_1.boot.tn.temp,
                                             fit_1.boot.fp = fit_1.boot.fp.temp,
                                             fit_1.boot.fn = fit_1.boot.fn.temp,
                                             fit_1.boot.n = fit_1.boot.n.temp)
      
      fit_1.boot.pt_stats <- rbind(fit_1.boot.pt_stats, fit_1.boot.pt_stats.temp)
    }
  }
  fit_1.boot.pt_stats$fit_1.boot.net_benefit <- NA
  fit_1.boot.pt_stats$fit_1.boot.net_benefit.all <- NA
  fit_1.boot.pt_stats$fit_1.boot.acc <- NA
  fit_1.boot.pt_stats$fit_1.boot.tpr <- NA
  fit_1.boot.pt_stats$fit_1.boot.tnr <- NA
  fit_1.boot.pt_stats$fit_1.boot.ppv <- NA
  fit_1.boot.pt_stats$fit_1.boot.npv <- NA
  for(k in 1:nrow(fit_1.boot.pt_stats)) {
    fit_1.boot.pt_stats[k, "fit_1.boot.net_benefit"] <- (fit_1.boot.pt_stats[k, "fit_1.boot.tp"] / fit_1.boot.pt_stats[k, "fit_1.boot.n"]) - 
                                                        (fit_1.boot.pt_stats[k, "fit_1.boot.fp"] / fit_1.boot.pt_stats[k, "fit_1.boot.n"]) * 
                                                        (fit_1.boot.pt_stats[k, "fit_1.boot.pt"] / (1 - fit_1.boot.pt_stats[k, "fit_1.boot.pt"]))
    
    fit_1.boot.pt_stats[k, "fit_1.boot.net_benefit.all"] <- fit_1.boot.pt_stats[k, "fit_1.boot.Ey"] - 
                                                            ((fit_1.boot.pt_stats[k, "fit_1.boot.pt"] / (1 - fit_1.boot.pt_stats[k, "fit_1.boot.pt"])) * (1 - fit_1.boot.pt_stats[k, "fit_1.boot.Ey"]))
    
    fit_1.boot.pt_stats[k, "fit_1.boot.acc"] <- (fit_1.boot.pt_stats[k, "fit_1.boot.tp"] + fit_1.boot.pt_stats[k, "fit_1.boot.tn"] ) /
                                                (fit_1.boot.pt_stats[k, "fit_1.boot.tp"] + fit_1.boot.pt_stats[k, "fit_1.boot.tn"] + fit_1.boot.pt_stats[k, "fit_1.boot.fp"] + fit_1.boot.pt_stats[k, "fit_1.boot.fn"])
    
    fit_1.boot.pt_stats[k, "fit_1.boot.tpr"] <- fit_1.boot.pt_stats[k, "fit_1.boot.tp"] /
                                                (fit_1.boot.pt_stats[k, "fit_1.boot.tp"] + fit_1.boot.pt_stats[k, "fit_1.boot.fn"])
    
    fit_1.boot.pt_stats[k, "fit_1.boot.tnr"] <- fit_1.boot.pt_stats[k, "fit_1.boot.tn"] /
                                                (fit_1.boot.pt_stats[k, "fit_1.boot.tn"] + fit_1.boot.pt_stats[k, "fit_1.boot.fp"])
    
    fit_1.boot.pt_stats[k, "fit_1.boot.ppv"] <- fit_1.boot.pt_stats[k, "fit_1.boot.tp"] /
                                                (fit_1.boot.pt_stats[k, "fit_1.boot.tp"] + fit_1.boot.pt_stats[k, "fit_1.boot.fp"])
    
    fit_1.boot.pt_stats[k, "fit_1.boot.npv"] <- fit_1.boot.pt_stats[k, "fit_1.boot.tn"] /
                                                (fit_1.boot.pt_stats[k, "fit_1.boot.tn"] + fit_1.boot.pt_stats[k, "fit_1.boot.fn"])
  } 
  for(p in pt) {
    fit_1.boot.id_rep.temp <- r
    
    fit_1.boot.pt.temp <- p
    
    fit_1.boot.net_benefit.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.net_benefit))
    
    fit_1.boot.net_benefit.mean_imp.temp <- mean(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.net_benefit, na.rm = TRUE)
    
    fit_1.boot.net_benefit.all.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.net_benefit.all))
    
    fit_1.boot.net_benefit.all.mean_imp.temp <- mean(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.net_benefit.all, na.rm = TRUE)

    fit_1.boot.acc.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.acc))
        
    fit_1.boot.acc.mean_imp.temp <- mean(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.acc, na.rm = TRUE)
    
    fit_1.boot.tpr.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.tpr))
    
    fit_1.boot.tpr.mean_imp.temp <- mean(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.tpr, na.rm = TRUE)
    
    fit_1.boot.tnr.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.tnr))    
    
    fit_1.boot.tnr.mean_imp.temp <- mean(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.tnr, na.rm = TRUE)    
    
    fit_1.boot.ppv.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.ppv))  
    
    fit_1.boot.ppv.mean_imp.temp <- mean(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.ppv, na.rm = TRUE)  
    
    fit_1.boot.npv.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.npv))  
    
    fit_1.boot.npv.mean_imp.temp <- mean(subset(fit_1.boot.pt_stats, pt == fit_1.boot.pt.temp)$fit_1.boot.npv, na.rm = TRUE)  
    
    fit_1.boot.pt_stats.mean_imp.temp <- data.frame(fit_1.boot.id_rep = fit_1.boot.id_rep.temp,
                                                    fit_1.boot.pt = fit_1.boot.pt.temp,
                                                    fit_1.boot.net_benefit.mean_imp.n_NA = fit_1.boot.net_benefit.mean_imp.n_NA.temp,
                                                    fit_1.boot.net_benefit.mean_imp = fit_1.boot.net_benefit.mean_imp.temp,
                                                    fit_1.boot.net_benefit.all.mean_imp.n_NA = fit_1.boot.net_benefit.all.mean_imp.n_NA.temp,
                                                    fit_1.boot.net_benefit.all.mean_imp = fit_1.boot.net_benefit.all.mean_imp.temp,
                                                    fit_1.boot.acc.mean_imp.n_NA = fit_1.boot.acc.mean_imp.n_NA.temp,
                                                    fit_1.boot.acc.mean_imp = fit_1.boot.acc.mean_imp.temp,
                                                    fit_1.boot.tpr.mean_imp.n_NA = fit_1.boot.tpr.mean_imp.n_NA.temp,
                                                    fit_1.boot.tpr.mean_imp = fit_1.boot.tpr.mean_imp.temp,
                                                    fit_1.boot.tnr.mean_imp.n_NA = fit_1.boot.tnr.mean_imp.n_NA.temp,
                                                    fit_1.boot.tnr.mean_imp = fit_1.boot.tnr.mean_imp.temp,
                                                    fit_1.boot.ppv.mean_imp.n_NA = fit_1.boot.ppv.mean_imp.n_NA.temp,
                                                    fit_1.boot.ppv.mean_imp = fit_1.boot.ppv.mean_imp.temp,
                                                    fit_1.boot.npv.mean_imp.n_NA = fit_1.boot.npv.mean_imp.n_NA.temp,
                                                    fit_1.boot.npv.mean_imp = fit_1.boot.npv.mean_imp.temp)
    
    fit_1.boot.pt_stats.mean_imp <- rbind(fit_1.boot.pt_stats.mean_imp, fit_1.boot.pt_stats.mean_imp.temp)
  }
  
  ###Test performance 
  
  fit_1.test.pt_stats <- as.data.frame(matrix(nrow = 0,
                                              ncol = 8))
  colnames(fit_1.test.pt_stats) <- c("fit_1.test.id_imp",
                                     "fit_1.test.pt",
                                     "fit_1.test.Ey",
                                     "fit_1.test.tp",
                                     "fit_1.test.tn",
                                     "fit_1.test.fp",
                                     "fit_1.test.fn",
                                     "fit_1.test.n")
  for (i in 1:n_imp){
    for (p in pt) {
      fit_1.test.id_imp.temp <- i
      
      fit_1.test.pt.temp <- p
      
      fit_1.test.Ey.temp <- sum(fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["y"]]) / nrow(as.data.frame(fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]]))
      
      fit_1.test.tp.temp <- ifelse((fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]] >= fit_1.test.pt.temp)
                                   &
                                   (fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["y"]] == 1),
                                   1,
                                   0)
      fit_1.test.tp.temp <- sum(fit_1.test.tp.temp)
      
      fit_1.test.tn.temp <- ifelse((fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]] < fit_1.test.pt.temp)
                                   &
                                   (fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["y"]] == 0),
                                   1,
                                   0)
      fit_1.test.tn.temp <- sum(fit_1.test.tn.temp)
      
      fit_1.test.fp.temp <- ifelse((fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]] >= fit_1.test.pt.temp)
                                   &
                                   (fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["y"]] == 0),
                                   1,
                                   0)
      fit_1.test.fp.temp <- sum(fit_1.test.fp.temp)
      
      fit_1.test.fn.temp <- ifelse((fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["fit_1.test.fit_1.stacked_imp.fit_1.pred_prob"]] < fit_1.test.pt.temp)
                                   &
                                   (fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]][["y"]] == 1),
                                   1,
                                   0)
      fit_1.test.fn.temp <- sum(fit_1.test.fn.temp)
      
      fit_1.test.n.temp <- nrow(as.data.frame(fit_1.test.fit_1.stacked_imp[[fit_1.test.id_imp.temp]]))
      
      fit_1.test.pt_stats.temp <- data.frame(fit_1.test.id_imp = fit_1.test.id_imp.temp,
                                             fit_1.test.pt = fit_1.test.pt.temp,
                                             fit_1.test.Ey = fit_1.test.Ey.temp,
                                             fit_1.test.tp = fit_1.test.tp.temp,
                                             fit_1.test.tn = fit_1.test.tn.temp,
                                             fit_1.test.fp = fit_1.test.fp.temp,
                                             fit_1.test.fn = fit_1.test.fn.temp,
                                             fit_1.test.n = fit_1.test.n.temp)
      
      fit_1.test.pt_stats <- rbind(fit_1.test.pt_stats, fit_1.test.pt_stats.temp)
    }
  }
  fit_1.test.pt_stats$fit_1.test.net_benefit <- NA
  fit_1.test.pt_stats$fit_1.test.net_benefit.all <- NA
  fit_1.test.pt_stats$fit_1.test.acc <- NA
  fit_1.test.pt_stats$fit_1.test.tpr <- NA
  fit_1.test.pt_stats$fit_1.test.tnr <- NA
  fit_1.test.pt_stats$fit_1.test.ppv <- NA
  fit_1.test.pt_stats$fit_1.test.npv <- NA
  for(k in 1:nrow(fit_1.test.pt_stats)) {
    fit_1.test.pt_stats[k, "fit_1.test.net_benefit"] <- (fit_1.test.pt_stats[k, "fit_1.test.tp"] / fit_1.test.pt_stats[k, "fit_1.test.n"]) - 
                                                        (fit_1.test.pt_stats[k, "fit_1.test.fp"] / fit_1.test.pt_stats[k, "fit_1.test.n"]) * 
                                                        (fit_1.test.pt_stats[k, "fit_1.test.pt"] / (1 - fit_1.test.pt_stats[k, "fit_1.test.pt"]))
    
    fit_1.test.pt_stats[k, "fit_1.test.net_benefit.all"] <- fit_1.test.pt_stats[k, "fit_1.test.Ey"] - 
                                                            ((fit_1.test.pt_stats[k, "fit_1.test.pt"] / (1 - fit_1.test.pt_stats[k, "fit_1.test.pt"])) * (1 - fit_1.test.pt_stats[k, "fit_1.test.Ey"]))
    
    fit_1.test.pt_stats[k, "fit_1.test.acc"] <- (fit_1.test.pt_stats[k, "fit_1.test.tp"] + fit_1.test.pt_stats[k, "fit_1.test.tn"]) /
                                                (fit_1.test.pt_stats[k, "fit_1.test.tp"] + fit_1.test.pt_stats[k, "fit_1.test.tn"] + fit_1.test.pt_stats[k, "fit_1.test.fp"] + fit_1.test.pt_stats[k, "fit_1.test.fn"])
    
    fit_1.test.pt_stats[k, "fit_1.test.tpr"] <- fit_1.test.pt_stats[k, "fit_1.test.tp"] /
                                                (fit_1.test.pt_stats[k, "fit_1.test.tp"] + fit_1.test.pt_stats[k, "fit_1.test.fn"])
    
    fit_1.test.pt_stats[k, "fit_1.test.tnr"] <- fit_1.test.pt_stats[k, "fit_1.test.tn"] /
                                                (fit_1.test.pt_stats[k, "fit_1.test.tn"] + fit_1.test.pt_stats[k, "fit_1.test.fp"])
    
    fit_1.test.pt_stats[k, "fit_1.test.ppv"] <- fit_1.test.pt_stats[k, "fit_1.test.tp"] /
                                                (fit_1.test.pt_stats[k, "fit_1.test.tp"] + fit_1.test.pt_stats[k, "fit_1.test.fp"])
    
    fit_1.test.pt_stats[k, "fit_1.test.npv"] <- fit_1.test.pt_stats[k, "fit_1.test.tn"] /
                                                (fit_1.test.pt_stats[k, "fit_1.test.tn"] + fit_1.test.pt_stats[k, "fit_1.test.fn"])
  } 
  for(p in pt) {
    fit_1.test.id_rep.temp <- r
    
    fit_1.test.pt.temp <- p
    
    fit_1.test.net_benefit.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.net_benefit))
    
    fit_1.test.net_benefit.mean_imp.temp <- mean(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.net_benefit, na.rm = TRUE)
    
    fit_1.test.net_benefit.all.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.net_benefit.all))
    
    fit_1.test.net_benefit.all.mean_imp.temp <- mean(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.net_benefit.all, na.rm = TRUE)
    
    fit_1.test.acc.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.acc))
    
    fit_1.test.acc.mean_imp.temp <- mean(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.acc, na.rm = TRUE)
    
    fit_1.test.tpr.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.tpr))
    
    fit_1.test.tpr.mean_imp.temp <- mean(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.tpr, na.rm = TRUE)
    
    fit_1.test.tnr.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.tnr))    
    
    fit_1.test.tnr.mean_imp.temp <- mean(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.tnr, na.rm = TRUE)    
    
    fit_1.test.ppv.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.ppv))  
    
    fit_1.test.ppv.mean_imp.temp <- mean(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.ppv, na.rm = TRUE)  
    
    fit_1.test.npv.mean_imp.n_NA.temp <- sum(is.na(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.npv))  
    
    fit_1.test.npv.mean_imp.temp <- mean(subset(fit_1.test.pt_stats, pt == fit_1.test.pt.temp)$fit_1.test.npv, na.rm = TRUE)  
    
    fit_1.test.pt_stats.mean_imp.temp <- data.frame(fit_1.test.id_rep = fit_1.test.id_rep.temp,
                                                    fit_1.test.pt = fit_1.test.pt.temp,
                                                    fit_1.test.net_benefit.mean_imp.n_NA = fit_1.test.net_benefit.mean_imp.n_NA.temp,
                                                    fit_1.test.net_benefit.mean_imp = fit_1.test.net_benefit.mean_imp.temp,
                                                    fit_1.test.net_benefit.all.mean_imp.n_NA = fit_1.test.net_benefit.all.mean_imp.n_NA.temp,
                                                    fit_1.test.net_benefit.all.mean_imp = fit_1.test.net_benefit.all.mean_imp.temp,
                                                    fit_1.test.acc.mean_imp.n_NA = fit_1.test.acc.mean_imp.n_NA.temp,
                                                    fit_1.test.acc.mean_imp = fit_1.test.acc.mean_imp.temp,
                                                    fit_1.test.tpr.mean_imp.n_NA = fit_1.test.tpr.mean_imp.n_NA.temp,
                                                    fit_1.test.tpr.mean_imp = fit_1.test.tpr.mean_imp.temp,
                                                    fit_1.test.tnr.mean_imp.n_NA = fit_1.test.tnr.mean_imp.n_NA.temp,
                                                    fit_1.test.tnr.mean_imp = fit_1.test.tnr.mean_imp.temp,
                                                    fit_1.test.ppv.mean_imp.n_NA = fit_1.test.ppv.mean_imp.n_NA.temp,
                                                    fit_1.test.ppv.mean_imp = fit_1.test.ppv.mean_imp.temp,
                                                    fit_1.test.npv.mean_imp.n_NA = fit_1.test.npv.mean_imp.n_NA.temp,
                                                    fit_1.test.npv.mean_imp = fit_1.test.npv.mean_imp.temp)
    
    fit_1.test.pt_stats.mean_imp <- rbind(fit_1.test.pt_stats.mean_imp, fit_1.test.pt_stats.mean_imp.temp)
  }
}

#Calculating optimism

##Rsq Nagelkerke

sum(fit_1.boot.RsqN.mean_imp$fit_1.boot.RsqN.mean_imp.n_NA)
sum(fit_1.boot.RsqN.mean_imp$fit_1.test.RsqN.mean_imp.n_NA)
fit_1.o.RsqN.mean_imp <- fit_1.boot.RsqN.mean_imp$fit_1.boot.RsqN.mean_imp - fit_1.test.RsqN.mean_imp$fit_1.test.RsqN.mean_imp
fit_1.o.RsqN.mean_imp <- as.data.frame(cbind(fit_1.boot.RsqN.mean_imp$fit_1.boot.id_rep, 
                                             fit_1.o.RsqN.mean_imp))
colnames(fit_1.o.RsqN.mean_imp) <- c("o.fit_1.id_rep",
                                     "o.fit_1.RsqN.mean_imp")

##C-index

sum(fit_1.boot.C.mean_imp$fit_1.boot.C.mean_imp.n_NA)
sum(fit_1.boot.C.mean_imp$fit_1.test.C.mean_imp.n_NA)
fit_1.o.C.mean_imp <- fit_1.boot.C.mean_imp$fit_1.boot.C.mean_imp - fit_1.test.C.mean_imp$fit_1.test.C.mean_imp
fit_1.o.C.mean_imp <- as.data.frame(cbind(fit_1.boot.C.mean_imp$fit_1.boot.id_rep, 
                                          fit_1.o.C.mean_imp))
colnames(fit_1.o.C.mean_imp) <- c("fit_1.o.id_rep",
                                  "fit_1.o.C.mean_imp")

##Performance statistics using pt

fit_1.boot.test.pt_stats.mean_imp <- merge(fit_1.boot.pt_stats.mean_imp,
                                           fit_1.test.pt_stats.mean_imp,
                                           by.x = c("fit_1.boot.id_rep",
                                                    "fit_1.boot.pt"),
                                           by.y = c("fit_1.test.id_rep",
                                                    "fit_1.test.pt"))
colnames(fit_1.boot.test.pt_stats.mean_imp)[1] <- "fit_1.boot.test.id_rep"
colnames(fit_1.boot.test.pt_stats.mean_imp)[2] <- "fit_1.boot.test.pt"

fit_1.boot.test.pt_stats.mean_imp.0_NA <- subset(fit_1.boot.test.pt_stats.mean_imp, (fit_1.boot.net_benefit.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.test.net_benefit.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.boot.net_benefit.all.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.test.net_benefit.all.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.boot.acc.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.test.acc.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.boot.tpr.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.test.tpr.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.boot.tnr.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.test.tnr.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.boot.ppv.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.test.ppv.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.boot.npv.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_1.test.npv.mean_imp.n_NA == 0))

for (p in pt){
  print(p)
  print(nrow(subset(fit_1.boot.test.pt_stats.mean_imp, fit_1.boot.test.pt == p)))
  print(nrow(subset(fit_1.boot.test.pt_stats.mean_imp.0_NA, fit_1.boot.test.pt == p)))
}

fit_1.o.pt_stats.mean_imp <- as.data.frame(matrix(nrow = nrow(fit_1.boot.test.pt_stats.mean_imp.0_NA),
                                                  ncol = 9))
colnames(fit_1.o.pt_stats.mean_imp) <- c("fit_1.o.id_rep",
                                         "fit_1.o.pt", 
                                         "fit_1.o.net_benefit.mean_imp",
                                         "fit_1.o.net_benefit.all.mean_imp",
                                         "fit_1.o.acc.mean_imp",
                                         "fit_1.o.tpr.mean_imp",
                                         "fit_1.o.tnr.mean_imp",
                                         "fit_1.o.ppv.mean_imp",
                                         "fit_1.o.npv.mean_imp")
fit_1.o.pt_stats.mean_imp$fit_1.o.id_rep <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.test.id_rep
fit_1.o.pt_stats.mean_imp$fit_1.o.pt <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.test.pt
fit_1.o.pt_stats.mean_imp$fit_1.o.net_benefit.mean_imp <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.net_benefit.mean_imp - fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.test.net_benefit.mean_imp
fit_1.o.pt_stats.mean_imp$fit_1.o.net_benefit.all.mean_imp <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.net_benefit.all.mean_imp - fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.test.net_benefit.all.mean_imp
fit_1.o.pt_stats.mean_imp$fit_1.o.acc.mean_imp <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.acc.mean_imp - fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.test.acc.mean_imp
fit_1.o.pt_stats.mean_imp$fit_1.o.tpr.mean_imp <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.tpr.mean_imp - fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.test.tpr.mean_imp
fit_1.o.pt_stats.mean_imp$fit_1.o.tnr.mean_imp <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.tnr.mean_imp - fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.test.tnr.mean_imp
fit_1.o.pt_stats.mean_imp$fit_1.o.ppv.mean_imp <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.ppv.mean_imp - fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.test.ppv.mean_imp
fit_1.o.pt_stats.mean_imp$fit_1.o.npv.mean_imp <- fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.boot.npv.mean_imp - fit_1.boot.test.pt_stats.mean_imp.0_NA$fit_1.test.npv.mean_imp

#Correcting apparent performance statistics with average optimism

##Rsq Nagelkerke

fit_1.oc.RsqN.mean_imp <- fit_1.app.RsqN.mean_imp - mean(fit_1.o.RsqN.mean_imp$o.fit_1.RsqN.mean_imp)
fit_1.oc.RsqN.mean_imp

##C-index

fit_1.oc.C.mean_imp <- fit_1.app.C.mean_imp - mean(fit_1.o.C.mean_imp$fit_1.o.C.mean_imp)
fit_1.oc.C.mean_imp

##Performance statistics using pt

fit_1.o.mean.pt_stats.mean_imp <- as.data.frame(matrix(nrow = length(pt),
                                                       ncol = 8))
colnames(fit_1.o.mean.pt_stats.mean_imp) <- c("fit_1.o.mean.pt", 
                                              "fit_1.o.mean.net_benefit.mean_imp",
                                              "fit_1.o.mean.net_benefit.all.mean_imp",
                                              "fit_1.o.mean.acc.mean_imp",
                                              "fit_1.o.mean.tpr.mean_imp",
                                              "fit_1.o.mean.tnr.mean_imp",
                                              "fit_1.o.mean.ppv.mean_imp",
                                              "fit_1.o.mean.npv.mean_imp")
fit_1.o.mean.pt_stats.mean_imp$fit_1.o.mean.pt <- pt
for(k in 1:nrow(fit_1.o.mean.pt_stats.mean_imp)) {
  fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.net_benefit.mean_imp"] <- mean(subset(fit_1.o.pt_stats.mean_imp, fit_1.o.pt == fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.pt"])$fit_1.o.net_benefit.mean_imp)
  
  fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.net_benefit.all.mean_imp"] <- mean(subset(fit_1.o.pt_stats.mean_imp, fit_1.o.pt == fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.pt"])$fit_1.o.net_benefit.all.mean_imp)
  
  fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.acc.mean_imp"] <- mean(subset(fit_1.o.pt_stats.mean_imp, fit_1.o.pt == fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.pt"])$fit_1.o.acc.mean_imp)
  
  fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.tpr.mean_imp"] <- mean(subset(fit_1.o.pt_stats.mean_imp, fit_1.o.pt == fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.pt"])$fit_1.o.tpr.mean_imp)
  
  fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.tnr.mean_imp"] <- mean(subset(fit_1.o.pt_stats.mean_imp, fit_1.o.pt == fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.pt"])$fit_1.o.tnr.mean_imp)
  
  fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.ppv.mean_imp"] <- mean(subset(fit_1.o.pt_stats.mean_imp, fit_1.o.pt == fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.pt"])$fit_1.o.ppv.mean_imp)
  
  fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.npv.mean_imp"] <- mean(subset(fit_1.o.pt_stats.mean_imp, fit_1.o.pt == fit_1.o.mean.pt_stats.mean_imp[k, "fit_1.o.mean.pt"])$fit_1.o.npv.mean_imp)
}

fit_1.oc.pt_stats.mean_imp <- fit_1.app.pt_stats.mean_imp[, 2:8] - fit_1.o.mean.pt_stats.mean_imp[, 2:8]
fit_1.oc.pt_stats.mean_imp <- as.data.frame(cbind(fit_1.app.pt_stats.mean_imp$fit_1.app.pt,
                                                  fit_1.oc.pt_stats.mean_imp))
colnames(fit_1.oc.pt_stats.mean_imp) <- c("fit_1.oc.pt", 
                                          "fit_1.oc.net_benefit.mean_imp",
                                          "fit_1.oc.net_benefit.all.mean_imp",
                                          "fit_1.oc.acc.mean_imp",
                                          "fit_1.oc.tpr.mean_imp",
                                          "fit_1.oc.tnr.mean_imp",
                                          "fit_1.oc.ppv.mean_imp",
                                          "fit_1.oc.npv.mean_imp")
fit_1.oc.pt_stats.mean_imp

### NCC MERP index ----

table(data$y, data$NCC_MERP_index.E_or_above_1, exclude = NULL)
54 / (54 + 26 + 8)
26 / (54 + 26 + 8)
8 / (54 + 26 + 8)

summary(subset(data, NCC_MERP_index.E_or_above_1 == 0)$fit_1.pred_prob)
summary(subset(data, NCC_MERP_index.E_or_above_1 == 1)$fit_1.pred_prob)

sum(is.na(subset(data, NCC_MERP_index.E_or_above_1 == 0)$fit_1.pred_prob))
14 / 54
sum(is.na(subset(data, NCC_MERP_index.E_or_above_1 == 1)$fit_1.pred_prob))
7 / 26

nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_1.pred_prob < 0.0333)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_1.pred_prob < 0.0333))) / 40
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_1.pred_prob < 0.0333)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_1.pred_prob < 0.0333))) / 19

nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_1.pred_prob < 0.0500)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_1.pred_prob < 0.0500))) / 40
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_1.pred_prob < 0.0500)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_1.pred_prob < 0.0500))) / 19

nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_1.pred_prob < 0.1000)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_1.pred_prob < 0.1000))) / 40
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_1.pred_prob < 0.1000)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_1.pred_prob < 0.1000))) / 19

## Model 2 ----

### Imputation ----

fit_2.imp <- aregImpute(~ y +
                          age.std +
                          sex.male_1 +
                          n_prescribed_medicines.std +
                          high_risk_medicines.yes_1 +
                          eGFR.below_60_1 +
                          out_specialty_current_MUMC.group_A_1 +
                          out_specialty_current_MUMC.group_B_1 +
                          n_specialty_MUMC.std +
                          health_literacy_composite.insufficient_1,
                        x = TRUE,
                        data = data,
                        n.impute = n_imp,
                        nk = 3)

for(i in 1:n_imp) {
  print(paste("imputation", i))
  print(table(impute.transcan(fit_2.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$eGFR.below_60_1))
  print(prop.table(table(impute.transcan(fit_2.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$eGFR.below_60_1)))
}

for(i in 1:n_imp) {
  print(paste("imputation", i))
  print(table(impute.transcan(fit_2.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$health_literacy_composite.insufficient_1))
  print(prop.table(table(impute.transcan(fit_2.imp, imputation = i, data = data, list.out = TRUE, pr = FALSE)$health_literacy_composite.insufficient_1)))
}

fit_2.stacked_imp <- list()
for (i in 1:n_imp) {
  fit_2.stacked_imp[[i]] <- impute.transcan(fit_2.imp, 
                                            imputation = i, 
                                            data = data, 
                                            list.out = TRUE, 
                                            pr = FALSE)
}

### Fit ----

fit_2 <- fit.mult.impute(y ~
                           age.std +
                           sex.male_1 +
                           n_prescribed_medicines.std +
                           high_risk_medicines.yes_1 +
                           eGFR.below_60_1 +                  
                           n_specialty_MUMC.std +
                           health_literacy_composite.insufficient_1,
                         lrm,
                         fit_2.imp,
                         data = data)
fit_2
exp(fit_2$coefficients)
exp(fit_2$coefficients - 1.96 * sqrt(diag(fit_2$var)))
exp(fit_2$coefficients + 1.96 * sqrt(diag(fit_2$var)))

fit_2 <- fit.mult.impute(y ~
                           age.std +
                           sex.male_1 +
                           n_prescribed_medicines.std +
                           high_risk_medicines.yes_1 +
                           n_specialty_MUMC.std +
                           health_literacy_composite.insufficient_1,
                         lrm,
                         fit_2.imp,
                         data = data)
fit_2
exp(fit_2$coefficients)
exp(fit_2$coefficients - 1.96 * sqrt(diag(fit_2$var)))
exp(fit_2$coefficients + 1.96 * sqrt(diag(fit_2$var)))

fit_2 <- fit.mult.impute(y ~
                           age.std +
                           n_prescribed_medicines.std +
                           high_risk_medicines.yes_1 +
                           n_specialty_MUMC.std +
                           health_literacy_composite.insufficient_1,
                         lrm,
                         fit_2.imp,
                         data = data)
fit_2
exp(fit_2$coefficients)
exp(fit_2$coefficients - 1.96 * sqrt(diag(fit_2$var)))
exp(fit_2$coefficients + 1.96 * sqrt(diag(fit_2$var)))

fit_2 <- fit.mult.impute(y ~
                           age.std +
                           n_prescribed_medicines.std +
                           high_risk_medicines.yes_1 +
                           health_literacy_composite.insufficient_1,
                         lrm,
                         fit_2.imp,
                         data = data)
fit_2
exp(fit_2$coefficients)
exp(fit_2$coefficients - 1.96 * sqrt(diag(fit_2$var)))
exp(fit_2$coefficients + 1.96 * sqrt(diag(fit_2$var)))

fit_2.stacked_imp.fit_2 <- list()
for(i in 1:n_imp) {
  fit_2.stacked_imp.fit_2[[i]] <- lrm(y ~ 
                                        age.std +
                                        n_prescribed_medicines.std +
                                        high_risk_medicines.yes_1 +
                                        health_literacy_composite.insufficient_1,
                                      data = as.data.frame(fit_2.stacked_imp[[i]]))
}

lrtest(fit_1, fit_2)

### Assessing linearity ----

fit_2.nl <- fit.mult.impute(y ~
                              rcs(age.std, 3) +
                              rcs(n_prescribed_medicines.std, 3) +
                              high_risk_medicines.yes_1 +
                              health_literacy_composite.insufficient_1,
                            lrm,
                            fit_2.imp,
                            data = data)
lrtest(fit_2, fit_2.nl)

### Assessing additivity ----

fit_2.ia <- fit.mult.impute(y ~
                              age.std +
                              n_prescribed_medicines.std +
                              high_risk_medicines.yes_1 +
                              health_literacy_composite.insufficient_1 +
                              age.std:n_prescribed_medicines.std +
                              age.std:high_risk_medicines.yes_1 +
                              age.std:health_literacy_composite.insufficient_1,
                            lrm,
                            fit_2.imp,
                            data = data)
lrtest(fit_2, fit_2.ia)

### Predicted probabilities ----

data$fit_2.pred_prob <- predict(fit_2, data, type = "fitted.ind")

for (i in 1:n_imp) {
  fit_2.stacked_imp[[i]][["fit_2.pred_prob"]] <- as.vector(predict(fit_2, fit_2.stacked_imp[[i]], type = "fitted.ind"))
}#Pooled model

for (i in 1:n_imp) {
  fit_2.stacked_imp[[i]][["fit_2.stacked_imp.fit_2.pred_prob"]] <- as.vector(predict(fit_2.stacked_imp.fit_2[[i]], fit_2.stacked_imp[[i]], type = "fitted.ind"))
}#Respective model imputed dataset i

### Linear predictor ----

data$fit_2.lp <- predict(fit_2, data, type = "lp")

for (i in 1:n_imp) {
  fit_2.stacked_imp[[i]][["fit_2.lp"]] <- as.vector(predict(fit_2, fit_2.stacked_imp[[i]], type = "lp"))
}#Pooled model

for (i in 1:n_imp) {
  fit_2.stacked_imp[[i]][["fit_2.stacked_imp.fit_2.lp"]] <- as.vector(predict(fit_2.stacked_imp.fit_2[[i]], fit_2.stacked_imp[[i]], type = "lp"))
}#Respective model imputed dataset i

### Figure boxplots ----

for(i in 1:n_imp){
  fit_2.stacked_imp[[i]][["id"]] <- data$id
}
temp <- data.frame(matrix(nrow = 0,
                          ncol = 4))
colnames(temp) <- c("id_imp",
                    "id",
                    "y",
                    "fit_2.pred_prob")
for (i in 1:n_imp) {
  id_imp.temp <- rep(i, nrow(as.data.frame(fit_2.stacked_imp[[i]])))
  
  id.temp <- fit_2.stacked_imp[[i]][["id"]]
  
  y.temp <- fit_2.stacked_imp[[i]][["y"]]
  
  fit_2.pred_prob.temp <- fit_2.stacked_imp[[i]][["fit_2.pred_prob"]]
  
  temp.temp <- data.frame(id_imp = id_imp.temp,
                          id = id.temp,
                          y = y.temp,
                          fit_2.pred_prob = fit_2.pred_prob.temp)
  
  temp <- rbind(temp, 
                temp.temp)
}
y.fit_2.pred_prob.mean_imp <- aggregate(temp[, c("y",
                                                 "fit_2.pred_prob")],
                                        list(temp[, c("id")]),
                                        mean)
y.fit_2.pred_prob.mean_imp <- y.fit_2.pred_prob.mean_imp[, 2:3]
colnames(y.fit_2.pred_prob.mean_imp)[2] <- "fit_2.pred_prob.mean_imp"

summary(subset(y.fit_2.pred_prob.mean_imp, y == 0)$fit_2.pred_prob.mean_imp)
summary(subset(y.fit_2.pred_prob.mean_imp, y == 1)$fit_2.pred_prob.mean_imp)

plot.fit_2 <- 
  ggplot(y.fit_2.pred_prob.mean_imp, aes(x = fit_2.pred_prob.mean_imp, y = y, group = y)) +
    geom_boxplot(width = 0.5) +
    scale_x_continuous(name = "Predicted probability (%)",
                       limits = c(0, 0.30),
                       breaks = seq(0, 0.30, 0.01),
                       labels = seq(0, 0.30, 0.01) * 100) +
    scale_y_continuous(name = NULL,
                       breaks = c(0, 1),
                       labels = c("0" = "No medication policy change",
                                  "1" = "Medication policy change")) +
    ggtitle("Model 2") +
    theme_bw() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold")) 

plot.fit_1_and_fit_2 <- plot_grid(plot.fit_1,
                                  plot.fit_2,
                                  nrow = 2,
                                  ncol = 1)

### Screening rate using pt ----

for (p in pt) {
  print(p)
  print(prop.table(table(y.fit_2.pred_prob.mean_imp$fit_2.pred_prob.mean_imp >= p)))
}

### Apparent performance statistics ----

#Rsq Nagelkerke 

fit_2.app.RsqN.mean_imp <- fit_2[["stats"]][["R2"]]
fit_2.app.RsqN.mean_imp

#C-index 

fit_2.app.C.mean_imp <- fit_2[["stats"]][["C"]]
fit_2.app.C.mean_imp

#Performance statistics using pt 

fit_2.app.pt_stats <- as.data.frame(matrix(nrow = 0,
                                           ncol = 8))
colnames(fit_2.app.pt_stats) <- c("fit_2.app.id_imp",
                                  "fit_2.app.pt",
                                  "fit_2.app.Ey",
                                  "fit_2.app.tp",
                                  "fit_2.app.tn",
                                  "fit_2.app.fp",
                                  "fit_2.app.fn",
                                  "fit_2.app.n")
for (i in 1:n_imp){
  for (p in pt) {
    fit_2.app.id_imp.temp <- i
    
    fit_2.app.pt.temp <- p
    
    fit_2.app.Ey.temp <- sum(fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["y"]]) / nrow(as.data.frame(fit_2.stacked_imp[[fit_2.app.id_imp.temp]]))
    
    fit_2.app.tp.temp <- ifelse((fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["fit_2.stacked_imp.fit_2.pred_prob"]] >= fit_2.app.pt.temp)
                                &
                                (fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["y"]] == 1),
                                1,
                                0)
    fit_2.app.tp.temp <- sum(fit_2.app.tp.temp)
    
    fit_2.app.tn.temp <- ifelse((fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["fit_2.stacked_imp.fit_2.pred_prob"]] < fit_2.app.pt.temp)
                                &
                                (fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["y"]] == 0),
                                1,
                                0)
    fit_2.app.tn.temp <- sum(fit_2.app.tn.temp)
    
    fit_2.app.fp.temp <- ifelse((fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["fit_2.stacked_imp.fit_2.pred_prob"]] >= fit_2.app.pt.temp)
                                &
                                (fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["y"]] == 0),
                                1,
                                0)
    fit_2.app.fp.temp <- sum(fit_2.app.fp.temp)
    
    fit_2.app.fn.temp <- ifelse((fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["fit_2.stacked_imp.fit_2.pred_prob"]] < fit_2.app.pt.temp)
                                &
                                (fit_2.stacked_imp[[fit_2.app.id_imp.temp]][["y"]] == 1),
                                1,
                                0)
    fit_2.app.fn.temp <- sum(fit_2.app.fn.temp)
    
    fit_2.app.n.temp <- nrow(as.data.frame(fit_2.stacked_imp[[fit_2.app.id_imp.temp]]))
    
    fit_2.app.pt_stats.temp <- data.frame(fit_2.app.id_imp = fit_2.app.id_imp.temp,
                                          fit_2.app.pt = fit_2.app.pt.temp,
                                          fit_2.app.Ey = fit_2.app.Ey.temp,
                                          fit_2.app.tp = fit_2.app.tp.temp,
                                          fit_2.app.tn = fit_2.app.tn.temp,
                                          fit_2.app.fp = fit_2.app.fp.temp,
                                          fit_2.app.fn = fit_2.app.fn.temp,
                                          fit_2.app.n = fit_2.app.n.temp)
    
    fit_2.app.pt_stats <- rbind(fit_2.app.pt_stats, fit_2.app.pt_stats.temp)
  }
}
fit_2.app.pt_stats$fit_2.app.net_benefit <- NA
fit_2.app.pt_stats$fit_2.app.net_benefit.all <- NA
fit_2.app.pt_stats$fit_2.app.acc <- NA
fit_2.app.pt_stats$fit_2.app.tpr <- NA
fit_2.app.pt_stats$fit_2.app.tnr <- NA
fit_2.app.pt_stats$fit_2.app.ppv <- NA
fit_2.app.pt_stats$fit_2.app.npv <- NA
for(k in 1:nrow(fit_2.app.pt_stats)) {
  fit_2.app.pt_stats[k, "fit_2.app.net_benefit"] <- (fit_2.app.pt_stats[k, "fit_2.app.tp"] / fit_2.app.pt_stats[k, "fit_2.app.n"]) - 
                                                    (fit_2.app.pt_stats[k, "fit_2.app.fp"] / fit_2.app.pt_stats[k, "fit_2.app.n"]) * 
                                                    (fit_2.app.pt_stats[k, "fit_2.app.pt"] / (1 - fit_2.app.pt_stats[k, "fit_2.app.pt"]))
  
  fit_2.app.pt_stats[k, "fit_2.app.net_benefit.all"] <- fit_2.app.pt_stats[k, "fit_2.app.Ey"] - 
                                                        ( ( fit_2.app.pt_stats[k, "fit_2.app.pt"] / (1 - fit_2.app.pt_stats[k, "fit_2.app.pt"]) ) * (1 - fit_2.app.pt_stats[k, "fit_2.app.Ey"] ) )
  
  fit_2.app.pt_stats[k, "fit_2.app.acc"] <- (fit_2.app.pt_stats[k, "fit_2.app.tp"] +  fit_2.app.pt_stats[k, "fit_2.app.tn"]) /
                                            (fit_2.app.pt_stats[k, "fit_2.app.tp"] + fit_2.app.pt_stats[k, "fit_2.app.tn"] + fit_2.app.pt_stats[k, "fit_2.app.fp"] + fit_2.app.pt_stats[k, "fit_2.app.fn"])
  
  fit_2.app.pt_stats[k, "fit_2.app.tpr"] <- fit_2.app.pt_stats[k, "fit_2.app.tp"] /
                                            (fit_2.app.pt_stats[k, "fit_2.app.tp"] + fit_2.app.pt_stats[k, "fit_2.app.fn"])
  
  fit_2.app.pt_stats[k, "fit_2.app.tnr"] <- fit_2.app.pt_stats[k, "fit_2.app.tn"] /
                                            (fit_2.app.pt_stats[k, "fit_2.app.tn"] + fit_2.app.pt_stats[k, "fit_2.app.fp"])
  
  fit_2.app.pt_stats[k, "fit_2.app.ppv"] <- fit_2.app.pt_stats[k, "fit_2.app.tp"] /
                                            (fit_2.app.pt_stats[k, "fit_2.app.tp"] + fit_2.app.pt_stats[k, "fit_2.app.fp"])
  
  fit_2.app.pt_stats[k, "fit_2.app.npv"] <- fit_2.app.pt_stats[k, "fit_2.app.tn"] /
                                            (fit_2.app.pt_stats[k, "fit_2.app.tn"] + fit_2.app.pt_stats[k, "fit_2.app.fn"])
}  
fit_2.app.pt_stats.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                    ncol = 7))
colnames(fit_2.app.pt_stats.mean_imp) <- c("fit_2.app.pt", 
                                           "fit_2.app.net_benefit.mean_imp", 
                                           "fit_2.app.net_benefit.all.mean_imp",
                                           "fit_2.app.acc.mean_imp",
                                           "fit_2.app.tpr.mean_imp",
                                           "fit_2.app.tnr.mean_imp",
                                           "fit_2.app.ppv.mean_imp")  
for(p in pt) {
  fit_2.app.pt.temp <- p
  
  fit_2.app.net_benefit.mean_imp.temp <- mean(subset(fit_2.app.pt_stats, pt == fit_2.app.pt.temp)$fit_2.app.net_benefit)
  
  fit_2.app.net_benefit.all.mean_imp.temp <- mean(subset(fit_2.app.pt_stats, pt == fit_2.app.pt.temp)$fit_2.app.net_benefit.all)
  
  fit_2.app.acc.mean_imp.temp <- mean(subset(fit_2.app.pt_stats, pt == fit_2.app.pt.temp)$fit_2.app.acc)
  
  fit_2.app.tpr.mean_imp.temp <- mean(subset(fit_2.app.pt_stats, pt == fit_2.app.pt.temp)$fit_2.app.tpr)
  
  fit_2.app.tnr.mean_imp.temp <- mean(subset(fit_2.app.pt_stats, pt == fit_2.app.pt.temp)$fit_2.app.tnr)    
  
  fit_2.app.ppv.mean_imp.temp <- mean(subset(fit_2.app.pt_stats, pt == fit_2.app.pt.temp)$fit_2.app.ppv)  
  
  fit_2.app.npv.mean_imp.temp <- mean(subset(fit_2.app.pt_stats, pt == fit_2.app.pt.temp)$fit_2.app.npv)  
  
  fit_2.app.pt_stats.mean_imp.temp <- data.frame(fit_2.app.pt = fit_2.app.pt.temp,
                                                 fit_2.app.net_benefit.mean_imp = fit_2.app.net_benefit.mean_imp.temp,
                                                 fit_2.app.net_benefit.all.mean_imp = fit_2.app.net_benefit.all.mean_imp.temp,
                                                 fit_2.app.acc.mean_imp = fit_2.app.acc.mean_imp.temp,
                                                 fit_2.app.tpr.mean_imp = fit_2.app.tpr.mean_imp.temp,
                                                 fit_2.app.tnr.mean_imp = fit_2.app.tnr.mean_imp.temp,
                                                 fit_2.app.ppv.mean_imp = fit_2.app.ppv.mean_imp.temp,
                                                 fit_2.app.npv.mean_imp = fit_2.app.npv.mean_imp.temp)
  
  fit_2.app.pt_stats.mean_imp <- rbind(fit_2.app.pt_stats.mean_imp, fit_2.app.pt_stats.mean_imp.temp)
}
fit_2.app.pt_stats.mean_imp

### Optimism-corrected performance statistics ----

#Creating objects to capture output loop

##Rsq Nagelkerke

###Bootstrap performance

fit_2.boot.RsqN.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                 ncol = 3))
colnames(fit_2.boot.RsqN.mean_imp) <- c("fit_2.boot.id_rep",
                                        "fit_2.boot.RsqN.mean_imp.n_NA",
                                        "fit_2.boot.RsqN.mean_imp")

###Test performance

fit_2.test.RsqN.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                 ncol = 3))
colnames(fit_2.test.RsqN.mean_imp) <- c("fit_2.test.id_rep",
                                        "fit_2.test.RsqN.mean_imp.n_NA",
                                        "fit_2.test.RsqN.mean_imp")

##C-index

###Bootstrap performance

fit_2.boot.C.mean_imp <- as.data.frame(matrix(nrow = 0,
                                              ncol = 3))
colnames(fit_2.boot.C.mean_imp) <- c("fit_2.boot.id_rep",
                                     "fit_2.boot.C.mean_imp.n_NA",
                                     "fit_2.boot.C.mean_imp")

###Test performance

fit_2.test.C.mean_imp <- as.data.frame(matrix(nrow = 0,
                                              ncol = 3))
colnames(fit_2.test.C.mean_imp) <- c("fit_2.test.id_rep",
                                     "fit_2.test.C.mean_imp.n_NA",
                                     "fit_2.test.C.mean_imp")

##Performance statistics using pt 

###Bootstrap performance

fit_2.boot.pt_stats.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                     ncol = 16))
colnames(fit_2.boot.pt_stats.mean_imp) <- c("fit_2.boot.id_rep",
                                            "fit_2.boot.pt",
                                            "fit_2.boot.net_benefit.mean_imp.n_NA",
                                            "fit_2.boot.net_benefit.mean_imp",
                                            "fit_2.boot.net_benefit.all.mean_imp.n_NA",
                                            "fit_2.boot.net_benefit.all.mean_imp",
                                            "fit_2.boot.acc.mean_imp.n_NA",
                                            "fit_2.boot.acc.mean_imp",
                                            "fit_2.boot.tpr.mean_imp.n_NA",
                                            "fit_2.boot.tpr.mean_imp",
                                            "fit_2.boot.tnr.mean_imp.n_NA",
                                            "fit_2.boot.tnr.mean_imp",
                                            "fit_2.boot.ppv.mean_imp.n_NA",
                                            "fit_2.boot.ppv.mean_imp",
                                            "fit_2.boot.npv.mean_imp.n_NA",
                                            "fit_2.boot.npv.mean_imp")

###Test performance

fit_2.test.pt_stats.mean_imp <- as.data.frame(matrix(nrow = 0,
                                                     ncol = 16))
colnames(fit_2.test.pt_stats.mean_imp) <- c("fit_2.test.id_rep",
                                            "fit_2.test.pt",
                                            "fit_2.test.net_benefit.mean_imp.n_NA",
                                            "fit_2.test.net_benefit.mean_imp",
                                            "fit_2.test.net_benefit.all.mean_imp.n_NA",
                                            "fit_2.test.net_benefit.all.mean_imp",
                                            "fit_2.test.acc.mean_imp.n_NA",
                                            "fit_2.test.acc.mean_imp",
                                            "fit_2.test.tpr.mean_imp.n_NA",
                                            "fit_2.test.tpr.mean_imp",
                                            "fit_2.test.tnr.mean_imp.n_NA",
                                            "fit_2.test.tnr.mean_imp",
                                            "fit_2.test.ppv.mean_imp.n_NA",
                                            "fit_2.test.ppv.mean_imp",
                                            "fit_2.test.npv.mean_imp.n_NA",
                                            "fit_2.test.npv.mean_imp")

#Loop

for(r in 1:n_rep) { 
  
  #Input data
  
  ##Bootstrap performance
  
  ###Bootstrap sample 
  
  fit_2.boot.index <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
  fit_2.boot.data <- data[fit_2.boot.index, ]
  
  ###Imputation 
  
  fit_2.boot.imp <- aregImpute(~ y +
                                 age.std +
                                 sex.male_1 +
                                 n_prescribed_medicines.std +
                                 high_risk_medicines.yes_1 +
                                 eGFR.below_60_1 +
                                 out_specialty_current_MUMC.group_A_1 +
                                 out_specialty_current_MUMC.group_B_1 +
                                 n_specialty_MUMC.std +
                                 health_literacy_composite.insufficient_1,
                               x = TRUE,
                               data = fit_2.boot.data,
                               n.impute = n_imp,
                               nk = 3,
                               pr = FALSE)
  
  fit_2.boot.fit_2.stacked_imp <- list()
  for (i in 1:n_imp) {
    fit_2.boot.fit_2.stacked_imp[[i]] <- impute.transcan(fit_2.boot.imp, 
                                                         imputation = i, 
                                                         data = fit_2.boot.data, 
                                                         list.out = TRUE, 
                                                         pr = FALSE)
  }
  
  ##Test performance
  
  fit_2.test.fit_2.stacked_imp <- fit_2.stacked_imp
  
  #Input model
  
  ##Bootstrap performance
  
  fit_2.boot.fit_2.stacked_imp.fit_2 <- list()
  for(i in 1:n_imp) {
    fit_2.boot.fit_2.stacked_imp.fit_2[[i]] <- lrm(y ~ 
                                                     age.std +
                                                     n_prescribed_medicines.std +
                                                     high_risk_medicines.yes_1 +
                                                     health_literacy_composite.insufficient_1,
                                                   data = as.data.frame(fit_2.boot.fit_2.stacked_imp[[i]]))
  }
  
  ##Test performance
  
  fit_2.test.fit_2.stacked_imp.fit_2 <- fit_2.boot.fit_2.stacked_imp.fit_2
  
  #Predicted probabilities 
  
  ##Bootstrap performance
  
  for (i in 1:n_imp) {
    fit_2.boot.fit_2.stacked_imp[[i]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]] <- as.vector(predict(fit_2.boot.fit_2.stacked_imp.fit_2[[i]], fit_2.boot.fit_2.stacked_imp[[i]], type = "fitted.ind"))
  }
  
  ##Test performance
  
  for (i in 1:n_imp) {
    fit_2.test.fit_2.stacked_imp[[i]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]] <- as.vector(predict(fit_2.test.fit_2.stacked_imp.fit_2[[i]], fit_2.test.fit_2.stacked_imp[[i]], type = "fitted.ind"))
  }
  
  #Performance statistics
  
  ##Rsq Nagelkerke
  
  ###Bootstrap performance
  
  fit_2.boot.RsqN.mean_imp[r, "fit_2.boot.id_rep"] <- r
  
  fit_2.boot.id_imp.fit_2.boot.RsqN <- as.data.frame(matrix(nrow = 0,
                                                            ncol = 7))
  colnames(fit_2.boot.id_imp.fit_2.boot.RsqN) <- c("fit_2.boot.id_imp",
                                                   "fit_2.boot.deviance.null",
                                                   "fit_2.boot.deviance.fit_2.boot.fit_2.stacked_imp.fit_2",
                                                   "fit_2.boot.n",
                                                   "fit_2.boot.Ey",
                                                   "fit_2.boot.RsqCS",
                                                   "fit_2.boot.RsqN")
  for (i in 1:n_imp){
    fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.id_imp"] <- i
    
    fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.deviance.null"] <- - 2 * sum(log(mean(fit_2.boot.fit_2.stacked_imp[[i]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]]) * fit_2.boot.fit_2.stacked_imp[[i]][["y"]] + (1 - mean(fit_2.boot.fit_2.stacked_imp[[i]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]])) * (1 - fit_2.boot.fit_2.stacked_imp[[i]][["y"]])))
    
    fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.deviance.fit_2.boot.fit_2.stacked_imp.fit_2"] <- - 2 * sum(log(fit_2.boot.fit_2.stacked_imp[[i]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]] * fit_2.boot.fit_2.stacked_imp[[i]][["y"]] + (1 - fit_2.boot.fit_2.stacked_imp[[i]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]]) * (1 - fit_2.boot.fit_2.stacked_imp[[i]][["y"]])))
    
    fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.n"] <- nrow(as.data.frame(fit_2.boot.fit_2.stacked_imp[[i]]))
    
    fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.Ey"] <- sum(fit_2.boot.fit_2.stacked_imp[[i]][["y"]]) / nrow(as.data.frame(fit_2.boot.fit_2.stacked_imp[[i]]))
    
    fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.RsqCS"] <- 1 - exp((fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.deviance.fit_2.boot.fit_2.stacked_imp.fit_2"] - fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.deviance.null"]) / 
                                                                          nrow(as.data.frame(fit_2.boot.fit_2.stacked_imp[[i]])))
    
    fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.RsqN"] <- fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.RsqCS"] / 
                                                               (1 - ((fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.Ey"] ** fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.Ey"]) * 
                                                                     ((1 - fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.Ey"]) ** (1 - fit_2.boot.id_imp.fit_2.boot.RsqN[i, "fit_2.boot.Ey"])) 
                                                                    ) ** 2)
  }
  
  fit_2.boot.RsqN.mean_imp[r, "fit_2.boot.RsqN.mean_imp.n_NA"] <- sum(is.na(fit_2.boot.id_imp.fit_2.boot.RsqN$fit_2.boot.RsqN))
  
  fit_2.boot.RsqN.mean_imp[r, "fit_2.boot.RsqN.mean_imp"] <- mean(fit_2.boot.id_imp.fit_2.boot.RsqN$fit_2.boot.RsqN, na.rm = TRUE)
  
  ###Test performance
  
  fit_2.test.RsqN.mean_imp[r, "fit_2.test.id_rep"] <- r
  
  fit_2.test.id_imp.fit_2.test.RsqN <- as.data.frame(matrix(nrow = 0,
                                                            ncol = 7))
  colnames(fit_2.test.id_imp.fit_2.test.RsqN) <- c("fit_2.test.id_imp",
                                                   "fit_2.test.deviance.null",
                                                   "fit_2.test.deviance.fit_2.test.fit_2.stacked_imp.fit_2",
                                                   "fit_2.test.n",
                                                   "fit_2.test.Ey",
                                                   "fit_2.test.RsqCS",
                                                   "fit_2.test.RsqN")
  for (i in 1:n_imp){
    fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.id_imp"] <- i
    
    fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.deviance.null"] <- - 2 * sum(log(mean(fit_2.test.fit_2.stacked_imp[[i]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]]) * fit_2.test.fit_2.stacked_imp[[i]][["y"]] + (1 - mean(fit_2.test.fit_2.stacked_imp[[i]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]])) * (1 - fit_2.test.fit_2.stacked_imp[[i]][["y"]])))
    
    fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.deviance.fit_2.test.fit_2.stacked_imp.fit_2"] <- - 2 * sum(log(fit_2.test.fit_2.stacked_imp[[i]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]] * fit_2.test.fit_2.stacked_imp[[i]][["y"]] + (1 - fit_2.test.fit_2.stacked_imp[[i]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]]) * (1 - fit_2.test.fit_2.stacked_imp[[i]][["y"]])))
    
    fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.n"] <- nrow(as.data.frame(fit_2.test.fit_2.stacked_imp[[i]]))
    
    fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.Ey"] <- sum(fit_2.test.fit_2.stacked_imp[[i]][["y"]]) / nrow(as.data.frame(fit_2.test.fit_2.stacked_imp[[i]]))
    
    fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.RsqCS"] <- 1 - exp((fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.deviance.fit_2.test.fit_2.stacked_imp.fit_2"] - fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.deviance.null"]) / 
                                                                         nrow(as.data.frame(fit_2.test.fit_2.stacked_imp[[i]])))
    
    fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.RsqN"] <- fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.RsqCS"] / 
                                                               (1 - ((fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.Ey"] ** fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.Ey"]) * 
                                                                     ((1 - fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.Ey"]) ** (1 - fit_2.test.id_imp.fit_2.test.RsqN[i, "fit_2.test.Ey"])) 
                                                                    ) ** 2)
  }
  
  fit_2.test.RsqN.mean_imp[r, "fit_2.test.RsqN.mean_imp.n_NA"] <- sum(is.na(fit_2.test.id_imp.fit_2.test.RsqN$fit_2.test.RsqN))
  
  fit_2.test.RsqN.mean_imp[r, "fit_2.test.RsqN.mean_imp"] <- mean(fit_2.test.id_imp.fit_2.test.RsqN$fit_2.test.RsqN, na.rm = TRUE)
  
  ##C-index
  
  ###Bootstrap performance
  
  fit_2.boot.C.mean_imp[r, "fit_2.boot.id_rep"] <- r
  
  fit_2.boot.id_imp.fit_2.boot.C <- as.data.frame(matrix(nrow = n_imp,
                                                         ncol = 2))
  colnames(fit_2.boot.id_imp.fit_2.boot.C) <- c("fit_2.boot.id_imp",
                                                "fit_2.boot.C")
  for (i in 1:n_imp) {
    fit_2.boot.id_imp.fit_2.boot.C[i, "fit_2.boot.id_imp"] <- i
    
    fit_2.boot.id_imp.fit_2.boot.C[i, "fit_2.boot.C"] <- val.prob(fit_2.boot.fit_2.stacked_imp[[i]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]], fit_2.boot.fit_2.stacked_imp[[i]][["y"]], pl = FALSE)[["C (ROC)"]]
  }
  
  fit_2.boot.C.mean_imp[r, "fit_2.boot.C.mean_imp.n_NA"] <- sum(is.na(fit_2.boot.id_imp.fit_2.boot.C$fit_2.boot.C))
  
  fit_2.boot.C.mean_imp[r, "fit_2.boot.C.mean_imp"] <- mean(fit_2.boot.id_imp.fit_2.boot.C$fit_2.boot.C, na.rm = TRUE)
  
  ###Test performance
  
  fit_2.test.C.mean_imp[r, "fit_2.test.id_rep"] <- r
  
  fit_2.test.id_imp.fit_2.test.C <- as.data.frame(matrix(nrow = n_imp,
                                                         ncol = 2))
  colnames(fit_2.test.id_imp.fit_2.test.C) <- c("fit_2.test.id_imp",
                                                "fit_2.test.C")
  for (i in 1:n_imp) {
    fit_2.test.id_imp.fit_2.test.C[i, "fit_2.test.id_imp"] <- i
    
    fit_2.test.id_imp.fit_2.test.C[i, "fit_2.test.C"] <- val.prob(fit_2.test.fit_2.stacked_imp[[i]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]], fit_2.test.fit_2.stacked_imp[[i]][["y"]], pl = FALSE)[["C (ROC)"]]
  }

  fit_2.test.C.mean_imp[r, "fit_2.test.C.mean_imp.n_NA"] <-  sum(is.na(fit_2.test.id_imp.fit_2.test.C$fit_2.test.C))
    
  fit_2.test.C.mean_imp[r, "fit_2.test.C.mean_imp"] <-  mean(fit_2.test.id_imp.fit_2.test.C$fit_2.test.C, na.rm = TRUE)
  
  ##Performance statistics using pt
  
  ###Bootstrap performance
  
  fit_2.boot.pt_stats <- as.data.frame(matrix(nrow = 0,
                                              ncol = 8))
  colnames(fit_2.boot.pt_stats) <- c("fit_2.boot.id_imp",
                                     "fit_2.boot.pt",
                                     "fit_2.boot.Ey",
                                     "fit_2.boot.tp",
                                     "fit_2.boot.tn",
                                     "fit_2.boot.fp",
                                     "fit_2.boot.fn",
                                     "fit_2.boot.n")
  for (i in 1:n_imp){
    for (p in pt) {
      fit_2.boot.id_imp.temp <- i
      
      fit_2.boot.pt.temp <- p
      
      fit_2.boot.Ey.temp <- sum(fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["y"]]) / nrow(as.data.frame(fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]]))
      
      fit_2.boot.tp.temp <- ifelse((fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]] >= fit_2.boot.pt.temp)
                                   &
                                   (fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["y"]] == 1),
                                   1,
                                   0)
      fit_2.boot.tp.temp <- sum(fit_2.boot.tp.temp)
      
      fit_2.boot.tn.temp <- ifelse((fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]] < fit_2.boot.pt.temp)
                                   &
                                   (fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["y"]] == 0),
                                   1,
                                   0)
      fit_2.boot.tn.temp <- sum(fit_2.boot.tn.temp)
      
      fit_2.boot.fp.temp <- ifelse((fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]] >= fit_2.boot.pt.temp)
                                   &
                                   (fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["y"]] == 0),
                                   1,
                                   0)
      fit_2.boot.fp.temp <- sum(fit_2.boot.fp.temp)
      
      fit_2.boot.fn.temp <- ifelse((fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["fit_2.boot.fit_2.stacked_imp.fit_2.pred_prob"]] < fit_2.boot.pt.temp)
                                   &
                                   (fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]][["y"]] == 1),
                                   1,
                                   0)
      fit_2.boot.fn.temp <- sum(fit_2.boot.fn.temp)
      
      fit_2.boot.n.temp <- nrow(as.data.frame(fit_2.boot.fit_2.stacked_imp[[fit_2.boot.id_imp.temp]]))
      
      fit_2.boot.pt_stats.temp <- data.frame(fit_2.boot.id_imp = fit_2.boot.id_imp.temp,
                                             fit_2.boot.pt = fit_2.boot.pt.temp,
                                             fit_2.boot.Ey = fit_2.boot.Ey.temp,
                                             fit_2.boot.tp = fit_2.boot.tp.temp,
                                             fit_2.boot.tn = fit_2.boot.tn.temp,
                                             fit_2.boot.fp = fit_2.boot.fp.temp,
                                             fit_2.boot.fn = fit_2.boot.fn.temp,
                                             fit_2.boot.n = fit_2.boot.n.temp)
      
      fit_2.boot.pt_stats <- rbind(fit_2.boot.pt_stats, fit_2.boot.pt_stats.temp)
    }
  }
  fit_2.boot.pt_stats$fit_2.boot.net_benefit <- NA
  fit_2.boot.pt_stats$fit_2.boot.net_benefit.all <- NA
  fit_2.boot.pt_stats$fit_2.boot.acc <- NA
  fit_2.boot.pt_stats$fit_2.boot.tpr <- NA
  fit_2.boot.pt_stats$fit_2.boot.tnr <- NA
  fit_2.boot.pt_stats$fit_2.boot.ppv <- NA
  fit_2.boot.pt_stats$fit_2.boot.npv <- NA
  for(k in 1:nrow(fit_2.boot.pt_stats)) {
    fit_2.boot.pt_stats[k, "fit_2.boot.net_benefit"] <- (fit_2.boot.pt_stats[k, "fit_2.boot.tp"] / fit_2.boot.pt_stats[k, "fit_2.boot.n"]) - 
                                                        (fit_2.boot.pt_stats[k, "fit_2.boot.fp"] / fit_2.boot.pt_stats[k, "fit_2.boot.n"]) * 
                                                        (fit_2.boot.pt_stats[k, "fit_2.boot.pt"] / (1 - fit_2.boot.pt_stats[k, "fit_2.boot.pt"]))
    
    fit_2.boot.pt_stats[k, "fit_2.boot.net_benefit.all"] <- fit_2.boot.pt_stats[k, "fit_2.boot.Ey"] - 
                                                            ((fit_2.boot.pt_stats[k, "fit_2.boot.pt"] / (1 - fit_2.boot.pt_stats[k, "fit_2.boot.pt"])) * (1 - fit_2.boot.pt_stats[k, "fit_2.boot.Ey"]))
    
    fit_2.boot.pt_stats[k, "fit_2.boot.acc"] <- (fit_2.boot.pt_stats[k, "fit_2.boot.tp"] + fit_2.boot.pt_stats[k, "fit_2.boot.tn"] ) /
                                                (fit_2.boot.pt_stats[k, "fit_2.boot.tp"] + fit_2.boot.pt_stats[k, "fit_2.boot.tn"] + fit_2.boot.pt_stats[k, "fit_2.boot.fp"] + fit_2.boot.pt_stats[k, "fit_2.boot.fn"])
    
    fit_2.boot.pt_stats[k, "fit_2.boot.tpr"] <- fit_2.boot.pt_stats[k, "fit_2.boot.tp"] /
                                                (fit_2.boot.pt_stats[k, "fit_2.boot.tp"] + fit_2.boot.pt_stats[k, "fit_2.boot.fn"])
    
    fit_2.boot.pt_stats[k, "fit_2.boot.tnr"] <- fit_2.boot.pt_stats[k, "fit_2.boot.tn"] /
                                                (fit_2.boot.pt_stats[k, "fit_2.boot.tn"] + fit_2.boot.pt_stats[k, "fit_2.boot.fp"])
    
    fit_2.boot.pt_stats[k, "fit_2.boot.ppv"] <- fit_2.boot.pt_stats[k, "fit_2.boot.tp"] /
                                                (fit_2.boot.pt_stats[k, "fit_2.boot.tp"] + fit_2.boot.pt_stats[k, "fit_2.boot.fp"])
    
    fit_2.boot.pt_stats[k, "fit_2.boot.npv"] <- fit_2.boot.pt_stats[k, "fit_2.boot.tn"] /
                                                (fit_2.boot.pt_stats[k, "fit_2.boot.tn"] + fit_2.boot.pt_stats[k, "fit_2.boot.fn"])
  } 
  for(p in pt) {
    fit_2.boot.id_rep.temp <- r
    
    fit_2.boot.pt.temp <- p
    
    fit_2.boot.net_benefit.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.net_benefit))
    
    fit_2.boot.net_benefit.mean_imp.temp <- mean(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.net_benefit, na.rm = TRUE)
    
    fit_2.boot.net_benefit.all.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.net_benefit.all))
    
    fit_2.boot.net_benefit.all.mean_imp.temp <- mean(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.net_benefit.all, na.rm = TRUE)
    
    fit_2.boot.acc.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.acc))
    
    fit_2.boot.acc.mean_imp.temp <- mean(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.acc, na.rm = TRUE)
    
    fit_2.boot.tpr.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.tpr))
    
    fit_2.boot.tpr.mean_imp.temp <- mean(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.tpr, na.rm = TRUE)
    
    fit_2.boot.tnr.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.tnr))    
    
    fit_2.boot.tnr.mean_imp.temp <- mean(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.tnr, na.rm = TRUE)    
    
    fit_2.boot.ppv.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.ppv))  
    
    fit_2.boot.ppv.mean_imp.temp <- mean(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.ppv, na.rm = TRUE)  
    
    fit_2.boot.npv.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.npv))  
    
    fit_2.boot.npv.mean_imp.temp <- mean(subset(fit_2.boot.pt_stats, pt == fit_2.boot.pt.temp)$fit_2.boot.npv, na.rm = TRUE)  
    
    fit_2.boot.pt_stats.mean_imp.temp <- data.frame(fit_2.boot.id_rep = fit_2.boot.id_rep.temp,
                                                    fit_2.boot.pt = fit_2.boot.pt.temp,
                                                    fit_2.boot.net_benefit.mean_imp.n_NA = fit_2.boot.net_benefit.mean_imp.n_NA.temp,
                                                    fit_2.boot.net_benefit.mean_imp = fit_2.boot.net_benefit.mean_imp.temp,
                                                    fit_2.boot.net_benefit.all.mean_imp.n_NA = fit_2.boot.net_benefit.all.mean_imp.n_NA.temp,
                                                    fit_2.boot.net_benefit.all.mean_imp = fit_2.boot.net_benefit.all.mean_imp.temp,
                                                    fit_2.boot.acc.mean_imp.n_NA = fit_2.boot.acc.mean_imp.n_NA.temp,
                                                    fit_2.boot.acc.mean_imp = fit_2.boot.acc.mean_imp.temp,
                                                    fit_2.boot.tpr.mean_imp.n_NA = fit_2.boot.tpr.mean_imp.n_NA.temp,
                                                    fit_2.boot.tpr.mean_imp = fit_2.boot.tpr.mean_imp.temp,
                                                    fit_2.boot.tnr.mean_imp.n_NA = fit_2.boot.tnr.mean_imp.n_NA.temp,
                                                    fit_2.boot.tnr.mean_imp = fit_2.boot.tnr.mean_imp.temp,
                                                    fit_2.boot.ppv.mean_imp.n_NA = fit_2.boot.ppv.mean_imp.n_NA.temp,
                                                    fit_2.boot.ppv.mean_imp = fit_2.boot.ppv.mean_imp.temp,
                                                    fit_2.boot.npv.mean_imp.n_NA = fit_2.boot.npv.mean_imp.n_NA.temp,
                                                    fit_2.boot.npv.mean_imp = fit_2.boot.npv.mean_imp.temp)
    
    fit_2.boot.pt_stats.mean_imp <- rbind(fit_2.boot.pt_stats.mean_imp, fit_2.boot.pt_stats.mean_imp.temp)
  }
  
  ###Test performance 
  
  fit_2.test.pt_stats <- as.data.frame(matrix(nrow = 0,
                                              ncol = 8))
  colnames(fit_2.test.pt_stats) <- c("fit_2.test.id_imp",
                                     "fit_2.test.pt",
                                     "fit_2.test.Ey",
                                     "fit_2.test.tp",
                                     "fit_2.test.tn",
                                     "fit_2.test.fp",
                                     "fit_2.test.fn",
                                     "fit_2.test.n")
  for (i in 1:n_imp){
    for (p in pt) {
      fit_2.test.id_imp.temp <- i
      
      fit_2.test.pt.temp <- p
      
      fit_2.test.Ey.temp <- sum(fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["y"]]) / nrow(as.data.frame(fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]]))
      
      fit_2.test.tp.temp <- ifelse((fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]] >= fit_2.test.pt.temp)
                                   &
                                   (fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["y"]] == 1),
                                   1,
                                   0)
      fit_2.test.tp.temp <- sum(fit_2.test.tp.temp)
      
      fit_2.test.tn.temp <- ifelse((fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]] < fit_2.test.pt.temp)
                                   &
                                   (fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["y"]] == 0),
                                   1,
                                   0)
      fit_2.test.tn.temp <- sum(fit_2.test.tn.temp)
      
      fit_2.test.fp.temp <- ifelse((fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]] >= fit_2.test.pt.temp)
                                   &
                                   (fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["y"]] == 0),
                                   1,
                                   0)
      fit_2.test.fp.temp <- sum(fit_2.test.fp.temp)
      
      fit_2.test.fn.temp <- ifelse((fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["fit_2.test.fit_2.stacked_imp.fit_2.pred_prob"]] < fit_2.test.pt.temp)
                                   &
                                   (fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]][["y"]] == 1),
                                   1,
                                   0)
      fit_2.test.fn.temp <- sum(fit_2.test.fn.temp)
      
      fit_2.test.n.temp <- nrow(as.data.frame(fit_2.test.fit_2.stacked_imp[[fit_2.test.id_imp.temp]]))
      
      fit_2.test.pt_stats.temp <- data.frame(fit_2.test.id_imp = fit_2.test.id_imp.temp,
                                             fit_2.test.pt = fit_2.test.pt.temp,
                                             fit_2.test.Ey = fit_2.test.Ey.temp,
                                             fit_2.test.tp = fit_2.test.tp.temp,
                                             fit_2.test.tn = fit_2.test.tn.temp,
                                             fit_2.test.fp = fit_2.test.fp.temp,
                                             fit_2.test.fn = fit_2.test.fn.temp,
                                             fit_2.test.n = fit_2.test.n.temp)
      
      fit_2.test.pt_stats <- rbind(fit_2.test.pt_stats, fit_2.test.pt_stats.temp)
    }
  }
  fit_2.test.pt_stats$fit_2.test.net_benefit <- NA
  fit_2.test.pt_stats$fit_2.test.net_benefit.all <- NA
  fit_2.test.pt_stats$fit_2.test.acc <- NA
  fit_2.test.pt_stats$fit_2.test.tpr <- NA
  fit_2.test.pt_stats$fit_2.test.tnr <- NA
  fit_2.test.pt_stats$fit_2.test.ppv <- NA
  fit_2.test.pt_stats$fit_2.test.npv <- NA
  for(k in 1:nrow(fit_2.test.pt_stats)) {
    fit_2.test.pt_stats[k, "fit_2.test.net_benefit"] <- (fit_2.test.pt_stats[k, "fit_2.test.tp"] / fit_2.test.pt_stats[k, "fit_2.test.n"]) - 
                                                        (fit_2.test.pt_stats[k, "fit_2.test.fp"] / fit_2.test.pt_stats[k, "fit_2.test.n"]) * 
                                                        (fit_2.test.pt_stats[k, "fit_2.test.pt"] / (1 - fit_2.test.pt_stats[k, "fit_2.test.pt"]))
    
    fit_2.test.pt_stats[k, "fit_2.test.net_benefit.all"] <- fit_2.test.pt_stats[k, "fit_2.test.Ey"] - 
                                                            ((fit_2.test.pt_stats[k, "fit_2.test.pt"] / (1 - fit_2.test.pt_stats[k, "fit_2.test.pt"])) * (1 - fit_2.test.pt_stats[k, "fit_2.test.Ey"]))
    
    fit_2.test.pt_stats[k, "fit_2.test.acc"] <- (fit_2.test.pt_stats[k, "fit_2.test.tp"] + fit_2.test.pt_stats[k, "fit_2.test.tn"]) /
                                                (fit_2.test.pt_stats[k, "fit_2.test.tp"] + fit_2.test.pt_stats[k, "fit_2.test.tn"] + fit_2.test.pt_stats[k, "fit_2.test.fp"] + fit_2.test.pt_stats[k, "fit_2.test.fn"])
    
    fit_2.test.pt_stats[k, "fit_2.test.tpr"] <- fit_2.test.pt_stats[k, "fit_2.test.tp"] /
                                                (fit_2.test.pt_stats[k, "fit_2.test.tp"] + fit_2.test.pt_stats[k, "fit_2.test.fn"])
    
    fit_2.test.pt_stats[k, "fit_2.test.tnr"] <- fit_2.test.pt_stats[k, "fit_2.test.tn"] /
                                                (fit_2.test.pt_stats[k, "fit_2.test.tn"] + fit_2.test.pt_stats[k, "fit_2.test.fp"])
    
    fit_2.test.pt_stats[k, "fit_2.test.ppv"] <- fit_2.test.pt_stats[k, "fit_2.test.tp"] /
                                                (fit_2.test.pt_stats[k, "fit_2.test.tp"] + fit_2.test.pt_stats[k, "fit_2.test.fp"])
    
    fit_2.test.pt_stats[k, "fit_2.test.npv"] <- fit_2.test.pt_stats[k, "fit_2.test.tn"] /
                                                (fit_2.test.pt_stats[k, "fit_2.test.tn"] + fit_2.test.pt_stats[k, "fit_2.test.fn"])
  } 
  for(p in pt) {
    fit_2.test.id_rep.temp <- r
    
    fit_2.test.pt.temp <- p
    
    fit_2.test.net_benefit.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.net_benefit))
    
    fit_2.test.net_benefit.mean_imp.temp <- mean(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.net_benefit, na.rm = TRUE)
    
    fit_2.test.net_benefit.all.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.net_benefit.all))
    
    fit_2.test.net_benefit.all.mean_imp.temp <- mean(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.net_benefit.all, na.rm = TRUE)
    
    fit_2.test.acc.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.acc))
    
    fit_2.test.acc.mean_imp.temp <- mean(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.acc, na.rm = TRUE)
    
    fit_2.test.tpr.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.tpr))
    
    fit_2.test.tpr.mean_imp.temp <- mean(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.tpr, na.rm = TRUE)
    
    fit_2.test.tnr.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.tnr))    
    
    fit_2.test.tnr.mean_imp.temp <- mean(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.tnr, na.rm = TRUE)    
    
    fit_2.test.ppv.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.ppv))  
    
    fit_2.test.ppv.mean_imp.temp <- mean(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.ppv, na.rm = TRUE)  
    
    fit_2.test.npv.mean_imp.n_NA.temp <- sum(is.na(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.npv))  
    
    fit_2.test.npv.mean_imp.temp <- mean(subset(fit_2.test.pt_stats, pt == fit_2.test.pt.temp)$fit_2.test.npv, na.rm = TRUE)  
    
    fit_2.test.pt_stats.mean_imp.temp <- data.frame(fit_2.test.id_rep = fit_2.test.id_rep.temp,
                                                    fit_2.test.pt = fit_2.test.pt.temp,
                                                    fit_2.test.net_benefit.mean_imp.n_NA = fit_2.test.net_benefit.mean_imp.n_NA.temp,
                                                    fit_2.test.net_benefit.mean_imp = fit_2.test.net_benefit.mean_imp.temp,
                                                    fit_2.test.net_benefit.all.mean_imp.n_NA = fit_2.test.net_benefit.all.mean_imp.n_NA.temp,
                                                    fit_2.test.net_benefit.all.mean_imp = fit_2.test.net_benefit.all.mean_imp.temp,
                                                    fit_2.test.acc.mean_imp.n_NA = fit_2.test.acc.mean_imp.n_NA.temp,
                                                    fit_2.test.acc.mean_imp = fit_2.test.acc.mean_imp.temp,
                                                    fit_2.test.tpr.mean_imp.n_NA = fit_2.test.tpr.mean_imp.n_NA.temp,
                                                    fit_2.test.tpr.mean_imp = fit_2.test.tpr.mean_imp.temp,
                                                    fit_2.test.tnr.mean_imp.n_NA = fit_2.test.tnr.mean_imp.n_NA.temp,
                                                    fit_2.test.tnr.mean_imp = fit_2.test.tnr.mean_imp.temp,
                                                    fit_2.test.ppv.mean_imp.n_NA = fit_2.test.ppv.mean_imp.n_NA.temp,
                                                    fit_2.test.ppv.mean_imp = fit_2.test.ppv.mean_imp.temp,
                                                    fit_2.test.npv.mean_imp.n_NA = fit_2.test.npv.mean_imp.n_NA.temp,
                                                    fit_2.test.npv.mean_imp = fit_2.test.npv.mean_imp.temp)
    
    fit_2.test.pt_stats.mean_imp <- rbind(fit_2.test.pt_stats.mean_imp, fit_2.test.pt_stats.mean_imp.temp)
  }
}

#Calculating optimism

##Rsq Nagelkerke

sum(fit_2.boot.RsqN.mean_imp$fit_2.boot.RsqN.mean_imp.n_NA)
sum(fit_2.boot.RsqN.mean_imp$fit_2.test.RsqN.mean_imp.n_NA)
fit_2.o.RsqN.mean_imp <- fit_2.boot.RsqN.mean_imp$fit_2.boot.RsqN.mean_imp - fit_2.test.RsqN.mean_imp$fit_2.test.RsqN.mean_imp
fit_2.o.RsqN.mean_imp <- as.data.frame(cbind(fit_2.boot.RsqN.mean_imp$fit_2.boot.id_rep, 
                                             fit_2.o.RsqN.mean_imp))
colnames(fit_2.o.RsqN.mean_imp) <- c("o.fit_2.id_rep",
                                     "o.fit_2.RsqN.mean_imp")

##C-index

sum(fit_2.boot.C.mean_imp$fit_2.boot.C.mean_imp.n_NA)
sum(fit_2.boot.C.mean_imp$fit_2.test.C.mean_imp.n_NA)
fit_2.o.C.mean_imp <- fit_2.boot.C.mean_imp$fit_2.boot.C.mean_imp - fit_2.test.C.mean_imp$fit_2.test.C.mean_imp
fit_2.o.C.mean_imp <- as.data.frame(cbind(fit_2.boot.C.mean_imp$fit_2.boot.id_rep, 
                                          fit_2.o.C.mean_imp))
colnames(fit_2.o.C.mean_imp) <- c("fit_2.o.id_rep",
                                  "fit_2.o.C.mean_imp")

##Performance statistics using pt

fit_2.boot.test.pt_stats.mean_imp <- merge(fit_2.boot.pt_stats.mean_imp,
                                           fit_2.test.pt_stats.mean_imp,
                                           by.x = c("fit_2.boot.id_rep",
                                                    "fit_2.boot.pt"),
                                           by.y = c("fit_2.test.id_rep",
                                                    "fit_2.test.pt"))
colnames(fit_2.boot.test.pt_stats.mean_imp)[1] <- "fit_2.boot.test.id_rep"
colnames(fit_2.boot.test.pt_stats.mean_imp)[2] <- "fit_2.boot.test.pt"

fit_2.boot.test.pt_stats.mean_imp.0_NA <- subset(fit_2.boot.test.pt_stats.mean_imp, (fit_2.boot.net_benefit.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.test.net_benefit.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.boot.net_benefit.all.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.test.net_benefit.all.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.boot.acc.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.test.acc.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.boot.tpr.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.test.tpr.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.boot.tnr.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.test.tnr.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.boot.ppv.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.test.ppv.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.boot.npv.mean_imp.n_NA == 0)
                                                                                    &
                                                                                    (fit_2.test.npv.mean_imp.n_NA == 0))

for (p in pt){
  print(p)
  print(nrow(subset(fit_2.boot.test.pt_stats.mean_imp, fit_2.boot.test.pt == p)))
  print(nrow(subset(fit_2.boot.test.pt_stats.mean_imp.0_NA, fit_2.boot.test.pt == p)))
}
                                        
fit_2.o.pt_stats.mean_imp <- as.data.frame(matrix(nrow = nrow(fit_2.boot.test.pt_stats.mean_imp.0_NA),
                                                  ncol = 9))
colnames(fit_2.o.pt_stats.mean_imp) <- c("fit_2.o.id_rep",
                                         "fit_2.o.pt", 
                                         "fit_2.o.net_benefit.mean_imp",
                                         "fit_2.o.net_benefit.all.mean_imp",
                                         "fit_2.o.acc.mean_imp",
                                         "fit_2.o.tpr.mean_imp",
                                         "fit_2.o.tnr.mean_imp",
                                         "fit_2.o.ppv.mean_imp",
                                         "fit_2.o.npv.mean_imp")
fit_2.o.pt_stats.mean_imp$fit_2.o.id_rep <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.test.id_rep
fit_2.o.pt_stats.mean_imp$fit_2.o.pt <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.test.pt
fit_2.o.pt_stats.mean_imp$fit_2.o.net_benefit.mean_imp <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.net_benefit.mean_imp - fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.test.net_benefit.mean_imp
fit_2.o.pt_stats.mean_imp$fit_2.o.net_benefit.all.mean_imp <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.net_benefit.all.mean_imp - fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.test.net_benefit.all.mean_imp
fit_2.o.pt_stats.mean_imp$fit_2.o.acc.mean_imp <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.acc.mean_imp - fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.test.acc.mean_imp
fit_2.o.pt_stats.mean_imp$fit_2.o.tpr.mean_imp <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.tpr.mean_imp - fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.test.tpr.mean_imp
fit_2.o.pt_stats.mean_imp$fit_2.o.tnr.mean_imp <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.tnr.mean_imp - fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.test.tnr.mean_imp
fit_2.o.pt_stats.mean_imp$fit_2.o.ppv.mean_imp <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.ppv.mean_imp - fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.test.ppv.mean_imp
fit_2.o.pt_stats.mean_imp$fit_2.o.npv.mean_imp <- fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.boot.npv.mean_imp - fit_2.boot.test.pt_stats.mean_imp.0_NA$fit_2.test.npv.mean_imp

#Correcting apparent performance statistics with average optimism

##Rsq Nagelkerke

fit_2.oc.RsqN.mean_imp <- fit_2.app.RsqN.mean_imp - mean(fit_2.o.RsqN.mean_imp$o.fit_2.RsqN.mean_imp)
fit_2.oc.RsqN.mean_imp

##C-index

fit_2.oc.C.mean_imp <- fit_2.app.C.mean_imp - mean(fit_2.o.C.mean_imp$fit_2.o.C.mean_imp)
fit_2.oc.C.mean_imp

##Performance statistics using pt

fit_2.o.mean.pt_stats.mean_imp <- as.data.frame(matrix(nrow = length(pt),
                                                       ncol = 8))
colnames(fit_2.o.mean.pt_stats.mean_imp) <- c("fit_2.o.mean.pt", 
                                              "fit_2.o.mean.net_benefit.mean_imp",
                                              "fit_2.o.mean.net_benefit.all.mean_imp",
                                              "fit_2.o.mean.acc.mean_imp",
                                              "fit_2.o.mean.tpr.mean_imp",
                                              "fit_2.o.mean.tnr.mean_imp",
                                              "fit_2.o.mean.ppv.mean_imp",
                                              "fit_2.o.mean.npv.mean_imp")
fit_2.o.mean.pt_stats.mean_imp$fit_2.o.mean.pt <- pt
for(k in 1:nrow(fit_2.o.mean.pt_stats.mean_imp)) {
  fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.net_benefit.mean_imp"] <- mean(subset(fit_2.o.pt_stats.mean_imp, fit_2.o.pt == fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.pt"])$fit_2.o.net_benefit.mean_imp)
  
  fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.net_benefit.all.mean_imp"] <- mean(subset(fit_2.o.pt_stats.mean_imp, fit_2.o.pt == fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.pt"])$fit_2.o.net_benefit.all.mean_imp)
  
  fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.acc.mean_imp"] <- mean(subset(fit_2.o.pt_stats.mean_imp, fit_2.o.pt == fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.pt"])$fit_2.o.acc.mean_imp)
  
  fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.tpr.mean_imp"] <- mean(subset(fit_2.o.pt_stats.mean_imp, fit_2.o.pt == fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.pt"])$fit_2.o.tpr.mean_imp)
  
  fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.tnr.mean_imp"] <- mean(subset(fit_2.o.pt_stats.mean_imp, fit_2.o.pt == fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.pt"])$fit_2.o.tnr.mean_imp)
  
  fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.ppv.mean_imp"] <- mean(subset(fit_2.o.pt_stats.mean_imp, fit_2.o.pt == fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.pt"])$fit_2.o.ppv.mean_imp)
  
  fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.npv.mean_imp"] <- mean(subset(fit_2.o.pt_stats.mean_imp, fit_2.o.pt == fit_2.o.mean.pt_stats.mean_imp[k, "fit_2.o.mean.pt"])$fit_2.o.npv.mean_imp)
}

fit_2.oc.pt_stats.mean_imp <- fit_2.app.pt_stats.mean_imp[, 2:8] - fit_2.o.mean.pt_stats.mean_imp[, 2:8]
fit_2.oc.pt_stats.mean_imp <- as.data.frame(cbind(fit_2.app.pt_stats.mean_imp$fit_2.app.pt,
                                                  fit_2.oc.pt_stats.mean_imp))
colnames(fit_2.oc.pt_stats.mean_imp) <- c("fit_2.oc.pt", 
                                          "fit_2.oc.net_benefit.mean_imp",
                                          "fit_2.oc.net_benefit.all.mean_imp",
                                          "fit_2.oc.acc.mean_imp",
                                          "fit_2.oc.tpr.mean_imp",
                                          "fit_2.oc.tnr.mean_imp",
                                          "fit_2.oc.ppv.mean_imp",
                                          "fit_2.oc.npv.mean_imp")
fit_2.oc.pt_stats.mean_imp

### NCC MERP index ----

summary(subset(data, NCC_MERP_index.E_or_above_1 == 0)$fit_2.pred_prob)
summary(subset(data, NCC_MERP_index.E_or_above_1 == 1)$fit_2.pred_prob)

sum(is.na(subset(data, NCC_MERP_index.E_or_above_1 == 0)$fit_2.pred_prob))
1 / 54
sum(is.na(subset(data, NCC_MERP_index.E_or_above_1 == 1)$fit_2.pred_prob))
1 / 26

nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_2.pred_prob < 0.0333)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_2.pred_prob < 0.0333))) / 53
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_2.pred_prob < 0.0333)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_2.pred_prob < 0.0333))) / 25

nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_2.pred_prob < 0.0500)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_2.pred_prob < 0.0500))) / 53
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_2.pred_prob < 0.0500)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_2.pred_prob < 0.0500))) / 25

nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_2.pred_prob < 0.1000)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 0) & (fit_2.pred_prob < 0.1000))) / 53
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_2.pred_prob < 0.1000)))
nrow(subset(data, (NCC_MERP_index.E_or_above_1 == 1) & (fit_2.pred_prob < 0.1000))) / 25

# Session info ----

sessionInfo()
