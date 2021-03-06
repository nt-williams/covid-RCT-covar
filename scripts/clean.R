# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(here[here], DT = data.table)

# survival ----------------------------------------------------------------

c19 <- readRDS(here("data", "private", "covid-raw.rds"))

DT$setDT(c19)

comorbid <- c("diabetes_mellitus", "hypertension", "copd", "ckd", 
              "esrd", "asthma", "interstitial_lung_disease", 
              "obstructive_sleep_apnea", "any_rheum_disease", 
              "any_pulmonary_disease", "hepatitis_or_hiv", 
              "renal_disease", "cva_stroke", "cirrhosis", 
              "cad_coronary_artery_disease", "any_active_cancer")

sympt <- grep("^symptoms", names(c19), value = TRUE)

c19 <- 
  c19[!is.na(bmi), 
      ][, days_outcome_cens := hours_outcome_cens %/% 24 + 1
        ][, (comorbid) := lapply(.SD, function(x) DT$fifelse(x == "Yes", 1, 0)), .SDcols = comorbid
          ][, num_comorbid := rowSums(.SD[, ..comorbid])
            ][, (sympt) := lapply(.SD, function(x) DT$fifelse(x == "Yes", 1, 0)), .SDcols = sympt
              ][, num_sympt := rowSums(.SD[, ..sympt])
                ][, .(days_outcome_cens, event_intubation_or_death, age_cdc_cats, sex, bmi, 
                      smoke_vape, supp_o2_within_3hrs_any, num_comorbid, num_sympt,
                      initial_chest_x_ray_findings_bilateral_infiltrates, 
                      symptoms_dyspnea, hypertension)]

DT$setnames(c19, names(c19), 
            c("days", "event", "age", "sex", "bmi", "smoke", 
              "o2", "num_comorbid", "num_symptoms", "bilat", "dyspnea", "hyper"))

c19 <- 
  c19[!(event == 0 & days < 15)
      ][event == 0, days := 15
        ][event == 1 & days > 15, `:=`(days = 15)]

saveRDS(c19, here("data", "private", "covid-survival.rds"))

# ordinal -----------------------------------------------------------------

c19 <- readRDS(here("data", "private", "intubation_0h.rds"))
ord <- readRDS(here("data", "private", "covid_14day_ordinal.rds"))

DT$setDT(c19)

comorbid <- c("diabetes_mellitus", "hypertension", "copd", "ckd", 
              "esrd", "asthma", "interstitial_lung_disease", 
              "obstructive_sleep_apnea", "any_rheum_disease", 
              "any_pulmonary_disease", "hepatitis_or_hiv", 
              "renal_disease", "cva_stroke", "cirrhosis", 
              "cad_coronary_artery_disease", "any_active_cancer")

sympt <- grep("^symptoms", names(c19), value = TRUE)

c19 <- 
  c19[!is.na(bmi), 
      ][, (comorbid) := lapply(.SD, function(x) DT$fifelse(x == "Yes", 1, 0)), .SDcols = comorbid
        ][, num_comorbid := rowSums(.SD[, ..comorbid])
          ][, (sympt) := lapply(.SD, function(x) DT$fifelse(x == "Yes", 1, 0)), .SDcols = sympt
            ][, num_sympt := rowSums(.SD[, ..sympt])
              ][, .(patient_empi, age, sex, bmi, 
                    smoke_vape, supp_o2_within_3hrs_any, num_comorbid, num_sympt,
                    initial_chest_x_ray_findings_bilateral_infiltrates, 
                    symptoms_dyspnea, hypertension)
                ][, patient_empi := as.character(patient_empi)]

DT$setnames(c19, names(c19), 
            c("empi", "age", "sex", "bmi", "smoke", "o2", "num_comorbid", 
              "num_symptoms", "bilat", "dyspnea", "hyper"))

DT$setkey(c19, "empi")
DT$setkey(ord, "empi")

c19 <- na.omit(c19[ord])

saveRDS(c19, here("data", "private", "covid-ordinal.rds"))

