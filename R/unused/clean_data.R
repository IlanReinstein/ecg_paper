library(tidyverse)

##### Import/Load Data #####
## ECG Data Set
ecg <- readRDS("data/raw/ecg1.rds") %>% ungroup() %>%
  mutate_at(vars(contains("Id")), as.character)

ecg$cardiologistDxLabel[str_detect(ecg$cardiologistDxLabel,"Bundle")] <- "Left Bundle Branch Block"
ecg$cardiologistDxLabel <- relevel(factor(ecg$cardiologistDxLabel), ref = "Normal ECG")
ecg <- within(ecg, caseType <- if_else(cardiologistDxLabel == "Normal ECG", "Normal", "Abnormal"))

ecg <- within(ecg, caseType2 <- if_else(cardiologistDxLabel == "Normal ECG", "Normal", if_else(str_detect(ecg$cardiologistDxLabel, "STEMI"), "STEMI", "Abnormal/Non-STEMI")))

ecg$caseType <- relevel(factor(ecg$caseType), ref = "Normal")
ecg$caseType2 <- relevel(factor(ecg$caseType2), ref = "Normal")
ecg <- ungroup(ecg)
write_csv(ecg, "data/interim/ecg_train.csv")

## Elbow Dataset

dfile <- 'elbows_cleaned_vars_added - base - LP0.csv'
cfile <- "ELBOW_QUALITYCHECKED_DECEMBER 2_2013_KB_2 - Controlled Vocabulary.xls"
dpath <- file.path('data/raw', dfile)
cpath <- file.path('data/raw', cfile)


# Raw Data
elbows <- read_csv(dpath)
# Diagnosis Vocabulary
control_voc <- readxl::read_excel(cpath,
                                  sheet = "ELBOW_QUALITYCHECKED_DECEMBER 2")

# Save unique Cases with respective Diagnosis
control_voc %>%
  dplyr::select(CaseID = Case_Id, CaseDx = `Controlled Vocabulary`) -> case_dx

elbows %>% left_join(case_dx, by = "CaseID") -> elbows

elbows %>%
  dplyr::select(-TOC, -TOC_capped, -NabN, -posttest_score, -posttest_nabn_score, -CaseCert) -> elbows

# elbows %>% group_by(RaterID) %>% mutate(seq = 1:n()) %>% ungroup() -> elbows
elbows %>% mutate(RaterID = as.character(RaterID)) %>% mutate_if(is.numeric, as.integer)%>% arrange(RaterID) -> elbows
elbows$CaseDx <- relevel(factor(elbows$CaseDx), ref = "Normal")
elbows$CaseType <- relevel(factor(elbows$CaseType), ref = "Normal")

elbows <- within(elbows, CaseType2 <- if_else(CaseDx == "Normal", "Normal", 
                                              if_else(str_detect(CaseDx, "Supracondylar"), 
                                                      "Supracondylar", "Abnormal/Other")))

elbows$CaseType2 <- relevel(factor(elbows$CaseType2), ref = "Normal")
elbows <- ungroup(elbows)
elbows <- elbows %>% filter(Series == "Learning")
write_csv(elbows, "data/interim/elbows_train.csv")



