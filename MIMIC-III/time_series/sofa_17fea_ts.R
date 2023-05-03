

setwd("./MIMIC-III")

library(data.table)
library(dplyr)
library(padr)
library(lubridate)

# 读入数据
icustays <- fread("./data/icustays.csv", header=T, fill=T)
chartevents <- fread("./data/chartevents.csv", header=T, fill=T)

cpap <- icustays %>%
  inner_join(chartevents, by = "icustay_id") %>%
  filter(itemid %in% c(467, 469, 226732),
         charttime >= intime,
        #  charttime <= intime + days(1), # 没有了结束时间的限制
         str_detect(tolower(value), "(cpap mask|bipap mask)")) %>%
  group_by(icustay_id) %>%
  summarize(starttime = min(charttime) - hours(1),
            endtime = max(charttime) + hours(4),
            cpap = max(str_detect(tolower(value), "(cpap mask|bipap mask)")))

surgflag <- admissions %>%
  left_join(services, by = "hadm_id") %>%
  mutate(surgical = if_else(str_detect(tolower(curr_service), "surg"), 1, 0)) %>%
  group_by(hadm_id) %>%
  mutate(serviceOrder = row_number(transferTime)) %>%
  ungroup()

comorb <- diagnoses_icd %>%
  mutate(icd9_code = as.character(icd9_code)) %>%
  group_by(hadm_id) %>%
  summarize(AIDS = max(icd9_code >= "042" & icd9_code <= "0449"),
            HEM = max(icd9_code %in% c("1960", "1961", "1963", "1965", "1966", "1968", "1969", "1991", "20970", "20971", "20972", "20973", "20974", "20975", "20979", "78951")),
            METS = max(icd9_code >= "1962" & icd9_code <= "1991" | icd9_code %in% c("20970", "20971", "20972", "20973", "20974", "20975", "20979", "78951")))

pafi1 <- bloodgas2dayarterial %>%
  left_join(ventdurations, by = "icustay_id") %>%
  left_join(cpap, by = "icustay_id") %>%
  mutate(vent = if_else(!is.na(starttime) & charttime >= starttime & charttime <= endtime, 1, 0),
         cpap = if_else(!is.na(cpap), 1, 0)) %>%
  select(icustay_id, charttime, PaO2FiO2, vent, cpap)

pafi2 <- pafi1 %>%
  filter(vent == 1 | cpap == 1) %>%
  group_by(icustay_id) %>%
  summarize(PaO2FiO2_vent_min = min(PaO2FiO2))

mengcz_17features <- icustays %>%
  inner_join(admissions, by = "hadm_id") %>%
  inner_join(patients, by = "subject_id") %>%
  left_join(pafi2, by = "icustay_id") %>%
  left_join(surgflag %>% filter(serviceOrder == 1), by = "hadm_id") %>%
  left_join(comorb, by = "hadm_id") %>%
  left_join(gcs2day, by = "icustay_id") %>%
  left_join(vitals2day, by = "icustay_id") %>%
  left_join(uo2day, by = "icustay_id") %>%
  left_join(labs2day, by = "icustay_id") %>%
  select(subject_id, hadm_id, icustay_id, intime, outtime,
         age = round(as.numeric(intime - dob) / 365.242, 2),
         heartrate_max, heartrate_min, sysbp_max, sysbp_min, tempc_max, tempc_min,
         PaO2FiO2_vent_min, urineoutput,
         bun_min, bun_max, wbc_min, wbc_max, potassium_min, potassium_max, sodium_min, sodium_max, bicarbonate_min, bicarbonate_max, bilirubin_min, bilirubin_max,
         mingcs, AIDS, HEM, METS,
         AdmissionType = case_when(ADMISSION_TYPE == "ELECTIVE" & surgical == 1 ~ "ScheduledSurgical",
                                    ADMISSION_TYPE != "ELECTIVE" & surgical == 1 ~ "UnscheduledSurgical",
                                    TRUE ~ "Medical")) %>%
  arrange(hadm_id, intime)


fwrite(mengcz_17features, file = "./data/mengcz_17features.csv", row.names = FALSE)