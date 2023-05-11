
setwd("/home/luojiawei/mimic3")

library(data.table)
library(dplyr)
library(padr)

# 读入数据
icustays <- fread("./mimic3_data/icustays.csv", header=T, fill=T)
chartevents <- fread("./mimic3_data/chartevents.csv", header=T, fill=T)
names(icustays)<-tolower(names(icustays))
names(chartevents)<-tolower(names(chartevents))

mengcz_vital_ts <- icustays %>%
  left_join(chartevents, by = c("subject_id", "hadm_id", "icustay_id")) %>%
  filter(charttime >= intime,
         error != 1,
         itemid %in% c(211, 220045, 51, 442, 455, 6701, 220179, 220050,
                       8368, 8440, 8441, 8555, 220180, 220051, 456, 52, 6702,
                       443, 220052, 220181, 225312, 615, 618, 220210, 224690,
                       223761, 678, 223762, 676, 646, 220277, 807, 811, 1529,
                       3745, 3744, 225664, 220621, 226537)) %>%
  mutate(VitalID = case_when(
    itemid %in% c(211, 220045) & valuenum > 0 & valuenum < 300 ~ 1,
    itemid %in% c(51, 442, 455, 6701, 220179, 220050) & valuenum > 0 & valuenum < 400 ~ 2,
    itemid %in% c(8368, 8440, 8441, 8555, 220180, 220051) & valuenum > 0 & valuenum < 300 ~ 3,
    itemid %in% c(456, 52, 6702, 443, 220052, 220181, 225312) & valuenum > 0 & valuenum < 300 ~ 4,
    itemid %in% c(615, 618, 220210, 224690) & valuenum > 0 & valuenum < 70 ~ 5,
    itemid %in% c(223761, 678) & valuenum > 70 & valuenum < 120 ~ 6,
    itemid %in% c(223762, 676) & valuenum > 10 & valuenum < 50 ~ 6,
    itemid %in% c(646, 220277) & valuenum > 0 & valuenum <= 100 ~ 7,
    itemid %in% c(807, 811, 1529, 3745, 3744, 225664, 220621, 226537) & valuenum > 0 ~ 8,
    TRUE ~ NA_real_
  )) %>%
  mutate(valuenum = if_else(itemid %in% c(223761, 678), (valuenum - 32) / 1.8, valuenum)) %>%
  select(subject_id, hadm_id, icustay_id, charttime, VitalID, valuenum) %>%
  group_by(hadm_id, charttime) %>%
  summarize(
    HeartRate_Min = min(valuenum[VitalID == 1], na.rm = TRUE),
    HeartRate_Max = max(valuenum[VitalID == 1], na.rm = TRUE),
    HeartRate_Mean = mean(valuenum[VitalID == 1], na.rm = TRUE),
    SysBP_Min = min(valuenum[VitalID == 2], na.rm = TRUE),
    SysBP_Max = max(valuenum[VitalID == 2], na.rm = TRUE),
    SysBP_Mean = mean(valuenum[VitalID == 2], na.rm = TRUE),
    DiasBP_Min = min(valuenum[VitalID == 3], na.rm = TRUE),
    DiasBP_Max = max(valuenum[VitalID == 3], na.rm = TRUE),
    DiasBP_Mean = mean(valuenum[VitalID == 3], na.rm = TRUE),
    MeanBP_Min = min(valuenum[VitalID == 4], na.rm = TRUE),
    MeanBP_Max = max(valuenum[VitalID == 4], na.rm = TRUE),
    MeanBP_Mean = mean(valuenum[VitalID == 4], na.rm = TRUE),
    RespRate_Min = min(valuenum[VitalID == 5], na.rm = TRUE),
    RespRate_Max = max(valuenum[VitalID == 5], na.rm = TRUE),
    RespRate_Mean = mean(valuenum[VitalID == 5], na.rm = TRUE),
    TempC_Min = min(valuenum[VitalID == 6], na.rm = TRUE),
    TempC_Max = max(valuenum[VitalID == 6], na.rm = TRUE),
    TempC_Mean = mean(valuenum[VitalID == 6], na.rm = TRUE),
    SpO2_Min = min(valuenum[VitalID == 7], na.rm = TRUE),
    SpO2_Max = max(valuenum[VitalID == 7], na.rm = TRUE),
    SpO2_Mean = mean(valuenum[VitalID == 7], na.rm = TRUE),
    Glucose_Min = min(valuenum[VitalID == 8], na.rm = TRUE),
    Glucose_Max = max(valuenum[VitalID == 8], na.rm = TRUE),
    Glucose_Mean = mean(valuenum[VitalID == 8], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(hadm_id, charttime)


mengcz_vital_ts_wide <- mengcz_vital_ts %>%
  pivot_wider(
    id_cols = c("subject_id", "hadm_id", "icustay_id", "charttime"),
    names_from = "VitalID",
    values_from = "valuenum"
  )


mengcz_vital_ts_wide <- mengcz_vital_ts_wide %>%
  arrange(subject_id, hadm_id, icustay_id, charttime) %>%
  group_by(subject_id, hadm_id) %>%
  pad(interval = "1 day", start_val = first(charttime), end_val = last(charttime)) %>%
  fill(everything(), .direction = "up")
