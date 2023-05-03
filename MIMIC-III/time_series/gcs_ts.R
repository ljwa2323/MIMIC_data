
setwd("./MIMIC-III")

library(data.table)
library(dplyr)
library(padr)

# 读入数据
icustays <- fread("./data/icustays.csv", header=T, fill=T)
chartevents <- fread("./data/chartevents.csv", header=T, fill=T)

gcs_all <- chartevents %>%
  inner_join(icustays, by = "icustay_id") %>%
  mutate(itemid = case_when(
    itemid %in% c(723, 223900) ~ 723,
    itemid %in% c(454, 223901) ~ 454,
    itemid %in% c(184, 220739) ~ 184,
    TRUE ~ itemid
  )) %>%
  mutate(valuenum = case_when(
    itemid == 723 & value == "1.0 ET/Trach" ~ 0,
    itemid == 223900 & value == "No Response-ETT" ~ 0,
    TRUE ~ valuenum
  )) %>%
  filter(itemid %in% c(184, 454, 723, 223900, 223901, 220739)) %>%
  filter(charttime >= intime) %>%
  filter(error != 1) %>%
  group_by(icustay_id, charttime) %>%
  summarize(
    GCSMotor = max(ifelse(itemid == 454, valuenum, NA), na.rm = TRUE),
    GCSVerbal = max(ifelse(itemid == 723, valuenum, NA), na.rm = TRUE),
    GCSEyes = max(ifelse(itemid == 184, valuenum, NA), na.rm = TRUE),
    EndoTrachFlag = ifelse(max(ifelse(itemid == 723, valuenum, NA), na.rm = TRUE) == 0, 1, 0),
    rn = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    GCSVerbalPrev = lag(GCSVerbal),
    GCSMotorPrev = lag(GCSMotor),
    GCSEyesPrev = lag(GCSEyes)
  ) %>%
  mutate(
    GCS = case_when(
      GCSVerbal == 0 ~ 15,
      is.na(GCSVerbal) & GCSVerbalPrev == 0 ~ 15,
      GCSVerbalPrev == 0 ~ coalesce(GCSMotor, 6) + coalesce(GCSVerbal, 5) + coalesce(GCSEyes, 4),
      TRUE ~ coalesce(GCSMotor, coalesce(GCSMotorPrev, 6)) + coalesce(GCSVerbal, coalesce(GCSVerbalPrev, 5)) + coalesce(GCSEyes, coalesce(GCSEyesPrev, 4))
    )
  ) %>%
  select(subject_id, hadm_id, icustay_id, charttime, GCS, GCSMotor, GCSVerbal, GCSEyes, EndoTrachFlag) %>%
  arrange(icustay_id, charttime) %>%
  group_by(hadm_id, charttime) %>%
  summarize(
    GCS = max(GCS),
    GCSMotor = max(GCSMotor),
    GCSVerbal = max(GCSVerbal),
    GCSEyes = max(GCSEyes)
  ) %>%
  ungroup()

