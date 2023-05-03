
setwd("./MIMIC-III")

library(data.table)
library(dplyr)
library(padr)

# 读入数据
icustays <- fread("./data/icustays.csv", header=T, fill=T)
labevents <- fread("./data/labevents.csv", header=T, fill=T)


df <- icustays %>%
  left_join(labevents, by = c("subject_id", "hadm_id")) %>%
  filter(charttime >= (intime - hours(6))) %>%
  filter(ITEMID %in% c(50868, 50862, 51144, 50882, 50885, 50912, 50902, 50806, 
                       50931, 50809, 51221, 50810, 51222, 50811, 50813, 51265, 50971,
                       50822, 51275, 51237, 51274, 50824, 50983, 51006, 51300, 51301)) %>%
  mutate(label = case_when(
    ITEMID == 50868 ~ "ANION GAP",
    ITEMID == 50862 ~ "ALBUMIN",
    ITEMID == 51144 ~ "BANDS",
    ITEMID == 50882 ~ "BICARBONATE",
    ITEMID == 50885 ~ "BILIRUBIN",
    ITEMID == 50912 ~ "CREATININE",
    ITEMID == 50902 ~ "CHLORIDE",
    ITEMID == 50806 ~ "CHLORIDE, WHOLE BLOOD",
    ITEMID == 50931 ~ "GLUCOSE",
    ITEMID == 50809 ~ "GLUCOSE",
    ITEMID == 51221 ~ "HEMATOCRIT",
    ITEMID == 50810 ~ "HEMATOCRIT, CALCULATED",
    ITEMID == 51222 ~ "HEMOGLOBIN",
    ITEMID == 50811 ~ "HEMOGLOBIN",
    ITEMID == 50813 ~ "LACTATE",
    ITEMID == 51265 ~ "PLATELET COUNT",
    ITEMID == 50971 ~ "POTASSIUM",
    ITEMID == 50822 ~ "POTASSIUM, WHOLE BLOOD",
    ITEMID == 51275 ~ "PTT",
    ITEMID == 51237 ~ "INR(PT)",
    ITEMID == 51274 ~ "PT",
    ITEMID == 50824 ~ "SODIUM, WHOLE BLOOD",
    ITEMID == 50983 ~ "SODIUM",
    ITEMID == 51006 ~ "UREA NITROGEN",
    ITEMID == 51300 ~ "WBC COUNT",
    ITEMID == 51301 ~ "WBC COUNT"
  )) %>%
  select(subject_id, hadm_id, charttime, label, valuenum) %>%
  group_by(subject_id, hadm_id, charttime, label) %>%
  summarise(valuenum = mean(valuenum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(charttime = as.POSIXct(charttime)) %>%
  arrange(subject_id, hadm_id, charttime, label)


df <- df %>% 
  mutate(valuenum = ifelse(
    ITEMID == 50862 & valuenum > 10, NA_real_,
    ITEMID == 50868 & valuenum > 10000, NA_real_,
    ITEMID == 51144 & (valuenum < 0 | valuenum > 100), NA_real_,
    ITEMID == 50882 & valuenum > 10000, NA_real_,
    ITEMID == 50885 & valuenum > 150, NA_real_,
    ITEMID == 50806 & valuenum > 10000, NA_real_,
    ITEMID == 50902 & valuenum > 10000, NA_real_,
    ITEMID == 50912 & valuenum > 150, NA_real_,
    ITEMID == 50809 & valuenum > 10000, NA_real_,
    ITEMID == 50931 & valuenum > 10000, NA_real_,
    ITEMID == 50810 & valuenum > 100, NA_real_,
    ITEMID == 51221 & valuenum > 100, NA_real_,
    ITEMID == 50811 & valuenum > 50, NA_real_,
    ITEMID == 51222 & valuenum > 50, NA_real_,
    ITEMID == 50813 & valuenum > 50, NA_real_,
    ITEMID == 51265 & valuenum > 10000, NA_real_,
    ITEMID == 50822 & valuenum > 30, NA_real_,
    ITEMID == 50971 & valuenum > 30, NA_real_,
    ITEMID == 51275 & valuenum > 150, NA_real_,
    ITEMID == 51237 & valuenum > 50, NA_real_,
    ITEMID == 51274 & valuenum > 150, NA_real_,
    ITEMID == 50824 & valuenum > 200, NA_real_,
    ITEMID == 50983 & valuenum > 200, NA_real_,
    ITEMID == 51006 & valuenum > 300, NA_real_,
    ITEMID == 51300 & valuenum > 1000, NA_real_,
    ITEMID == 51301 & valuenum > 1000, NA_real_,
    TRUE, valuenum
  ))

wide_df <- df %>%
  group_by(subject_id, hadm_id, charttime) %>%
  pivot_wider(names_from = label, values_from = valuenum)


wide_df <- wide_df %>%
  arrange(subject_id, hadm_id, charttime) %>%
  group_by(subject_id, hadm_id) %>%
  pad(interval = "1 day", start_val = first(charttime), end_val = last(charttime)) %>%
  fill(everything(), .direction = "up")
