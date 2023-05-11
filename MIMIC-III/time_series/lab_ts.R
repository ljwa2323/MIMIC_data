
setwd("./MIMIC-III")

library(data.table)
library(dplyr)
library(padr)

# 读入数据
icustays <- fread("./data/icustays.csv", header=T, fill=T)
labevents <- fread("./data/labevents.csv", header=T, fill=T)
names(icustays)<-tolower(names(icustays))
names(labevents)<-tolower(names(labevents))

df <- icustays %>%
  left_join(labevents, by = c("subject_id", "hadm_id")) %>%
  filter(charttime >= (intime - hours(6))) %>%
  filter(itemid %in% c(50868, 50862, 51144, 50882, 50885, 50912, 50902, 50806, 
                       50931, 50809, 51221, 50810, 51222, 50811, 50813, 51265, 50971,
                       50822, 51275, 51237, 51274, 50824, 50983, 51006, 51300, 51301)) %>%
  mutate(label = case_when(
    itemid == 50868 ~ "ANION GAP",
    itemid == 50862 ~ "ALBUMIN",
    itemid == 51144 ~ "BANDS",
    itemid == 50882 ~ "BICARBONATE",
    itemid == 50885 ~ "BILIRUBIN",
    itemid == 50912 ~ "CREATININE",
    itemid == 50902 ~ "CHLORIDE",
    itemid == 50806 ~ "CHLORIDE, WHOLE BLOOD",
    itemid == 50931 ~ "GLUCOSE",
    itemid == 50809 ~ "GLUCOSE",
    itemid == 51221 ~ "HEMATOCRIT",
    itemid == 50810 ~ "HEMATOCRIT, CALCULATED",
    itemid == 51222 ~ "HEMOGLOBIN",
    itemid == 50811 ~ "HEMOGLOBIN",
    itemid == 50813 ~ "LACTATE",
    itemid == 51265 ~ "PLATELET COUNT",
    itemid == 50971 ~ "POTASSIUM",
    itemid == 50822 ~ "POTASSIUM, WHOLE BLOOD",
    itemid == 51275 ~ "PTT",
    itemid == 51237 ~ "INR(PT)",
    itemid == 51274 ~ "PT",
    itemid == 50824 ~ "SODIUM, WHOLE BLOOD",
    itemid == 50983 ~ "SODIUM",
    itemid == 51006 ~ "UREA NITROGEN",
    itemid == 51300 ~ "WBC COUNT",
    itemid == 51301 ~ "WBC COUNT"
  )) %>%
  select(subject_id, hadm_id, charttime, label, valuenum) %>%
  group_by(subject_id, hadm_id, charttime, label) %>%
  summarise(valuenum = mean(valuenum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(charttime = as.POSIXct(charttime)) %>%
  arrange(subject_id, hadm_id, charttime, label)


df <- df %>% 
  mutate(valuenum = ifelse(
    itemid == 50862 & valuenum > 10, NA_real_,
    itemid == 50868 & valuenum > 10000, NA_real_,
    itemid == 51144 & (valuenum < 0 | valuenum > 100), NA_real_,
    itemid == 50882 & valuenum > 10000, NA_real_,
    itemid == 50885 & valuenum > 150, NA_real_,
    itemid == 50806 & valuenum > 10000, NA_real_,
    itemid == 50902 & valuenum > 10000, NA_real_,
    itemid == 50912 & valuenum > 150, NA_real_,
    itemid == 50809 & valuenum > 10000, NA_real_,
    itemid == 50931 & valuenum > 10000, NA_real_,
    itemid == 50810 & valuenum > 100, NA_real_,
    itemid == 51221 & valuenum > 100, NA_real_,
    itemid == 50811 & valuenum > 50, NA_real_,
    itemid == 51222 & valuenum > 50, NA_real_,
    itemid == 50813 & valuenum > 50, NA_real_,
    itemid == 51265 & valuenum > 10000, NA_real_,
    itemid == 50822 & valuenum > 30, NA_real_,
    itemid == 50971 & valuenum > 30, NA_real_,
    itemid == 51275 & valuenum > 150, NA_real_,
    itemid == 51237 & valuenum > 50, NA_real_,
    itemid == 51274 & valuenum > 150, NA_real_,
    itemid == 50824 & valuenum > 200, NA_real_,
    itemid == 50983 & valuenum > 200, NA_real_,
    itemid == 51006 & valuenum > 300, NA_real_,
    itemid == 51300 & valuenum > 1000, NA_real_,
    itemid == 51301 & valuenum > 1000, NA_real_,
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
