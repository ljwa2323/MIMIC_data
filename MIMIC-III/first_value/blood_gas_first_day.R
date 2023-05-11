
setwd("./MIMIC-III")

library(dplyr)
library(lubridate)
library(data.table)

icustays <- fread("path/to/icustays.csv", header=T, fill=T)
labevents <- fread("path/to/labevents.csv", header=T, fill=T)
names(icustays)<-tolower(names(icustays))
names(labevents)<-tolower(names(labevents))

labels <- tribble(
  ~itemid, ~label,
  50800, "SPECIMEN",
  50801, "AADO2",
  50802, "BASEEXCESS",
  50803, "BICARBONATE",
  50804, "TOTALCO2",
  50805, "CARBOXYHEMOGLOBIN",
  50806, "CHLORIDE",
  50808, "CALCIUM",
  50809, "GLUCOSE",
  50810, "HEMATOCRIT",
  50811, "HEMOGLOBIN",
  50812, "INTUBATED",
  50813, "LACTATE",
  50814, "METHEMOGLOBIN",
  50815, "O2FLOW",
  50816, "FIO2",
  50817, "SO2",
  50818, "PCO2",
  50819, "PEEP",
  50820, "PH",
  50821, "PO2",
  50822, "POTASSIUM",
  50823, "REQUIREDO2",
  50824, "SODIUM",
  50825, "TEMPERATURE",
  50826, "TIDALVOLUME",
  50827, "VENTILATIONRATE",
  50828, "VENTILATOR"
)

bloodgas <- labevents %>%
  filter(ITEMID %in% labels$itemid) %>%
  mutate(label = labels$label[match(ITEMID, labels$itemid)]) %>%
  select(subject_id, hadm_id, icustay_id, label, charttime, value, valuenum) %>%
  filter(valuenum > 0) %>%
  mutate(
    valuenum = case_when(
      ITEMID == 50810 & valuenum > 100 ~ NA_real_,
      ITEMID == 50816 & valuenum > 100 ~ NA_real_,
      ITEMID == 50817 & valuenum > 100 ~ NA_real_,
      ITEMID == 50815 & valuenum > 70 ~ NA_real_,
      ITEMID == 50821 & valuenum > 800 ~ NA_real_,
      TRUE ~ valuenum
    )
  )

bloodgas_summary <- bloodgas %>%
  group_by(subject_id, hadm_id, icustay_id, charttime) %>%
  summarize(
    SPECIMEN = max(if_else(label == "SPECIMEN", value, NA_character_)),
    AADO2 = max(if_else(label == "AADO2", valuenum, NA_real_)),
    BASEEXCESS = max(if_else(label == "BASEEXCESS", valuenum, NA_real_)),
    BICARBONATE = max(if_else(label == "BICARBONATE", valuenum, NA_real_)),
    TOTALCO2 = max(if_else(label == "TOTALCO2", valuenum, NA_real_)),
    CARBOXYHEMOGLOBIN = max(if_else(label == "CARBOXYHEMOGLOBIN", valuenum, NA_real_)),
    CHLORIDE = max(if_else(label == "CHLORIDE", valuenum, NA_real_)),
    CALCIUM = max(if_else(label == "CALCIUM", valuenum, NA_real_)),
    GLUCOSE = max(if_else(label == "GLUCOSE", valuenum, NA_real_)),
    HEMATOCRIT = max(if_else(label == "HEMATOCRIT", valuenum, NA_real_)),
    HEMOGLOBIN = max(if_else(label == "HEMOGLOBIN", valuenum, NA_real_)),
    INTUBATED = max(if_else(label == "INTUBATED", valuenum, NA_real_)),
    LACTATE = max(if_else(label == "LACTATE", valuenum, NA_real_)),
    METHEMOGLOBIN = max(if_else(label == "METHEMOGLOBIN", valuenum, NA_real_)),
    O2FLOW = max(if_else(label == "O2FLOW", valuenum, NA_real_)),
    FIO2 = max(if_else(label == "FIO2", valuenum, NA_real_)),
    SO2 = max(if_else(label == "SO2", valuenum, NA_real_)),
    PCO2 = max(if_else(label == "PCO2", valuenum, NA_real_)),
    PEEP = max(if_else(label == "PEEP", valuenum, NA_real_)),
    PH = max(if_else(label == "PH", valuenum, NA_real_)),
    PO2 = max(if_else(label == "PO2", valuenum, NA_real_)),
    POTASSIUM = max(if_else(label == "POTASSIUM", valuenum, NA_real_)),
    REQUIREDO2 = max(if_else(label == "REQUIREDO2", valuenum, NA_real_)),
    SODIUM = max(if_else(label == "SODIUM", valuenum, NA_real_)),
    TEMPERATURE = max(if_else(label == "TEMPERATURE", valuenum, NA_real_)),
    TIDALVOLUME = max(if_else(label == "TIDALVOLUME", valuenum, NA_real_)),
    VENTILATIONRATE = max(if_else(label == "VENTILATIONRATE", valuenum, NA_real_)),
    VENTILATOR = max(if_else(label == "VENTILATOR", valuenum, NA_real_))
  ) %>%
  arrange(subject_id, hadm_id, icustay_id, charttime)

fwrite(bloodgas_summary, file = "./data/bloodgas_summary.csv", row.names = FALSE)