
setwd("./MIMIC-III")

# 读入数据
library(data.table)
library(dplyr)

diagnoses_icd <- fread("data/diagnoses_icd.csv", header=T, fill=T)
procedures_icd <- fread("data/procedures_icd.csv", header=T, fill=T)
admissions <- fread("data/admissions.csv", header=T, fill=T)

# 筛选出具有心血管疾病的患者
cardiovascular_patients <- diagnoses_icd %>%
  filter(substring(icd9_code, 1, 3) %in% c('390', '391', '392', '393', '394', '395',
                                           '396', '397', '398', '399', '401', '402', '403', '404', '405', 
                                           '410', '411', '412', '413', '414', '415', '416', '417', '418', 
                                           '420', '421', '422', '423', '424', '425', '426', '427', '428', 
                                           '429', '440', '441', '442', '443', '444', '445', '446', '447', 
                                           '448', '451', '453', '454', '455', '456', '457', '458')) %>%
  select(subject_id, hadm_id)

fwrite(cardiovascular_patients, "./data/cardiovascular_patients.csv", row.names = F)