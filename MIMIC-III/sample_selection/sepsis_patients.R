
# 这段代码使用了dplyr包中的管道操作符%>%，以及mutate()、case_when()、select()、left_join()和if_else()等函数。
# 它将SQL代码中的每个子查询转换为一个dplyr管道中的步骤，并使用left_join()函数将它们连接起来。
# 最后，它使用mutate()和case_when()函数计算出Angus标准的结果，并将结果存储在一个名为angus_sepsis的数据框中。

setwd("/home/luojiawei/mimic3")

library(data.table)
library(dplyr)

# 读入数据
diagnoses_icd <- fread("./mimic3_data/diagnoses_icd.csv", header=T, fill=T)
procedures_icd <- fread("./mimic3_data/procedures_icd.csv", header=T, fill=T)
admissions <- fread("./mimic3_data/admissions.csv", header=T, fill=T)

names(diagnoses_icd) <- tolower(names(diagnoses_icd))
names(procedures_icd) <- tolower(names(procedures_icd))
names(admissions) <- tolower(names(admissions))

# 生成判断 sepsis 的一些辅助表格
infection_group <- diagnoses_icd %>%
  mutate(infection = case_when(
    (substring(icd9_code, 1, 3) %in% c('001', '002', '003', '004', '005', '008',
                                       '009', '010', '011', '012', '013', '014', '015', '016', '017', '018',
                                       '020', '021', '022', '023', '024', '025', '026', '027', '030', '031',
                                       '032', '033', '034', '035', '036', '037', '038', '039', '040', '041',
                                       '090', '091', '092', '093', '094', '095', '096', '097', '098', '100',
                                       '101', '102', '103', '104', '110', '111', '112', '114', '115', '116',
                                       '117', '118', '320', '322', '324', '325', '420', '421', '451', '461',
                                       '462', '463', '464', '465', '481', '482', '485', '486', '494', '510',
                                       '513', '540', '541', '542', '566', '567', '590', '597', '601', '614',
                                       '615', '616', '681', '682', '683', '686', '730')) ~ 1, 
    (substring(icd9_code, 1, 4) %in% c('5695', '5720', '5721', '5750', '5990', '7110',
                                      '7907', '9966', '9985', '9993')) ~ 1, 
    (substring(icd9_code, 1, 5) %in% c('49121', '56201', '56203', '56211', '56213', '56983')) ~ 1, 
    TRUE ~ 0)) %>%
  select(subject_id, hadm_id, infection) %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE)

organ_diag_group <- diagnoses_icd %>%
  mutate(organ_dysfunction = case_when(
    (substring(icd9_code, 1, 3) %in% c('458', '293', '570', '584')) ~ 1, 
    (substring(icd9_code, 1, 4) %in% c('7855', '3483', '3481', '2874', '2875', '2869', '2866', '5734')) ~ 1, 
    TRUE ~ 0), 
    explicit_sepsis = case_when(
      (substring(icd9_code, 1, 5) %in% c('99592', '78552')) ~ 1, 
      TRUE ~ 0)) %>%
  select(subject_id, hadm_id, organ_dysfunction, explicit_sepsis) %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE)

organ_proc_group <- procedures_icd %>%
  mutate(mech_vent = case_when(
    (substring(icd9_code, 1, 4) %in% c('9670', '9671', '9672')) ~ 1, 
    TRUE ~ 0)) %>%
  select(subject_id, hadm_id, mech_vent)  %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE)

aggregate <- admissions %>%
  left_join(infection_group, by = c('subject_id', 'hadm_id')) %>%
  left_join(organ_diag_group, by = c('subject_id', 'hadm_id')) %>%
  left_join(organ_proc_group, by = c('subject_id', 'hadm_id')) %>%
  select(subject_id, hadm_id, infection, explicit_sepsis, organ_dysfunction, mech_vent)  %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE)


# 标记 sepsis 的样本
angus_sepsis <- aggregate %>%
  mutate(angus = case_when(
    (explicit_sepsis == 1) ~ 1, 
    (infection == 1 & organ_dysfunction == 1) ~ 1, 
    (infection == 1 & mech_vent == 1) ~ 1, 
    TRUE ~ 0)) %>%
  select(subject_id, hadm_id, infection, explicit_sepsis, organ_dysfunction, mech_vent, angus) %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE)

fwrite(angus_sepsis, "./data/angus_sepsis.csv", row.names = F)


angus_sepsis %>%
  filter(angus == 1) %>%
  slice_head(n = 10) 