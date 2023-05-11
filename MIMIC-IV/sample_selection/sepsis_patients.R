
setwd("/home/luojiawei/mimic4_work/mimic4_data_extract/")

library(dplyr)
library(data.table)

# 读入数据
diagnoses_icd <- fread("/home/luojiawei/mimic4/mimic-iv-2.1/hosp/diagnoses_icd.csv", header=T, fill=T)
procedures_icd <- fread("/home/luojiawei/mimic4/mimic-iv-2.1/hosp/procedures_icd.csv", header=T, fill=T)
admissions <- fread("/home/luojiawei/mimic4/mimic-iv-2.1/hosp/admissions.csv", header=T, fill=T)

# 创建新列
infection_group <- diagnoses_icd %>%
  mutate(infection = case_when(
    (icd_version == 9 & substring(icd_code, 1, 3) %in% c('001','002','003','004','005','008',
                                       '009','010','011','012','013','014','015','016','017','018',
                                       '020','021','022','023','024','025','026','027','030','031',
                                       '032','033','034','035','036','037','038','039','040','041',
                                       '090','091','092','093','094','095','096','097','098','100',
                                       '101','102','103','104','110','111','112','114','115','116',
                                       '117','118','320','322','324','325','420','421','451','461',
                                       '462','463','464','465','481','482','485','486','494','510',
                                       '513','540','541','542','566','567','590','597','601','614',
                                       '615','616','681','682','683','686','730'))~1,
    (icd_version == 9 & substring(icd_code, 1, 4) %in% c('5695','5720','5721','5750','5990','7110',
                                       '7907','9966','9985','9993'))~1,
    (icd_version == 9 & substring(icd_code, 1, 5) %in% c('49121','56201','56203','56211','56213',
                                       '56983'))~1,
    (icd_version == 10 & substring(icd_code, 1, 3) %in% c('A40','A41','A48','B37','J09','J10',
                                       'J11','J12','J13','J14','J15','J16','J17','J18','J85',
                                       'J86','J93','J95','J96','J98','K65','K66','N10','N11',
                                       'N13','N15','N17','N18','N19','N30','N34','N39','O85',
                                       'R65','T80','Z16','Z22','Z86'))~1,
    TRUE ~ 0
  )) %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE) %>%
  group_by(subject_id, hadm_id) 

organ_diag_group <- diagnoses_icd %>%
  mutate(
    organ_dysfunction = case_when(
      (icd_version == 9 & substring(icd_code, 1, 3) %in% c('458','293','570','584'))~1,
      (icd_version == 9 & substring(icd_code, 1, 4) %in% c('7855','3483','3481',
                                         '2874','2875','2869','2866','5734'))~1,
      (icd_version == 10 & substring(icd_code, 1, 3) %in% c('K85','K86','K90','K91'))~1,
      TRUE~0
    ),
    explicit_sepsis = case_when(
      (icd_version == 9 & substring(icd_code, 1, 5) %in% c('99592','78552'))~1,
      (icd_version == 10 & substring(icd_code, 1, 6) %in% c('R6521','R6522','R6523','R6524','R6525','R6526','R6527','R6528'))~1,
      TRUE~0
    )
  ) %>% distinct(subject_id, hadm_id, .keep_all = TRUE) %>%
  group_by(subject_id, hadm_id) 

organ_proc_group <- procedures_icd %>%
    mutate(
    mech_vent = case_when(
        (icd_version == 9 & substring(icd_code, 1, 4) %in% c('9670','9671','9672'))~1,
        (icd_version == 10 & substring(icd_code, 1, 5) %in% c('5A1945Z','5A1955Z','5A1965Z'))~1,
        TRUE~0
    )
    ) %>%
    group_by(subject_id, hadm_id) %>% 
  distinct(subject_id, hadm_id, .keep_all = TRUE)

# 合并数据
sepsis_data <- admissions %>%
  left_join(infection_group, by = c("subject_id", "hadm_id")) %>%
  left_join(organ_diag_group, by = c("subject_id", "hadm_id")) %>%
  left_join(organ_proc_group, by = c("subject_id", "hadm_id")) %>%
  mutate(
    angus = case_when(
      (explicit_sepsis == 1)~1,
      (infection == 1 & organ_dysfunction == 1)~1,
      (infection == 1 & mech_vent == 1)~1,
      TRUE~0
    )
  ) %>%
  distinct(subject_id, hadm_id, .keep_all = TRUE)  %>% 
  select(subject_id, hadm_id, infection, explicit_sepsis, organ_dysfunction, mech_vent, angus)


# 将 sepsis_data输出到外部 csv 文件
fwrite(sepsis_data, file = "./sepsis_data.csv", row.names = FALSE)
