setwd("./MIMIC-III")

library(data.table)
library(dplyr)

metavision<-fread("./data/metavision.csv",header=T, fill=T)
chartevents<-fread("./data/chartevents.csv",header=T, fill=T)
d_items<-fread("./data/d_items.csv",header=T, fill=T)

# 查看需要用到的字段长什么样子
df <- metavision %>%
  filter(dbsource == 'metavision') %>%
  filter(str_detect(label, 'crrt', case = FALSE)) %>%
  select(itemid, label, category, linksto)
head(df)

# 获取与CRRT相关的itemid列表
crrt <- metavision %>%
  filter(dbsource == "metavision",
         grepl("dialy|crrt", label, ignore.case = TRUE) | category == "Dialysis") %>%
  arrange(linksto, category, label)


df_chart <- chartevents %>%
  inner_join(d_items, by = "itemid") %>%
  filter(icustay_id == 246866,
         itemid %in% c(224404, 224406, 228004, 224145, 225183, 
                       224149, 224144, 224154, 224151, 224150, 224191, 228005, 
                       228006, 224153, 224152, 226457)) %>%
  select(icustay_id, label, charttime, value, valueuom) %>%
  arrange(icustay_id, charttime, label)


print_itemid_info <- function(d_items, chartevents, itemid) {
  # get name of itemid
    df <- d_items %>%
    filter(itemid == itemid) %>%
        select(label)

    cat(sprintf("Values for %s - %s...\n", itemid, df$label))

    # get statistics of itemid
    df <- chartevents %>%
    filter(itemid == itemid) %>%
        group_by(value) %>%
        summarize(number_of_patients = n_distinct(icustay_id),
                    number_of_observations = n()) %>%
        arrange(value)

    df[is.na(df)] <- ""
    print(xtable::xtable(df, digits = 0), type = "html", include.rownames = FALSE)
}

t1 <- chartevents %>%
  filter(itemid %in% c(227290, 228004, 225958, 224145, 225183, 224149, 
                       224144, 225977, 224154, 224151, 224150, 224191, 228005, 
                       228006, 225976, 224153, 224152, 226457)) %>%
  group_by(icustay_id) %>%
  summarize(HasMode = max(ifelse(itemid == 227290, 1, 0))) %>%
  ungroup() %>%
  summarize(Num_ICUSTAY_ID = n(), Num_With_Mode = sum(HasMode))
