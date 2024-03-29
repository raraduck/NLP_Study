label_df <- read.csv("week2/CXR_label.csv", stringsAsFactors = FALSE)
label_tbl <- as_tibble(label_df)

simple_tbl<- label_tbl %>%
  select(-path, -ViewCodeSequence_CodeMeaning, -ViewPosition)
simple_tbl
write.csv(simple_tbl, "week2/CXR_label_simple.csv", row.names = FALSE)

# 중복된 항목을 합치기 위해 group_by()와 summarise() 사용
unique_tbl <- simple_tbl %>%
  group_by(subject_id, study_id) %>%
  summarise(across(c(Atelectasis:Tortuous.Aorta), max, na.rm = TRUE), .groups = 'drop')
unique_tbl
write.csv(unique_tbl, "week2/CXR_label_unique.csv", row.names = FALSE)


data_tbl <- data_tbl %>%
  mutate(
    subject_id = as.integer(subject_id),
    study_id = as.integer(study_id)
  )
data_tbl

merged_tbl <- inner_join(data_tbl, unique_tbl)
merged_tbl <- merged_tbl %>% select(doc_id, group_id, subject_id, study_id, Atelectasis:Tortuous.Aorta, text)
write.csv(merged_tbl, "week2/CXR_label_merged.csv", row.names = FALSE)

