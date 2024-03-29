library(multilinguer)
library(KoNLP)
library(dplyr)
library(stringr)


# 필요한 라이브러리를 로드합니다. 없을 경우 설치합니다.
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("tidytext")) install.packages("tidytext")
if (!require("readr")) install.packages("readr")
# 필요한 라이브러리를 로드합니다. 없으면 설치합니다.
if (!require("tm")) install.packages("tm")
library(tm)
library(tidyverse)
library(readr)
library(tidytext)

# 파일을 읽어들일 폴더의 경로를 지정합니다.
folder_path <- "week2/data/p10"

# 해당 경로에서 .txt 파일의 목록을 재귀적으로 가져옵니다.
file_paths <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
file_paths

# 각 파일을 읽어들여 하나의 tibble에 저장합니다.
data_tbl <- map_dfr(file_paths, function(el) {
  data <- read_lines(el) %>%
    paste(collapse = "\n") %>%
    tibble(text = .) %>%
    mutate(
      group_id = basename(dirname(dirname(el))),
      subject_id = basename(dirname(el)),
      study_id = basename(el),
    ) %>%
    mutate(
      subject_id = str_extract(subject_id, "\\d+"),
      study_id = str_extract(study_id, "\\d+")
    )
}) %>% 
  mutate(doc_id = row_number()) # 여기에서 doc_id를 추가합니다.

data_tbl <- data_tbl %>% select(doc_id, group_id, subject_id, study_id, text)
data_tbl

docs <- VCorpus(DataframeSource(data_tbl))
docs


# 이미 생성된 VCorpus 객체(docs)에 대해
# 각 문서에 group_id, subject_id, study_id 메타데이터 추가
for (i in 1:length(docs)) {
  # 각 문서의 group_id, subject_id, study_id 가져와서 메타데이터에 설정
  meta(docs[[i]], tag = "group_id") <- data_tbl$group_id[i]
  meta(docs[[i]], tag = "subject_id") <- data_tbl$subject_id[i]
  meta(docs[[i]], tag = "study_id") <- data_tbl$study_id[i]
}
docs
target_docs = c(1,2,3)
# lapply(docs, meta)[target_docs]
# lapply(docs, content)[target_docs]
docs_tm <- docs
# docs_tm <- tm_map(docs_tm, content_transformer(tolower))
# docs_tm <- tm_map(docs_tm, stripWhitespace)
docs_tm <- tm_map(docs_tm, content_transformer(trimws))
lapply(docs_tm, content)[target_docs]
lapply(docs_tm, meta)[3]
