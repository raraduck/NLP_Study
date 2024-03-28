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
library(stringi) # stri_replace_all_fixed 사용


# 파일을 읽어들일 폴더의 경로를 지정합니다.
folder_path <- "E:/workspace/NLP_Study/week2/data/p10"


# 해당 경로에서 .txt 파일의 목록을 재귀적으로 가져옵니다.
file_paths <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
file_paths
corpora <- lapply(file_paths, function(el){
  text <- read_lines(el) %>% 
    paste(collapse = "") %>% 
    str_trim()
  corpus <- VCorpus(VectorSource(text))
  # 파일 이름과 상위 폴더 이름 추출
  filename <- basename(el)
  patient_id <- basename(dirname(el))
  study_id <- basename(dirname(dirname(el)))
  # 각 VCorpus 문서의 메타 데이터에 report_id 삽입
  meta(corpus[[1]], "filename") <- filename
  meta(corpus[[1]], "pid") <- patient_id
  meta(corpus[[1]], "sid") <- study_id
  return(corpus)
})
corpora
corpus <- corpora[[812]]
corpus[[1]]$meta
corpus[[1]]$content
lapply(corpus, content)

# 소문자 변환
class(corpora)
corpora_lower <- lapply(corpora, function(corpus){
  tm_map(corpus, content_transformer(tolower))
})
content(corpora_lower[[1]][[1]])
lapply(corpora_lower[[1]], content)
corpora_lower[[1]][[1]]$meta

# 불용어, 패턴, 문장부호 제거 (no 까지 제거해버리는 문제 있음)
myRemove <- function(x, pattern){
  return(gsub(pattern, "", x))
}
corpora_words <- lapply(corpora_lower, function(corpus){
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, content_transformer(myRemove), "(//)|(final report examination)|(indication)|(findings)|(impression)")
  # removePunctuation 적용
  corpus <- tm_map(corpus, removePunctuation)
  # content_transformer를 사용하여 trimws 적용
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(trimws))
  return(corpus)
})
content(corpora_words[[1]][[1]])
lapply(corpora_words[[1]], content)
corpora_words[[1]][[1]]$meta

# 변환 규칙 사전 정의
# read.csv를 사용하여 변환 규칙 파일 불러오기
rules_df <- read.csv("week2/replacement_rules_r.csv", stringsAsFactors = FALSE)
rules_df
# 데이터 프레임을 리스트로 변환
replacement_rules <- setNames(as.list(rules_df$replacement), rules_df$original)
replacement_rules
# 사용자 정의 치환 함수
replace_custom <- function(text, rules) {
  for (pattern in names(rules)) {
    replacement <- rules[[pattern]]
    # 단어 경계를 추가하여 정확한 단어 매칭이 이루어지도록 함
    pattern_with_boundaries <- paste0("\\b", pattern, "\\b")
    text <- stri_replace_all_regex(str = text, pattern = pattern_with_boundaries, replacement = replacement, vectorize_all = FALSE)
    # text <- stri_replace_all_fixed(str = text, pattern = pattern, replacement = replacement, vectorize_all = FALSE)
  }
  return(text)
}

# 축약단어 치환
corpora_temp <- corpora_words # corpora_stem # corpora_words
corpora_replace <- lapply(corpora_temp, function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_custom), rules = replacement_rules)
  return(corpus)
})
content(corpora_lower[[1]][[1]])
content(corpora_words[[1]][[1]])
content(corpora_replace[[1]][[1]])
corpora_replace[[1]][[1]]$meta

# # 어간추출 및 검토 (프로세스에서 제외)
# corpora_stem <- lapply(corpora_replace, function(corpus){
#   corpus <- tm_map(corpus, stemDocument)
#   return(corpus)
# })
# content(corpora_lower[[1]][[1]])
# content(corpora_words[[1]][[1]])
# content(corpora_replace[[1]][[1]])
# content(corpora_stem[[1]][[1]])

corpus1_tidy <- tidy(corpora_replace[[1]]) %>%
  select(c("sid", "pid", "text")) %>%
  unnest_tokens(output=word, input=text)
corpus1_tidy
class(corpus1_tidy)
corpus1_tidy %>% print(n=Inf)

corpus1_tidy %>% 
  count(word) %>%
  arrange(desc(n))

