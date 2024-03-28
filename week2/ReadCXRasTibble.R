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
folder_path <- "E:/workspace/NLP_Study/week2/data/p10/p10000032"

# 해당 경로에서 .txt 파일의 목록을 재귀적으로 가져옵니다.
file_paths <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
file_paths
# 각 파일을 읽어들여 tibble에 저장합니다.
data_list <- lapply(file_paths, function(file_path) {
  data <- read_lines(file_path) %>% # 여기서는 각 파일을 텍스트 라인으로 읽어들입니다.
    paste(collapse = " ") %>% # 모든 줄을 하나의 문자열로 합칩니다.
    tibble(text = .) %>% # 결과 문자열을 tibble의 하나의 행으로 만듭니다.
    mutate(
      uid = str_extract(file_path, "s\\S*.txt"),
      file_path = file_path, # 파일 경로를 데이터에 추가합니다.
    ) %>% 
    mutate(
      uid = str_extract(uid, "(?<=s)\\d+")
    )
})
data_list
# class(data_list[[1]])
# data1_tibble <- text.df %>% select(uid, word)
data1 <- data_list[[1]]
class(data1)
data1$text
data1$uid

# 모든 tibble을 하나로 결합합니다.
final_data <- bind_rows(data_list)
final_data

# final_data의 text 칼럼에서 앞뒤 공백을 제거
final_data <- final_data %>% 
  mutate(text = str_trim(text))



text.df <- final_data %>%
  unnest_tokens(input = text,        # 분석 대상
                output = word,        # 출력 변수명
  )  # 토큰화 함수
text.df

text.tidy <- text.df %>% select(uid, word)
text.tidy


text.tidy.noun <- text.tidy %>%
  count(word, sort = T) %>%    # 단어 빈도 구해 내림차순 정렬
  filter(str_count(word) > 1)  # 두 글자 이상만 남기기
text.tidy.noun


top30 <- text.tidy.noun %>%
  head(30)
top30

# 막대 그래프 만들기
library(ggplot2)
ggplot(top30, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
