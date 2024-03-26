library(multilinguer)
library(KoNLP)
library(dplyr)
library(stringr)

# 가정: `text_data`는 위의 텍스트 데이터를 포함하는 문자열 변수
# 텍스트 파일에서 데이터 읽기
text_data <- readLines("week1/data/kindergarten.txt", warn = FALSE, encoding="UTF-8")

# 전체 파일 내용을 하나의 문자열로 결합
text_data <- paste(text_data, collapse = "\n")
text_data
# 전화번호 패턴을 사용하여 텍스트를 분리
# 한국 전화번호 형식에 맞는 정규 표현식을 사용
phone_number_pattern <- "\\d{2,3}-\\d{3,4}-\\d{4}"

# 전화번호를 포함하여 텍스트 분리
# `str_split` 대신 R 기본 함수 `strsplit` 사용
# `perl=TRUE`를 사용하여 Perl 호환 정규 표현식 사용 가능
split_text <- unlist(strsplit(text_data, phone_number_pattern, perl=TRUE))
split_text <- split_text[-1]
# split_text <- strsplit(text_data, phone_number_pattern, simplify = FALSE)
split_text
# 전화번호도 추출하여 함께 저장
phone_numbers <- str_extract_all(text_data, phone_number_pattern)[[1]]
phone_numbers
length(split_text)
length(phone_numbers)
# 분리된 텍스트와 전화번호를 결합하여 데이터 프레임 생성
data_frame <- data.frame(
  Information = unlist(split_text),
  Phone = phone_numbers,
  stringsAsFactors = FALSE
)
data_frame
# 데이터 프레임에서 추가 정제 작업 수행
# 예: `유형` 정보 추출
data_frame$Type <- str_extract(data_frame$Information, "정현원 :[0-9]+/[0-9]+")

# / 뒤의 숫자를 추출하여 새로운 열로 추가
data_frame <- data_frame %>%
  mutate("Count" = as.integer(str_extract(Type, "(?<=/)\\d+")))

data_frame$Name <- str_extract(data_frame$Information, "(?<=비교선택\\s\\n)[^\\n]+평가참여")
data_frame
data_frame$Name <- sapply(data_frame$Name, function(name) {
  if (!is.na(name)) {
    gsub("(\\b\\w+\\b)(?:\\s+\\1)+", "\\1", name)
  } else {
    NA
  }
})

data_frame
selected_data <- select(data_frame, Count, Name)
selected_data

# Name 열에서 "~~어린이집" 형식의 문자열 추출하여 새로운 열에 추가
selected_data <- selected_data %>%
  mutate(Kindergarten = str_extract(Name, "\\S+어린이집"))

filtered_data <- select(selected_data, Kindergarten, Count)
filtered_data

# CSV 파일로 저장
write.csv(filtered_data, "week1/data/kindergarten_data.csv", row.names = FALSE)
