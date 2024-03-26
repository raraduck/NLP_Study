# 필요한 패키지 불러오기
library(stringr)
library(dplyr)

# 텍스트 파일 읽기
text_lines <- readLines("week1/data/kindergarten.txt", encoding="UTF-8")
text_lines
text_data <- paste(text_lines, collapse = "\n")
text_data
# 전화번호 패턴을 기준으로 전체 텍스트를 나누기
text_parts_ <- str_split(text_data, "\\d{2,3}-\\d{3,4}-\\d{4}", simplify = FALSE)[[1]]
text_parts_
# 전화번호도 추출하여 함께 저장
phone_numbers <- str_extract_all(text_data, phone_number_pattern)[[1]]
phone_numbers
# text_parts에서 첫 번째 빈 요소 제거
text_parts <- text_parts_[-1]
length(text_parts)
length(phone_numbers)
# 분리된 텍스트와 전화번호를 결합하여 데이터 프레임 생성
data_frame <- data.frame(
  Phone = phone_numbers,
  Information = unlist(text_parts),
  stringsAsFactors = FALSE
)
head(data_frame)
df_tibble <- as_tibble(data_frame)
head(df_tibble)

# data_frame$Name <- str_extract(data_frame$Information, "(?<=비교선택\\s\\n)[^\\n]+평가참여")
# 어린이집 이름과 정현원 정보 추출 및 새로운 열로 추가
data <- df_tibble %>%
  mutate(
    # Kindergarten = str_extract(Information, "(?<=비교선택\\s*\\n+)[^\\n]+"),
    # Kindergarten = str_extract(Information, "비교선택\\s*\\n*\\s*([^\\n]+)"),
    # Kindergarten = str_extract(Information, "(비교선택\\s*\\n*)+(\\S+어린이집)"),
    Kindergarten = str_extract(Information, "\\S*(어린이집|유치원|초등학교)"),
    CountInfo = str_extract(Information, "정현원 :\\d+/(\\d+)")
  ) %>%
  mutate(
    # Kindergarten = str_extract(Kindergarten, "\\S+(어린이집|유치원|초등학교)"),
    CurrentCount = str_extract(CountInfo, "(?<=/)\\d+")
  )
data

# CSV 파일로 저장
write.csv(data, "week1/data/kindergarten_answer.csv", row.names = FALSE)
