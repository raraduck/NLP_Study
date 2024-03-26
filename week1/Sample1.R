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






# 먼저, 각 부분을 처리하는 함수를 정의합니다.
extract_data <- function(part) {
  # 어린이집 이름 추출
  # "비교선택" 다음 줄에 있는 텍스트를 추출하기 위해 수정된 정규 표현식 사용
  kindergarten_name <- str_match(part, "비교선택\\s*\\n([^\\n]+)")[,2]
  
  # 현원 정보 추출
  current_count_match <- str_match(part, "정현원 :\\d+/(\\d+)")
  if (length(current_count_match) > 1) {
    current_count <- as.numeric(current_count_match[1,2])
  } else {
    current_count <- NA
  }
  
  # 결과 반환
  return(data.frame(Kindergarten = kindergarten_name, CurrentCount = current_count, stringsAsFactors = FALSE))
}


# 각 부분에 대해 함수 적용
results_list <- lapply(text_parts, extract_data)

# 결과 리스트를 하나의 데이터프레임으로 결합
final_data_frame <- do.call(rbind, results_list)

# 중복 제거 및 NA 제거
final_data_frame <- unique(na.omit(final_data_frame))
final_data_frame

# CSV 파일로 저장
write.csv(final_data_frame, "week1/data/kindergarten_current_counts.csv", row.names = FALSE)
