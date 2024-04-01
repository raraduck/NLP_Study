# read.csv를 사용하여 변환 규칙 파일 불러오기
merged_tbl <- read.csv("week2/CXR_label_merged.csv", stringsAsFactors = FALSE)
merged_tbl <- as.tibble(merged_tbl)
class(merged_tbl)
merged_tbl

docs <- VCorpus(DataframeSource(merged_tbl))
docs


# 이미 생성된 VCorpus 객체(docs)에 대해
# 각 문서에 group_id, subject_id, study_id 메타데이터 추가
for (i in 1:length(docs)) {
  # 각 문서의 group_id, subject_id, study_id 가져와서 메타데이터에 설정
  meta(docs[[i]], tag = "doc_id") <- data_tbl$doc_id[i]
  meta(docs[[i]], tag = "group_id") <- data_tbl$group_id[i]
  meta(docs[[i]], tag = "subject_id") <- data_tbl$subject_id[i]
}
docs
target_docs = c(1,2,3)
lapply(docs, meta)[target_docs]
lapply(docs, content)[target_docs]



docs_tm <- docs
docs_tm <- tm_map(docs_tm, content_transformer(tolower))
docs_tm <- tm_map(docs_tm, stripWhitespace)
docs_tm <- tm_map(docs_tm, content_transformer(trimws))
mystopwords <- c(stopwords("english"), c("can", "cant", "don", "dont", "get", 
                                         "got", "just", "one", "will",
                                         "final", "report","examination", "indication",
                                         "findings", "impression"
                                         ))
docs_tm <- tm_map(docs_tm, removeWords, mystopwords)
toSpace <- function(x, pattern){
  return(gsub(pattern, " ", x))
}
docs_tm <- tm_map(docs_tm, content_transformer(toSpace), ":")
docs_tm <- tm_map(docs_tm, content_transformer(toSpace), ";")
docs_tm <- tm_map(docs_tm, content_transformer(toSpace), "//")
docs_tm <- tm_map(docs_tm, content_transformer(toSpace), "/")
docs_tm <- tm_map(docs_tm, content_transformer(toSpace), "_")
docs_tm <- tm_map(docs_tm, content_transformer(trimws))
docs_tm <- tm_map(docs_tm, removePunctuation)
docs_tm <- tm_map(docs_tm, removeNumbers)
docs_tm <- tm_map(docs_tm, stripWhitespace)
docs_tm <- tm_map(docs_tm, content_transformer(trimws))
docs_tm <- tm_map(docs_tm, stemDocument)
docs_tm
lapply(docs_tm, content)[target_docs]
lapply(docs_tm, meta)[3]


dtm <- DocumentTermMatrix(docs_tm)
dtm <- DocumentTermMatrix(
  docs_tm, 
  control=list(
    wordLengths=c(3,15), 
    bounds=list(
      global=c(10,1000)
      )
    )
  )
dtm
inspect(dtm)
term.freq <- colSums(as.matrix(dtm))
term.freq[head(order(term.freq, decreasing=TRUE))]
term.freq[tail(order(term.freq, decreasing=TRUE))]

findFreqTerms(dtm, lowfreq=200)
findAssocs(dtm, terms=c("pneumonia", "pulmonari"), corlimit=c(0.20, 0.25))


if (!require("wordcloud")) install.packages("wordcloud")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
library(wordcloud)
set.seed(123)
wordcloud(words=names(colSums(as.matrix(dtm))), freq=colSums(as.matrix(dtm)),
          scale=c(4,0.5), min.freq=30, max.words=200,
          rot.per=0, random.order=FALSE, random.color=FALSE,
          colors=brewer.pal(6,"Set2"))
