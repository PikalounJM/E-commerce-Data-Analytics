library(dplyr)
library(stringr)
library(tidyverse)
library(readxl)
library(plyr)

setwd("C:/Users/user/Desktop/로컬라이브랩/06.데이터분석")

review <- read_xlsx("C:/Users/user/Desktop/로컬라이브랩/06.데이터분석/category_review.xlsx")
View(review)
review1 <- review$"내용"
length(review1)
summary(review1)

#감성사전에서 가져온 positive와 negative를 변수에 불러오기
positive <- readLines("positive.txt", encoding="UTF-8")
positive = positive[-1]

negative <- readLines("negative.txt", encoding = "UTF-8")
negative = negative[-1]

#긍정, 부정어 비교 함수
sentimental = function(sentences, positive, negative){
  
  scores = laply(sentences, function(sentence, positive, negative) {
    
    sentence = gsub('[[:punct:]]', '', sentence) # 문장부호 제거
    sentence = gsub('[[:cntrl:]]', '', sentence) # 특수문자 제거
    sentence = gsub('\\d+', '', sentence)        # 숫자 제거
    
    word.list = str_split(sentence, '\\s+')      # 공백 기준으로 단어 생성 -> \\s+ : 공백 정규식, +(1개 이상)
    words = unlist(word.list)                    # unlist() : list를 vector 객체로 구조변경
    
    pos.matches = match(words, positive)           # words의 단어를 positive에서 matching
    neg.matches = match(words, negative)
    
    pos.matches = !is.na(pos.matches)            # NA 제거, 위치(숫자)만 추출
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)  # 긍정 - 부정   
    return(score)
  }, positive, negative)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#분석 결과 mapping
result=sentimental(review1, positive, negative)

result$color[result$score >=1] = "blue"
result$color[result$score ==0] = "green"
result$color[result$score < 0] = "red"

result$score
table(result$color)

result$remark[result$score >=1] = "긍정"
result$remark[result$score ==0] = "중립"
result$remark[result$score <0] = "부정"

sentiment_result = table(result$remark)
sentiment_result

labels <- sprintf("%s: %1.1f%%(%d)", names(sentiment_result), sentiment_result / sum(sentiment_result) * 100, sentiment_result)

pie(sentiment_result, main = "로컬라이브 review 감성분석 결과",
    col=c("blue","red","green"), radius = 0.8, labels=labels)

#상품 카테고리별 리뷰 수 정렬

library(ggplot2)
A <- table(review$"상품 카테고리")
A <- as.data.frame(A)
names(A) <- c('category','빈도')

ggplot(A, aes(reorder(category, -빈도), 빈도, fill=category))+geom_bar(stat="identity") + xlab("상품 카테고리") + ylab("리뷰 수") + ggtitle("상품 카테고리별 리뷰 수")


#상품 카테고리별 리뷰 감성 분석

category_list <- list()
unique_category <- unique(review$`상품 카테고리`)

for (category_value in unique_category) {
  category_review <- review$내용[review$`상품 카테고리` == category_value]
  category_list[[category_value]] <- category_review
}

for (category_value in unique_category) {
  cat(paste("Category:", category_value, "\n"))
  cat(category_list[[category_value]], "\n\n")
}

category_dataframes <- lapply(category_list, function(reviews) {
  data.frame(Review = reviews)
})

sentiment_results <- lapply(category_dataframes, function(df) {
  scores <- lapply(df$Review, function(sentence) {
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    
    pos.matches <- match(words, positive)
    neg.matches <- match(words, negative)
    
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  })
  
  scores.df <- data.frame(score = unlist(scores), text = df$Review)
  return(scores.df)
})

#카테고리별 score 점수 결과 확인
plots <- list()

for (i in 1:length(unique_category)) {
  cat(paste("Category:", unique_category[i], "\n"))
  scores <- sentiment_results[[i]]$score
  colors <- ifelse(scores >= 1, "긍정", ifelse(scores == 0, "중립", "부정"))
  cat(scores, "\n")
  table_result <- table(colors)
  table_result["부정"][is.na(table_result["부정"])] <- 0
  cat("긍정:", table_result["긍정"], " / 중립:", table_result["중립"], " / 부정:", table_result["부정"], "\n\n")
  
  sentiment_data <- data.frame(Sentiment=c("긍정","중립","부정"), Count=c(table_result["긍정"], table_result["중립"],table_result["부정"]))
  
  p <- ggplot(sentiment_data, aes(x="", y = Count, fill=Sentiment)) +
    geom_bar(stat="identity", width=1) +
    geom_text(aes(label=ifelse(Count>0, paste(Sentiment, ":", Count),"")), position=position_stack(vjust=0.5), size=6, fontface="bold") +
    coord_polar("y") +
    labs(fill="Sentiment") +
    ggtitle(paste("상품 카테고리별 감성분석 결과:", unique_category[i]))
  
  plots[[i]] <- p #그래프를 리스트에 저장
  cat("\n")
}

#그래프를 배열 형태로 표시
install.packages("gridExtra")
library(gridExtra)
grid.arrange(grobs = plots, ncol=2)

combined_plots <- plots[[1]] + plots[[2]] + plots[[3]] + plots[[4]] +
  plots[[5]] + plots[[6]] + plots[[7]] + plots[[8]] +
  plots[[9]] + plotsp[[10]] + plots[[11]] + plots[[12]]

combined_plots
