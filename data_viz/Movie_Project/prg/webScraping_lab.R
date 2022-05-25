
#웹클로링 크린징
install.packages("rvest") #Web Crawling/Web Scraping 패키지
install.packages("stringr") #문자열 처리하는 패키지
library(rvest)
library(stringr)
url.review="https://movie.naver.com/movie/point/af/list.nhn?page="
reviews.all = NULL
for(page in  21:40){
  url.review.page = paste(url.review, page, sep="")
  html = read_html(url.review.page, encoding = "utf-8")
  reviews = html_nodes(html, ".title") %>% html_text() #html node중에 title 중에 html_text를 가져옴
  reviews.all = c(reviews.all, reviews)
}

reviews.all
reviews.all[20] #영화제목/리뷰내용
#title을 누르면 해당하는 영화의 평가가 나오고 그래프가 뜨도록 - shiny

titles <- str_match(reviews.all, "\n\t\t\t\t(.*)\n\t\t\t") #str_match 문자열 추출, 소괄호는 (  ) 문자열 추출. 타이틀만 가져오기
titles <- titles[,2]
titles
table(titles)

reviews <- str_replace(reviews.all, titles, "") #str_replace  #리뷰에서 타이틀 제목 제거
reviews <- gsub("신고", "", reviews)
reviews <- gsub("\t|\n", "", reviews)
reviews

index<- str_locate(reviews, '총 10점')[,1]+7 #위치를 찾음
index
str<-substr(reviews, 1, index) #review중에 1~index까지 문자를 발췌
str #1점이랑 10점 두개 다 1로 나옴
reviews = str_replace(reviews, str, "") #지움
reviews = str_replace(reviews, "0", "") #10점 받은 것들은 0이 남아있기 때문에 지워줌
reviews
#ㄴ-> cleansing

save(reviews, file="reviews.rda") #save해두기
save(titles, file="titles.rda")
#save(predict, file="predict.rda")
####  predict 
predict = NULL
scores = NULL
m <- length(reviews)
for(i in 1:m){
words = str_split(reviews[i], '\\s+') #문자열 나누기
words = unlist(words) #리스트를 vector로 바꿈
words <- gsub("[^A-Za-z0-9ㄱ-힣]", "", words)
pos.matches = intersect(words, positive)
pos.cnt=length(pos.matches)
# words의 단어를 positive에서 matching
neg.matches = intersect(words, negative)
neg.cnt=length(neg.matches)

score = sum(pos.cnt) - sum(neg.cnt) 
scores = c(scores, score)
if(pos.cnt > neg.cnt){
  predict <- c(predict, 1)
   } else if(pos.cnt < neg.cnt){
     predict<-c(predict, -1)
      }  else {
          predict<-c(predict, 0)  
        }
}
#### polarity 평가 ###





#### 특정 영화에 대한 극성평가 ##




### 차트 #####
table(titles)





