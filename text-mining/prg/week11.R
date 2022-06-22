# 2017447 김예나
# 11주차
library(dplyr)
library(ggplot2)
#ODDS Ratio
df_long <- frequency %>% 
  group_by(presidents) %>% 
  slice_max(n, n=10) %>% 
  filter(word %in% c("국민","우리","정치","행복"))
df_long

#pivoting
install.packages("tidyr")
library(tidyr)
df_wide <- df_long %>% 
  pivot_wider(names_from = president,
              values_from = n)
df_wide

#NA를 0으로
df_wide <- df_long %>% 
  pivot_wider(name_from = presidents,
              values_from = n,
              values_fill = list(n = 0))
df_wide

frequency_wide <- frequency %>% 
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
frequency_wide

#Odds ratio 계산
frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon)/(sum(moon))), #moon에서 단어의 비중
         ratio_park = ((park)/(sum(park)))) #park에서 단어의 비중
frequency_wide

#단어 비중 비교를 위해서 각 행에 1을 더함. 0을 없애기 위해
frequency_wide <- frequency_wide %>% 
  mutate(ratio_moon = ((moon + 1)/(sum(moon + 1))),
         ratio_park = ((park + 1)/(sum(park + 1))))
frequency_wide

frequency_wide <- frequency_wide %>% 
  mutate(odds_ratio = ratio_moon/ratio_park)
frequency_wide


# "moon"에서 상대적인 비중이 클수록 1보다 큰 값
# "park"에서 상대적인 비중 클수록 1보다 작은 값

frequency_wide %>% 
  arrange(-odds_ratio) #내림차순

frequency_wide %>% 
  arrange(odds_ratio)

#상대적으로 중요한 단어 추출하기
top10 <- frequency_wide %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)
top10

top10 <- top10 %>% 
  mutate(president = ifelse(odds_ratio > 1, "moon","park"),
         n = ifelse(odds_ratio > 1, moon, park))
top10

top10 <- top10 %>% 
  group_by(president) %>% 
  slice_max(n, n = 10, with_ties = F)
top10

ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered()


#그래프 별로 축 별도 설정
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = ,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL) +# x축 삭제
  theme(text = element_text(family = "nanumgothic")) #폰트 지정

#로그 오즈비
frequency_wide <- frequency_wide %>% 
  mutate(log_odds_ratio = log(odds_ratio))
frequency_wide

frequency_wide %>% 
  arrange(-log_odds_ratio)

frequency_wide %>% 
  arrange(log_odds_ratio)

top10 <- frequency_wide %>% 
  group_by(president = ifelse(log_odds_ratio > 0, "moon","park")) %>% 
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10

top10 %>% 
  arrange(-log_odds_ratio) %>%
  selecct(word, log_odds_ratio, president)


#서로 다른 방향으로 막대 그래프 그리기
ggplot(top10, aes(x=reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
  geom_col() +
  coord_flilp() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))




# 11-2
#텍스트 벡터 생성
text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")

library(tm)

#텍스트 전처리

corpus.docs <- VCorpus(VectorSource(text))
lapply(corpus.docs, meta)
lapply(corpus.docs, content)
corpus.docs <- tm_map(corpus.docs, content_transformer(tolower)) #소문자 변환
corpus.docs <- tm_map(corpus.docs, removeWords, stopwords('english')) #불용어 제거
myRemove <- content_transformer(function(x, pattern) #특정 패턴 가지는 문자열 제거 함수 생성
  {return(gsub(pattern, "",x))})
corpus.docs <- tm_map(corpus.docs, myRemove, "(f|ht)tp\\S+\\s*") #url삭제 -함수이용
corpus.docs <- tm_map(corpus.docs, removePunctuation) #문장부호 삭제
lapply(corpus.docs, content)
corpus.docs[[1]]$content

corpus.docs <- tm_map(corpus.docs, removeNumbers) #숫자삭제
corpus.docs <- tm_map(corpus.docs, stripWhitespace) #여백삭제
corpus.docs <- tm_map(corpus.docs, content_transformer(trimws)) #텍스트 앞뒤 공백 삭제
corpus.docs <- tm_map(corpus.docs, stemDocument) #어간추출
corpus.docs <- tm_map(corpus.docs, content_transformer(gsub),
                      pattern = 'economist', replacement='economi') #동의어 처리
lapply(corpus.docs, content)



?DocumentTermMatrix

DocumentTermMatrix(corpus.docs,
                   control = list(wordLengths = c(2,Inf)))
corpus.dtm <- DocumentTermMatrix(corpus.docs,
                                 control = list(wordLengths = c(2,Inf)))
corpus.dtm
nTerms(corpus.dtm) #=>19
nDocs(corpus.dtm) #=>3
Terms(corpus.dtm)

Docs(corpus.dtm)
row.names(corpus.dtm) <- c("BBC","CNN","FOX")

inspect(corpus.dtm)
inspect(corpus.dtm[1:3, 10:19])


#tidy text형식의 데이터 셋
text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetarian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
source <- c("BBC","CNN","FOX")

library(dplyr)
library(tidytext)
library(SnowballC)

text.df <- tibble(source = source, text = text)
text.df

text.df$text <- gsub("(f|ht)tp\\S+\\s*", "", text.df$text) #url 삭제
text.df$text <- gsub("\\d+", "", text.df$text) #숫자 삭제
tidy.docs <- text.df %>% 
  unnest_tokens(output = word, input = text) %>% #토큰화
  anti_join(stop_words, by = "word") %>%  #불용어 사전을 활용한 불용어 제거
  mutate(word = wordStem(word)) #어간 추출
tidy.docs$word <- gsub("\\s+" ,"",tidy.docs$word) #공백제거
tidy.docs$word <- gsub("economist","economi",tidy.docs$word) #동의어 처리
tidy.docs$word
tidy.docs

tidy.dtm <- tidy.docs %>% count(source, word) %>% 
  cast_dtm(document = source, term = word, value = n)
tidy.dtm

Terms(tidy.dtm)
Docs(tidy.dtm)
inspect(tidy.dtm[1:2,3:5])
