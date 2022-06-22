install.packages('textdata')

library(tidytext)
library(textdata)

## 감성사전
get_sentiments(lexicon = 'bing')
unique(get_sentiments(lexicon = 'bing')$sentiment)

get_sentiments(lexicon = 'afinn')
unique(get_sentiments('afinn')$value)
summary((get_sentiments('afinn')$value))

get_sentiments(lexicon = 'nrc')
unique(get_sentiments(lexicon = 'nrc')$sentiment)

get_sentiments(lexicon = 'loughran')
unique(get_sentiments(lexicon = 'loughran')$sentiment)


install.packages("purrr")
install.packages("lubridate")
install.packages("reshape2")
install.packages("readr")
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate) 

url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip"
local.copy <- tempfile() # 임시 파일로 사용
download.file(url, local.copy, mode = "wb")
Sys.setlocale("LC_TIME", "English") # 시스템 시간 미국으로 바꾸기
Sys.setlocale()

# map() : 순차적으로 수행하는 함수
map(unzip(zipfile = local.copy,
          files = c("Health-Tweets/bbchealth.txt", 
                    "Health-Tweets/cnnhealth.txt", 
                    "Health-Tweets/foxnewshealth.txt", 
                    "Health-Tweets/NBChealth.txt")), 
    read_delim, delim = "|", quote ="", # read_delim() : 텍스트를 읽어홈. 리스트 형태로 반환. 구분자 지정
    col_types = list(col_character(), col_character(), col_character()), 
    col_names = c("id","datetime","tweet"))


# 4개의 리스트 요소를 하나로 통합
health.twitter <- 
  map(unzip(zipfile = local.copy,
            files = c("Health-Tweets/bbchealth.txt",
                      "Health-Tweets/cnnhealth.txt",
                      "Health-Tweets/foxnewshealth.txt",
                      "Health-Tweets/NBChealth.txt")),
            read_delim, delim = "|", quote = "", 
            col_types = list(col_character(), col_character(),  col_character()),
            col_names = c("id","datetime","tweet")) %>% 
  map2(c("bbc", "cnn", "foxnews", "nbc"), #map2 함수를 활용하여 이름 붙이기
       ~cbind(.x, source=.y)) %>% 
  reduce(bind_rows) %>% 
  as_tibble()  %>% 
  mutate(datetime=ymd_hms(strptime(datetime,
                                  "%a %b %d %H:%M:%S +0000 %Y")))
unlink(local.copy)
Sys.setlocale()

# 다운로드 되지 않는 경우 csv 파일 불러오기
health.twitter <-   read.csv("data/health.twitter.csv") %>% as.tibble()
health.twitter


# 신문사 종류 확인하기   
health.twitter %>% count(source)

### tidytext형식으로 단어 추출(긍정/부정의 출현 빈도 확인)

## 전처리(반복적인 수행이 필요함)
library(stringr)
health.words <- health.twitter %>% 
  select(-id, -X) %>%  # 필요없는 열 삭제
  mutate(tweet = str_replace_all(tweet, pattern = "(f|ht)tp\\S+s*", replacement = "")) %>% # url지우기
  mutate(tweet = str_replace_all(tweet, pattern = "\\d+", replacement = "")) %>%  # 숫자 지우기
  mutate(tweet = str_replace_all(tweet, pattern = "\\bRT", replacement = "")) %>% # 리트윗삭제
  mutate(tweet = str_replace_all(tweet, pattern = "@\\S+", replacement = "")) %>% # 멘션삭제
  mutate(tweet = str_replace_all(tweet, pattern = "&amp", replacement = "")) %>% 
  unnest_tokens(word, tweet) # 토큰화 - tidytext형태로 만들기
health.words

## 감성어휘사전과 결합
health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") # 감성어휘 사전에 없는 단어는 사라짐 . bing에 있는 단어들만 가져옴(inner_join)

# 단어 빈도 확인 - 긍정/부정 합쳐서
health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) # 단어, 감성 기준으로 count

# 빈도 수 상위 긍정/부정 단어 각각 10개씩 확인
health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% #감성 별로 나누고
  top_n(10, n) %>% # 상위10개
  ungroup()



## 시각화
library(ggplot2)
library(scales)


# 긍정과 부정 빈도를 구분하여 시각화하기 위해 nsign 열 생성
health.sentiment<- health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(nsign = ifelse(sentiment == "negative", -n, n)) # sentiment가 negative이면 출현빈도를 -n으로 표시
health.sentiment


# 막대그래프
ggplot(health.sentiment,
       aes(x=reorder(word, nsign), y=nsign, #nsign을 중심으로 그림
           fill = factor(sentiment,
                         levels = c("positive", "negative")))) + 
  geom_col(color = "lightslategray", width = 0.8)+
  geom_text(aes(label=n), size= 3, color="black", #bar에숫자
            hjust=ifelse(health.sentiment$nsign < 0, 1.1, -0.1))+ # 0보다 작으면, 왼쪽 막대의 끝에. 0보다크면, 오른쪽막대 끝에
  scale_fill_manual(values = c("cornflowerblue", "tomato")) +
  scale_y_continuous(breaks = pretty(health.sentiment$nsign),
                     labels = abs(pretty(health.sentiment$nsign))) +
  labs(x=NULL, y = 'count') + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  coord_flip()

# 고빈도 단어 제거하기(의학용어와 관련된 단어이기 때문에  
health.sentiment<- health.words %>% 
  inner_join(get_sentiments("bing"), by = "word") %>% 
  filter(!(word == "patient"|word == "cancer"|word == "virus")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  mutate(nsign = ifelse(sentiment == "negative", -n, n))

health.sentiment

# 막대그래프(단어의 출현빈도를 활용)
ggplot(health.sentiment,
       aes(x=reorder(word, n), y=n,
           fill = factor(sentiment,
                         levels = c("positive", "negative")))) + 
  geom_col(color = "lightslategray", width = 0.8, show.legend = FALSE)+
  geom_text(aes(label=n), size= 3, color="black",
            hjust= 1.2)+
  scale_fill_manual(values = c("lightsteelblue", "lightsalmon")) +
  facet_wrap(~factor(sentiment,
                     levels = c("positive", "negative")),
             ncol = 2, scales = 'free') +
  labs(x=NULL, y = 'Count') + 
  coord_flip()

# wordcloud(긍정/부정 나타내기)
library(wordcloud)
library(reshape2)

set.seed(123)
health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word == "patient"|word == "cancer"|word == "virus")) %>%
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("tomato","cornflowerblue"),
                   title.size = 2,
                   title.colors = c("red","blue"),
                   title.bg.colors = "wheat",
                   scale = c(4, 0.3), max.words = 200) # 범주를 나타내는 단어 크기

# ?ð????? ???? ???? ?????? ?? ?????????? ???? ????

# ???? ?? ???? ???????? ???? ????
health.sentiment <- health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word == "patient"|word == "cancer"|word == "virus")) %>%
  count(word, sentiment, source, ,sort = TRUE) %>%
  group_by(source, sentiment) %>% 
  top_n(10, n) %>% 
  ungroup()

health.sentiment


# ???????? ?????????? ???? ??????????
ggplot(health.sentiment,
       aes(reorder_within(x=word, by =n, within = source),
           y=n, fill=source))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~ factor(source,
                      labels = c("BBC", "CNN", "Fox News", "NBC"))+ sentiment,
             ncol=2, scale = "free")+
  scale_x_reordered()+
  labs(x=NULL, y="Count") +
  coord_flip()



# wordcloud(????????/???????? ???? ????????)
library(wordcloud)
library(reshape2)

set.seed(123)
health.words %>% 
  inner_join(get_sentiments("bing"), by="word") %>% 
  filter(!(word == "patient"|word == "cancer"|word == "virus")) %>%
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("tomato","cornflowerblue"),
                   title.size = 2,
                   title.colors = c("red","blue"),
                   title.bg.colors = "wheat",
                   scale = c(4, 0.3), max.words = 200) #?????? ???????? ???? ???? 

