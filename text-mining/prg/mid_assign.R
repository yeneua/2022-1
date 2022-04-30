# 2017447 김예나

# 데이터 불러오기
raw_park <- readLines("speech_park.txt", encoding="UTF-8")
head(raw_park)

# 전처리 - 한글이 아닌 것을 공백으로 대체
library(stringr)
park_string <- raw_park %>% str_replace_all("[^가-힣]", " ")
head(park_string)

# 전처리 - 공백 없애기
park_space <- str_squish(park_string)
head(park_space)

# 전처리 - tibble
library(dplyr)
park_tb <- as_tibble(park_space)
park_tb

# 토큰화
library(tidytext)
park_tk <- park_tb %>% unnest_tokens(input = value, output = word, token = "words")
park_tk

# 단어 빈도수
park_cnt <- park_tk %>% count(word, sort = T)
park_cnt

# 한 글자 이상인 문자만
park <- park_cnt %>% filter(str_count(word) > 1)
park

# 상위 20개 추출
top20_park <- park %>% head(20)
top20_park


# 막대그래프
library(ggplot2)
ggplot(top20_park, aes(x = reorder(word, n), y = n)) +
  geom_col()+
  coord_flip()+
  labs(title = "박근혜 전 대통령 출마 선언문 - 가장 자주 사용된 단어 20개", x = "단어", y = "빈도")

# 박근혜 전 대통령 워드 클라우드
library(ggwordcloud)
ggplot(park, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA),
               range = c(3, 30))


ggplot(park, aes(label = word, size= n, col = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA),
               range = c(3,30)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()
