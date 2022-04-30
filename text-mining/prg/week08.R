## 8주차

raw_moon <- readLines("speech_moon.txt", encoding="UTF-8") #한글이라서 encoding 속성 부여
head(raw_moon)
class(raw_moon) # => "character", 문자열 벡터

txt <- "치킨은!! 맛있다. xyz 정말 맛있다 !@#"
txt
library(stringr)
txt <- str_replace_all(txt, "[^가-힣]", replacement=" ") #정규표현식 - 한글 가~힣(모든 한글), ^ : not의 의미 => 한글이 아닌 것을 공백으로
#변수로 할당해야지 데이터 바뀐다

moon <- raw_moon %>% str_replace_all("[^가-힣]", " ") #한글이 아닌 것을 공백으로 대체 / 파이프 연산자 - raw_moon 데이터 에서(순차적) str_replace_all 함수를 실행시켜라
head(moon)

moon <- str_trim(moon) #앞뒤공백 사라짐
head(moon)

moon <- str_squish(moon) #여러개 공백을 하나의 공백으로 통일 
head(moon)

library(dplyr) #tibble()함수 사용하기위해
class(moon) #character
moon <- as_tibble(moon)
moon
class(moon)

# 토큰화하기
# tidytext, unnest_token() 함수 활용

install.packages("tidytext")
library(tidytext)

text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
text #두개의 문장이, 하나의 tibble요소로 들어가있다.

#토큰화 ->  문건에서 어떤 기준으로 쪼개는 것. 기준? - 문장, 단어, 하나의 문자, 등등 ...

text %>% unnest_tokens(input = value, #value를 이용(tibble 데이터 기준)
                       output = word, #value를 word로
                       token = "sentences") #기준 :sentences
text %>% unnest_tokens(input = value,
                       output = word,
                       token = "words") #공백을 기준으로 끊김(띄워쓰기 기준)
text %>% unnest_tokens(input = value,
                       output = word,
                       token = "characters") #한 글자 한 글자

moon_space <- moon %>% unnest_tokens(input = value, output = word, token = "words")
moon_space #moon 데이터를 단어 중심으로 토큰화

#단어 빈도 분석하기
moon_space <- moon_space %>% count(word)
moon_space
moon_space <- moon_space %>% count(word, sort = T)
moon_space

str_count("배")
str_count("배배배")

moon_space <- moon_space %>%
                filter(str_count(word) > 1) #한글자짜리는 빼고(한글에는 한 글자만 가지고 의미를 가지는 경우가 잘 없다 )
moon_space

top20_moon <- moon_space %>% head(20) #상위20개만. 형태소구분X
top20_moon

top10_moon <- moon_space %>% head(10)
top10_moon

# ggplot2
library(ggplot2)

ggplot(top20_moon, aes(x = reorder(word, n), y = n)) + #단어 빈도순 정렬
  geom_col()+
  coord_flip() #x,y축 뒤집어서

#워드클라우드
install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(moon_space, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA),
               range = c(3, 30))


ggplot(moon_space, aes(label = word, size= n, col = n)) +
  geom_text_wordcloud(seed = 1234) +
  scale_radius(limits = c(3, NA),
               range = c(3,30)) +
  scale_color_gradient(low = "#66aaf2", high = "#004EA1") +
  theme_minimal()
