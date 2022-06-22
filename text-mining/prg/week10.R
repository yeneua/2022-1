# 2017447 김예나
# 10-1


text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg","A Vegetar$ian diet exludes all animal flesh (meat, poultry, seafood).","Economists surveyed by Refinitiv expect the economy added 160,000 jobs.")
source <- c("BBC","FOX","CNN")

library(dplyr) #tibble()
text.df <- tibble(source = source, text=text)
text.df
class(text.df)


#tokenization
library(tidytext)
unnest_tokens(tbl = text.df, output = word, input = text)

head(iris)
iris %>% head(10)

tidy.docs <- text.df %>% 
  unnest_tokens(output = word, input = text)

print(tidy.docs, n = Inf)

tidy.docs %>% count(source) %>% arrange(desc(n)) #counting의 결과

#불필요한 단어 제거
?anti_join()

stop_words

anti_join(tidy.docs, stop_words, by="word")

tidy.docs <- tidy.docs %>% anti_join(stop_words, by="word")
tidy.docs

word.removed <- tibble(word=c("http","bbc.in","1g0j4agg"))

tidy.docs <- tidy.docs %>% anti_join(word.removed,by="word")

tidy.docs$source
tidy.docs$word

grep("\\d+", tidy.docs$word)

tidy.docs <- tidy.docs[-grep("\\d+",tidy.docs$word),]
tidy.docs

text.df

text.df$text <- gsub("(f|ht)tp\\S+\\s*","",text.df$text)
text.df$text

text.df$text <- gsub("\\d+","",text.df$text)
text.df$text


tidy.docs <- text.df %>% 
  unnest_tokens(output = word, input=text)
tidy.docs

tidy.docs <- tidy.docs %>% anti_join(stop_words,by="word")
tidy.docs

tidy.docs$word <- gsub("ian","",tidy.docs$word)
tidy.docs$word

tidy.docs$word <- gsub("economists","economy",tidy.docs$word) #economist->economy
tidy.docs$word


#corpus -> tidy : 연속적으로 작업
library(tm)
corpus.docs <- VCorpus(VectorSource(text))

meta(corpus.docs, tag="author",type="local") <- source

lapply(corpus.docs, meta)

tidy(corpus.docs)

tidy(corpus.docs) %>% unnest_tokens(word, text)

tidy(corpus.docs) %>% unnest_tokens(word, text) %>% select(source=author,word)



# 10-2
#문재인 박근혜 연설문 비교

library(dplyr)

#문재인 대통령 연설문 불러오기
raw_moon <- readLines("data/speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>% 
  as_tibble() %>% 
  mutate(president = "moon")
moon

#박근혜 대통령 연설문 불러오기
raw_park <- readLines("data/speech_park.txt", encoding = "UTF-8")
park <- raw_park %>% 
  as_tibble() %>% 
  mutate(president = "park")
park

#두 데이터 합치기
bind_speeches <- bind_rows(moon, park) %>% #cbind()
  select(president, value)

bind_speeches %>% count(president)

head(bind_speeches)
tail(bind_speeches)


#기본적인 전처리
library(stringr)
speeches <- bind_speeches %>% 
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
speeches


#토큰화
library(tidytext)
library(KoNLP) #별도의 추가 설치
speeches <- speeches %>% 
  unnest_tokens(input = value,
                output = word,
                token = extractNoun) # <- 에러
speeches

frequency <- speeches %>% 
  count(president, word) %>%  #연설문 및 단어별 빈도
  filter(str_count(word) > 1) #한 글자가 넘는( 두글자 이상 추출)
head(frequency)
tail(frequency)

#dplyr::slice_max() : 값이 큰 상위 n개의 행을 추출해 내림차순 정렬
top10 <- frequency %>% 
  group_by(president) %>%  #president별로 분리
  arrange(desc(n)) %>% #상위 10개추출
  head(10) %>% filter(president == "park")
top10

top10 <- frequency %>% 
  group_by(president) %>%  #president별로 분리
  slice_max(n,n=10)
top10

top10 <- frequency %>% 
  group_by(president) %>% 
  slice_max(n,n=10,with_ties = F)
top10

library(ggplot2)
ggplot(top10,aes(x=reorder(word,n),
                 y=n,
                 fill=president)) +
  geom_col()+
  coord_filp()+
  facet_wrap(~president)


#y축을 통일하지 않고 각각 19개씩
ggplot(top10, aes(x=reorder(word,n),
                  y=n,
                  fill=president)) +
  geom_col()+
  coord_flip()+
  facet_wrap(~president,scales="free_y")


#축 재정리
ggplot(top10, aes(x=reorder_within(word,n,president),
                  y=n,
                  fill=president))+
  geom_col()+
  coord_flip()+
  facet_wrap(~president,sclaes="free_y")

#tidytext::scale-x_reordered():각 단어 뒤의 범주 항목 제거
ggplot(top10, aes(x=reorder_within(word,n,president),
                  y=n,
                  fill=president))+
  geom_col()+
  coord_flip()+
  facet_wrap(~president,sclaes="free_y")+
  scale_x_reordered()+
  labs(x=NULL)+ #x축삭제
  theme(text=element_text(family = "nanumgothic")) #폰트


install.packages(c("hash", "tau", "Sejong", "RSQLite", "devtools", "bit", "rex", "lazyeval", "htmlwidgets", "crosstalk", "promises", "later", "sessioninfo", "xopen", "bit64", "blob", "DBI", "memoise", "plogr", "covr", "DT", "rcmdcheck", "rversions"), type = "binary")
