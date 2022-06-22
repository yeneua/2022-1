# 2017447 김예나
file <- choose.files()
bts <- read.csv(file, header=T)
str(bts)
dim(bts)

# 문제2
raw_news_comment <- bts
library(dplyr)
library(stringr)
library(textclean)
news_comment <- raw_news_comment %>%
  mutate(id = row_number(),
         reply = str_squish(replace_html(reply)))
news_comment %>%
  select(id, reply)
dim(news_comment)
library(tidytext)
token_bts <- unnest_tokens(news_comment, output= word, input =reply)
dim(token_bts)


# 문제3
file1 <- choose.files()
dic <- read.csv(file1, header=T)

dic_bts <- token_bts %>% 
  left_join(dic, by="word") %>% 
  mutate(polarity = ifelse(is.na(polarity), 0, polarity)) %>% 
  arrange(-polarity)

head(dic_bts)


a <- dic_bts %>% count(word, polarity, sort=TRUE) %>%group_by(polarity)
a <- subset(a,a$n==1)
a <- grep("\\D+", a, value = TRUE)
head(a)

# 문제4
dic_bts %>% group_by(polarity) %>% count(polarity)
dic %>% group_by(polarity)
%>% count(polarity)


dic_bts <- groupb_by(polarity_)
subset(dic_bts)


# 문제5
comment <- dic_bts %>%
  unnest_tokens(input = word,
                output = word,
                token = "words",
                drop = F)

frequency_word <- comment %>%
  count(sentiment, word, sort = T)


comments <- dic_bts %>% select(word,polarity)
comments
comments <- tibble(comments)
comments

f <- comments %>% 
  mutate(odds=(()))

pivot <- comments %>% pivot_wider(names_from=)


frequency <- comments %>%  
  count(word)
frequency <- gsub("\\D+", "",frequency)
frequency
