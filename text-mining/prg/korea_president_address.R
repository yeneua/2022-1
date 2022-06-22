#한국데이터연설문 분석해보기
moon <- readLines("data/speech_moon.txt", encoding="UTF-8")
park <- readLines("data/speech_park.txt", encoding = "UTF-8")
library(KoNLP)
library(tm)
library(dplyr)
library(tidytext)
library(tidyr)
library(tibble)


moon <- tibble(text = moon)
moon <- moon %>% add_column(source = "moon", .before = 1)
moon


moon$text <- gsub("\\d+", "", moon$text)
moon$text <- gsub("\\s+", "", moon$text)
 


moon_tidy <- moon %>%
  unnest_tokens(output = word, input = text, token = extractNoun)
moon_tidy <- moon_tidy %>%  
  filter(str_count(word) > 1)

tidy_moon <- tibble(moon_tidy)
tidy_moon
tail(tidy_moon)


moon$text <- moon %>% 
  unnest_tokens(output = word, input = text, token = extractNoun)

moon




moon <- as.vector(moon)
m_corpus <- VCorpus(VectorSource(moon))
m_corpus
m_corpus <- tm_map(m_corpus, removePunctuation)
m_corpus <- tm_map(m_corpus, removeNumbers)
m_corpus <- tm_map(m_corpus, stripWhitespace)
m_corpus <- tm_map(m_corpus, content_transformer(trimws))
lapply(m_corpus, content)
m_corpus <- unnest_tokens()