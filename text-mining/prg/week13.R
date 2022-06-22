# 2017447 김예나
# 13-1

install.packages("quanteda")
library(quanteda)
data_corpus_inaugural

summary(data_corpus_inaugural)
class(data_corpus_inaugural)

library(tidytext)
library(tibble)
library(dplyr)

us.president.address <- tidy(data_corpus_inaugural) %>% 
  filter(Year > 1990) %>% 
  group_by(President, FirstName) %>% 
  summarise_all(list(~trimws(paste(., collapse = " ")))) %>% 
  arrange(Year) %>% 
  ungroup()
us.president.address

library(tm)
?DataframeSource()

us.president.address <- us.president.address %>% 
  select(text, everything()) %>% 
  add_column(doc_id = 1:nrow(.), .before = 1)
us.president.address

address.corpus <- VCorpus(DataframeSource(us.president.address))
lapply(address.corpus[1], content)


#전처리
address.corpus <- tm_map(address.corpus,content_transformer(tolower))
address.corpus[[1]]$content

sort(stopwords("english"))

Mystopwords <- c(stopwords("english"),c("must","will","can", "bless", "america"))

address.corpus <- tm_map(address.corpus, removeWords, Mystopwords)
address.corpus[[1]]$content


address.corpus <- tm_map(address.corpus,removePunctuation)
lapply(address.corpus[1],content)

address.corpus <- tm_map(address.corpus, removeNumbers)
lapply(address.corpus[1],content)

address.corpus <- tm_map(address.corpus, stripWhitespace)
lapply(address.corpus[1],content)

address.corpus <- tm_map(address.corpus, content_transformer(trimws))
lapply(address.corpus[1],content)

address.corpus <- tm_map(address.corpus, content_transformer(gsub), 
                         pattern = "america|american|americans|america",
                         replacement = "america")
lapply(address.corpus[1],content)

#DTM
address.dtm <- DocumentTermMatrix(address.corpus)
inspect(address.dtm)

as.matrix(address.dtm)
colSums(as.matrix(address.dtm))

termfreq <- colSums(as.matrix(address.dtm))
length(termfreq)

termfreq[head(order(termfreq, decreasing = TRUE),10)]
termfreq[tail(order(termfreq, decreasing = TRUE),10)]

?findFreqTerms
findFreqTerms(address.dtm, lowfreq=40)
findFreqTerms(address.dtm, lowfreq = 40, highfreq = 80)

library(ggplot2)
class(termfreq)

termfreq.df <- data.frame(word = names(termfreq), frequency = termfreq)
head(termfreq.df)

ggplot(subset(termfreq.df, frequency >=40),
       aes(x = word, y = frequency, fill= word))+
  geom_col(color="dimgray")+
  labs(x=NULL, y = "Term Frequency(count)")

ggplot(subset(termfreq.df, frequency >= 40),
       aes(x = reorder(word, frequency), y = frequency, fill = word))+
  geom_col(color = "dimgray", width = 0.5, show.legend = FALSE)+
  geom_text(aes(label = frequency), size=3.5, color="black",hjust=0)+
  labs(x=NULL, y = "Term Frequency(count)")+
  coord_flip()

inspect(address.dtm)

Docs(address.dtm)
row.names(address.dtm) <- c("Clinton","Bush","Obama","Trump","Biden")
Docs(address.dtm)


# 워드 클라우드
set.seed(123)

install.packages("wordcloud")
library(wordcloud)
head(termfreq)
wordcloud(words = names(termfreq), freq = termfreq,
          scale = c(3,0.2), min.freq = 10,
          rot.per = 0.1, random.order = FALSE,
          colors = brewer.pal(6, 'Dark2'))


# 각각 대통령별로 어떤 단어들이 많이 등장 ?

address.tf <- tidy(address.dtm)

address.tf <- address.tf %>% 
  mutate(document = factor(document, levels = c("Clinton","Bush","Obama","Trump","Biden"))) %>% 
  arrange(desc(count)) %>% 
  group_by(document) %>% 
  top_n(n=10,wt=count) %>% 
  ungroup()
address.tf

ggplot(address.tf,
       aes(term,count,fill=document))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~document, ncol=2,scales="free")+
  labs(x=NULL, y="Term Frequency count")+
  coord_flip()

ggplot(address.tf,
       aes(reorder_within(x=term,by=count,within=document),y=count,fill=document))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~document, ncol=2,scales="free")+
  scale_x_reordered()+
  labs(x=NULL, y="Term Frequency count")+
  coord_flip()


# TF-IDF
address.dtm2 <- DocumentTermMatrix(address.corpus,
                                   control = list(weighting = weightTfIdf))
inspect(address.dtm2)

row.names(address.dtm2) <- c("Clinton", "Bush", "Obama", "Trump", "Biden")

address.tfidf <- tidy(address.dtm2) %>% 
  mutate(tf_idf = count, count = NULL)

address.tfidf <- address.tfidf %>% 
  mutate(document = factor(document, levels = c("Clinton", "Bush", "Obama", "Trump", "Biden"))) %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(document) %>% 
  top_n(n = 10, wt = tf_idf) %>% 
  ungroup()
address.tfidf

ggplot(address.tfidf,
       aes(reorder_within(x = term,by = tf_idf,  within = document), y = tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, ncol = 2, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL, y = "tf_idf") +
  coord_flip()


## 13-2

# tidy-text 기반 TF-IDF

us.president.address <- tidy(data_corpus_inaugural) %>% 
  filter(Year > 2000) %>% 
  group_by(President, FirstName) %>% 
  summarise_all(list(~trimws(paste(., collapse = " ")))) %>%
  arrange(Year) %>% 
  ungroup()
us.president.address

# tidy text로만들기

# 단어를 기준으로 토큰화
address.words <- us.president.address %>% 
  unnest_tokens(output = word, input = text)
address.words

# 전처리하기
address.words <- address.words %>% 
  anti_join(stop_words, by="word") %>% #불용어제거
  filter(!grepl(patter = "\\d+", word)) %>%  #숫자제거
  mutate(word = gsub(pattern = "'", replacement ="",word)) %>% #공백제거
  mutate(word = gsub(pattern = "america|americas|american|americans",
                     replacement = "america", word)) %>% # 동의어처리
  count(President, word, sort = TRUE, name =  'count') %>% #단어 빈도 수 계산
  ungroup()
address.words


# 단어빈도 시각화
address.words %>% 
  group_by(word) %>% 
  summarise(count = sum(count)) %>% 
  arrange(desc(count)) %>% 
  top_n(n = 10, wt = count) %>% # => 상위10개 단어 추출
  ggplot(aes(reorder(word, count), count)) +
  geom_col(color = "dimgray", fill = "salmon", width = 0.6, show.legend = FALSE) +
  # theme(axis.test.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = count), size = 3.5, color = "black", vjust = 1) +
  labs(x = NULL, y ='Term Frequency (count)') +
  coord_flip()


# tf-idf계산 - bind_tf_idf() 함수
address.words <- address.words %>% 
  bind_tf_idf(term = word, document = President, n = count)
address.words
# => tf, idf, tf_idf 값이 계산됨


# 시각화를통해 tf-idf 순위 확인
address.words %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(President = factor(President,
                            levels = c("Clinton", "Bush", "Obama", "Trump", "Biden"))) %>% 
  group_by(President) %>% 
  top_n(7, wt = tf_idf) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(word, tf_idf, President), tf_idf, fill = President)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~President, ncol = 2, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL, y = "Term Frequency-Inverse Document Frequency") +
  coord_flip()


# tf-idf 상위 7개 단어 확인
address.words %>% 
  arrange(desc(tf)) %>% 
  mutate(President = factor(President,
                            levels = c("Clinton", "Bush", "Obama", "Trump", "Biden"))) %>% 
  group_by(President) %>% 
  top_n(7, wt = tf) %>% 
  ungroup() %>% 
  ggplot(aes(reorder_within(word, tf, President), tf, fill = President)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~President, ncol = 2, scales = "free") +
  scale_x_reordered() +
  labs(x = NULL, y = "Term Frequency(proportion)") +
  coord_flip()

