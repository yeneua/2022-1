#2017447 김예나
#9-1

text <- c("Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg",
          "A vegetar$ian diet excludes all animal flesh (meat, poultry, seafood).",
          "Economists surveyed by Refinitiv expect the economy added 160,000 jobs.") #vector
install.packages("tm")
library(tm)
data(crude)
crude

crude[[1]]
crude[[1]]$content
crue[[1]]$meta
text

VCorpus()
getSources()

corpus.docs <- VCorpus(VectorSource(text))
class(corpus.docs)

inspect(corpus.docs[1])
inspect(corpus.docs[[1]])

as.character(corpus.docs[[1]])

lapply(corpus.docs, as.character)

str(corpus.docs)

corpus.docs[[1]]$content #첫번째 들어있던 content
lapply(corpus.docs, content) #(가져올 corpus, 하려고하는것)

as.vector(unlist(lapply(corpus.docs,content)))

paste(as.vector(unlist(lapply(corpus.docs, content))),collapse = " ")

corpus.docs[[1]]$meta

meta(corpus.docs[[1]])
meta(corpus.docs[[1]], tag = "author")
meta(corpus.docs[[1]], tag = "id")

meta(corpus.docs[[1]],tag = "author", type="local") <- "Dong-A"

cor.author <- c("Dong-A","Kim","Ryu")
meta(corpus.docs[[1]],tag = "author", type="local") <- "cor.author"
corpus.docs[[1]]$meta
corpus.docs[[2]]$meta
corpus.docs[[3]]$meta

lapply(corpus.docs,meta)
lapply(corpus.docs,meta, tag="author")


# 9-2
lapply(corpus.docs, meta)

category <- c("health","lifestyle","business")
meta(corpus.docs, meta, tag="category", type="local") <- category #추가
lapply(corpus.docs, meta)

meta(corpus.docs, meta, tag="origin", type="local") <- NULL #origin 지우기
lapply(corpus.docs, meta)

lapply(tm_filter(corpus.docs, FUN=function(x)
  any(grep("weight|diet", content(x)))),content)

lapply(corpus.docs, content)

index <- meta(corpus.docs, "author") == "Dong-A"|meta(corpus.docs,"authot") =="Ryu"

lapply(corpus.docs[index], content)

#corpus저장
writeCorpus(corpus.docs)
list.files(pattern="\\.txt") #텍스트형태로 저장


#텍스트정제
getTransformations()
tm_map()
toupper()
tolower() #corpus에는 적용 X

content_transformer()

lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, content_transformer(tolower))


#불용어(stopwords)
stopwords("english") #추가,수정,삭제 가능

corpus.docs <- tm_map(corpus.docs,removeWords, stopwords("english"))

lapply(corpus.docs, content)

myRemoves <- content_transformer(function(x,pattern)
  {return(gsub(pattern,"",x))})

corpus.docs <- tm_map(corpus.docs,myRemoves,"(f|ht)tp\\S+\\s*") #url없애기
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs,removePunctuation)
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs,removeNumbers)
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs,stripWhitespace) #공백없애기
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs,content_transformer(trimws)) #맨앞공백 지우기
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs,stemDocument) #어간처리
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs,content_transformer(gsub),pattern="economist",replacement="economi")
lapply(corpus.docs, content)

