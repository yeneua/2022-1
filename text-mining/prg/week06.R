# 6-1

#텍스트 전처리 방법
#정규표현식?


#지난시간review
#텍스트 벡터 기본적인 전처리
#R내장함수

x <- "We have a dream"
class(x)
nchar(x)
length(x)

y <- c("We", "have", "a", "dream")
nchar(y)
length(y)
length(y[4])
nchar(y[4])

#letters
#문자열도 숫자처럼 sorting가능

tolower()
toupper()
#R 대소문자구분

fox.saｉｄ <- "what is essential is invisible to the eye" 
strsplit(fox.said,split=" ") #공백을 기준으로 각각의 벡터로
strsplit(fox.said, split="") #공백까지 모두 하나하나로로


fox.said.words <- unlist(strsplit(fox.said, split= " "))
fox.said.worsd

fox.said.words[3]
fox.said.words[1]

unlist(strsplit(fox.said, split=" "))


strsplit(fox.said, split=" ")
strsplit(fox.said, split=" ")[[1]][[3]]




p1 <- "You com eat four in the afternoon, then at three I shall bebine to be happy"
p2 <- "One runs the irst of weeping a litter if one lets himself be tamed"
p3 <- "What maeks the deser beautiful is that somewhere it hides a well"

littleprince <- c(p1,p2,p3)
littleprince

strsplit(littleprince, split = " ")
strsplit(littleprince, split = " ")[[3]]
strsplit(littleprince, split = " ")[[3]][[5]]
strsplit(littleprince, split = " ")[[1]][[7]]

#고유한 단어들만
unique(tolower(fox.said.words))
unique(toupper(fox.said.words))


paste() #벡터를 하나로 합쳐줌
paste("Everyone", "wants", "to","fly")
paste0("Everyone", "wants", "to","fly")#공백없이합쳐줌줌


heroes <- c("Batman", "Captain America", "Hulk", "yena")
colors <- c("Black", "Blue", "Green")

paste(heroes, colors) #나머지 거는 처음으로 다시 돌아감

paste("Type",1:10)
paste(heroes, "wants","to","fly")

paste(fox.said.words, collagpse = "-")

paste(month.abb) #내장데이터
paste(month.abb, 1:12)
paste(month.abb, 1:12, sep = "-") #공백X. bar
paste(month.abb, 1:12, sep = ":")
paste(month.abb, 1:!2, sep = "-", collapse = "_") #한꺼번에 가져오기. -> collapse
paste(month.abb, 1:12, sep = "-", collapse = " ")


outer(1:3, 1:3)
outer(c(1,2,3), c(1,2,3))

countries <- c("KOREA", "US", "EU")
stat <- c("GDP", "Pop", "Area")

outer(countries, stat, FUN = paste)
outer(countries, stat, FUN = paste, sep = "-") #구분자

#고객 주문 결과를 보여주는 시나리오
customer <- "Kim"
buysize <- 10
deliveryday <- 2

paste("hello" , customer, ",you order of ",buysize,
      "product(s) will be delivered within ", deliveryday,"days")

#sprintf()

sprintf("hello %s, you order of %s will be delivered within %s", customer, buysize, deliveryday) #더 쉽게, 직관적

#substr() : 특정부분만 가져오는것 
?substr
substr("Text Analytics", start = 1, stop = 4)
substr("Text Analytics", start = 6, stop = 14)

substring("Text Analytics",6)

class <- c("Data analytics", "Data visualization", "Data science introduction")
substr(class, 1, 4)
substring(class, 6)

countries <- c("Korea, KR", "United states, US", "China, CN") 
substring(countries, nchar(countries)-1)
substr(countries, nchar(countries)-1, nchar(countries))

?islands

head(islands)

names(islands)

landnames <- names(islands)

index <- grep(pattern = "New", x = landnames)

grep(pattern = "New", x = landnames, value = TRUE)

#텍스트의 치환
sub()
gsub()


txt <- "Data Analytics is useful. Data Analytics is also interesting" #문자구조 벡터
?sub()
sub(pattern = "Data",replacement="Business",x=txt)
#첫번째 문자열에만 적용
#=>gsub()
gsub(pattern = "Data",replacement="Business",x=txt)
gsub(pattern = "Data",replacement=" ",x=txt)

text2 <- c("product.csv","order.csv","customer.csv")#확장자필요없다
gsub(".csv","",text2) #이름만나옴
#=> 패턴추출


#정규표현식(regular expression)
#반복되는 패턴, ..



### 6-2 ###

#정규표현식
words <- c("at","bat","cat","chaenomeloes","chase","cheep","check","cheese","chick","hat")

grep("che",words,value=TRUE)#문자열 자체
grep("a",words,value=TRUE)
grep("at",words,value=TRUE)  #at문자열이 연속해서 들어있어야함

#대괄호기준
grep("[ch]",words,value=TRUE) #c혹은 h가 ㅍ포함된 문자열
grep("[at]",words,value=TRUE) #a혹은 t
grep("ch|at",words,value=TRUE)
grep("che|at",words,value=TRUE)
grep("ch(e|i)ck",words,value=TRUE) #ch로 시작 ck로 끝남. 중간에 i혹은e


#수량자 - 문자 내에서 반복되는것
#? : 0또는 1
#* : 최소 0회
#+ : 1회 이상

grep("chas?e",words,value=TRUE) #cha 다음에 s가 없어도 되고, 한번까지. e로끝나는
grep("chas*e",words,value=TRUE) #0혹은1회이상
grep("chas+e",words,value=TRUE) #한번이상

words1 <- c("at","bat","cat","chaenomeloes","chase","cheep","check","cheese","chick","hat","chasse")
grep("chas?e",words1,value=TRUE)
grep("chas*e",words1,value=TRUE)
grep("chas+e",words1,value=TRUE)

grep("ch(a*|e*)se",words,value=TRUE) #a가 한 번 이상. e가 한 번 이상 - 수량자와 소괄호 함께 이용해 표현



#메타문자 ^ $
#^ : 시작하는
#$:끝나는

grep("^c",words,value=TRUE)#c로 시작
grep("t$",words,value=TRUE)# t로 끝

grep("^c.t$",words,value=TRUE) # . : 모든문자열

grep("^c.*t$",words,value=TRUE) #c로 시작하고, t로끝나는 문자. 문자열이 0또는 1회 반복되는 것이 있는 문자열

grep("^[ch]?at",words,value=TRUE) #c또는 h로 시작.0또는1회.at로 끝나는.


#문자클래스

words2 <- c("12 Dec","OK","http://","<TITLE>Time?<TITLE>","12345","Hi there")

grep("[[:alnum:]]",words2,value=TRUE) #알파벳, 넘버를 포함한 모든 문자

grep("[[:alpha:]]",words2,value=TRUE)#알파벳만 들어있는 것

grep("[[:digit:]]",words2,value=TRUE)#숫자가포함된

grep("[[:punct:]]",words2,value=TRUE)#특수문자

grep("[[:space:]]",words2,value=TRUE)#공백문자-두개이상의 단어로 이루어진



#문자 클래스 시퀀스

grep("\\w+",words2,value=TRUE) #\\ : 두개
grep("\\s+",words2,value=TRUE)#space를 포함한
grep("\\d+",words2,value=TRUE) #숫자를 포함한
grep("\\D+",words2,value=TRUE) #숫자가 있는 것을 제외한




















