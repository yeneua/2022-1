# 공부

#지난시간 review

x <- "We have a dream"
x
class(x)
nchar(x) #길이
length(x)

y <- c("We","have","a","dream")
nchar(y)
length(y)
length(y[4])
nchar(y[4])

fox.says <- "It is only with the HEART that one cas See Rightly"
fox.says

tolower(fox.says)
toupper(fox.says)

fox.said <- "what is essential is invisible to the eye"
strsplit(fox.said, split= " ")
strsplit(fox.said, split="") #strsplit() : 리스트형태로 반환

fox.said.words <- unlist(strsplit(fox.said, split= " "))
fox.said.words #unlist() : 벡터형태로

fox.said.words[3]
fox.said.words[1]

unlist(strsplit(fox.said,split= " "))
unlist(strsplit(fox.said,split= " "))[3]

strsplit(fox.said, split= " ")
strsplit(fox.said, split= " ")[[1]][[3]]

p1 <- "You come at four in the afternoon, then at three I shall bebine to be happy"
p2 <- "One runs the irst of weeping alitter, if one lets himself be tamed"
p3 <- "What make the desert beautiful is that somewhere it hides a well"

littleprince <- c(p1,p2,p3)
littleprince #각각 벡터 형태로 저장되어있음

strsplit(littleprince, split= " ")
strsplit(littleprince, split= " ")[[3]]
strsplit(littleprince, split= " ")[[3]][[5]]
strsplit(littleprince, split= " ")[[1]][[7]]

fox.said <- "WHAT IS ESSENTIAL is invisible to the Eye"
unlist(strsplit(fox.said,split= " "))
fox.said.words <- strsplit(fox.said, split=" ")[[1]]
fox.said.words
unique(tolower(fox.said.words))
unique(toupper(fox.said.words))

paste("Everyone","wants","to","fly")
paste("Everyone","wants","to","fly", sep="-")
paste("Everyone","wants","to","fly",sep="")
paste0("Everyone","wants","to","fly")

fox.said.words
paste(fox.said.words)

paste(pi, sqrt(pi))

paste("25 degree Celsius is", 25*1.8+32, "degree Fahrenheit")

heroes <- c("Batman","Captain America","Hulk")
colors <- c("Black","Blue","Green")

paste(heroes, colors)

paste("Type", 1:10)

paste(heroes, "wants","to","fly")

fox.said.words
paste(fox.said.words, collapse="-")



month.abb
paste(month.abb,1:12)
paste(month.abb,1:12,sep="-")
paste(month.abb,1:12,sep="-",collapse="_")
paste(month.abb,1:12,sep="-",collapse=" ")

outer(1:3,1:3)
outer(c(1,2,3),c(1,2,3))

countries <- c("KOR","US","EU")
stat <- c("GDP","Pop","Area")

outer(countries,stat, FUN=paste, sep="-")



#고객 주문 결과를 보여주는 시나리오

customer <- "R"
buysize <- 10
deliveryday <- 2

paste("hello",customer,",your order of",buysize,"product(s) will be delivered within",deliveryday)

#sprintf()
sprintf("hello %s your order of %s product(s) will be delivered within %s", customer, buysize, deliveryday)

substr("text analytics", start=1,stop=4)
substring("text analytics",6)


class <- c("data analytics", "cata visualization", "data science introduction")
substr(class,1,4)
substring(class,6)

countries <- c("Korea, KR", "United states,US","China, CN")
substring(countries, nchar(countries)-1)
substr(countries, nchar(countries)-1, nchar(countries))


landnames <- names(islands) #열이름만
index <- grep(pattern="New",x=landnames) #New(특정요소)를 가지고 있는 것만 추출
grep(pattern ="New", x=landnames, value=TRUE)
grep(pattern = " ", x=landnames, value = TRUE)


### review끝 ###

#추출하고, 원하는 형태로 텍스트 치환 등 .. 추가적인 작업을 하게 된다
sub()
gsub()

txt <- "Data Analytics is useful. Data Analytics is also interesting"
?sub() #pattern matching and replacement

sub(pattern = "Data", replacement="Business",x=txt) # txt에서 Data를 Business로 바꾸기 -> 첫번째 문자열에만 적용됨
gsub(pattern= "Data", replacement="Business",x=txt) #모든 문자열에 다 적용시키려면 gsub함수이용
gsub(pattern = "Data",replacement="",x=txt)

text2 <- c("product.csv","order.csv","customer.csv")
gsub(pattern=".csv", replacement="",x=text2) #.csv를 없애고싶다
gsub(".csv","",text2)


#정규표현식
#반복되는 패턴턴
#조건에 만족하는 결과들 리턴 ..


## 2차시

#정규표현식
txt

words <- c("at","bat","cat","chaenomeloes","chase","cheep","check","cheese","chick","hat")

grep() #grep(패턴, 데이터셋, value값)
grep("che", words,value=TRUE) #che를 포함
grep("a",words, value=TRUE) #a를 포함하는 것들
grep("at", words, value=TRUE)

#정규표현식으로 표현하기
grep("[ch]", words, value=TRUE) #  c혹은 h가 포함된 모든 문자
grep("[at]", words, value=TRUE) #a혹은 t가 포함된 모든 문자열

grep("ch|at",words, value = TRUE) #at혹은 ch가 들어간 문자열
grep("che|at", words, value=TRUE) #che 혹은at가 들어간 문자열
grep("ch(e|i)ck", words, value=TRUE) #ch로 시작. ck로 끝. 중간의 문자가 e혹은 i

#수량자 - 문자 내에서 반복되는 것
grep("chas?e", words, value=TRUE) # cha로 시작하고. s(물음표 바로 앞)가 0회 또는 1회(최대1회). 그 다음이 e인 문자열 => chaenomeloes, chase 출력됨
grep("chas*e", words, value=TRUE) # * : 앞 문자는 0회 이상 반복됨(최소0회)
grep("chas+e", words, value=TRUE) # + : 앞 문자는 1회 이상 반복됨(최소1회)

wordss <- c("at","bat","cat","chaenomeloes","chase","cheep","check","cheese","chick","hat",
            "chasse") #chasse추가
grep("chas?e", wordss, value=TRUE)
grep("chas*e", wordss,value = TRUE)
grep("chas+e", wordss, value = TRUE)

grep("ch(a*|e*)se", words, value = TRUE) #ch로 시작. se로끝남. a* 혹은 e* 중 하나라도 만족하면 추출(a혹은 e가 한 번 이상). 


#메타문자
#  ^ : 문자의 시작
#  $ : 문자의 끝 

grep("^c", words, value = TRUE) #c로 시작하는 문자

grep("$c", words, value = TRUE) #c로 끝나는 문자
grep("t$", words, value = TRUE) #t로 끝나는 문자

grep("^c.t$", words, value = TRUE) #c로 시작. t로 끝나는 문자열 # . : 모든 문자열 
grep("^c.*t$", words, value = TRUE) #c로 시작. t로 끝나는 문자열인데, 그 안에 반복되는 문자열이 있는 문자

wordsss <- c("at","bat","cat","chaenomeloes","chase","cheep","check","cheese","chick","hat",
             "ca-t")
grep("^c.*t$",wordsss, value=TRUE)

grep("^[ch]?at", words, value = TRUE) #c또는 h로 시작하고(<-0또는 1회 반복), at로 끝나는 문자열 - c또는 h가 있어도, 없어도 됨


#문자클래스
words2 <- c("12 Dec","OK","http://","<TITLE>Time?<TITLE>","12345", "Hi there")

grep("[[:alnum:]]", words2, value = TRUE) #알파벳 문자 + 숫자
grep("[[:alpha:]]", words2, value = TRUE) #알파벳 문자
grep("[[:digit:]]", words2, value = TRUE) #숫자
grep("[[:punct:]]", words2, value= TRUE) #문장부호
grep("[[:space:]]", words2, value = TRUE) #스페이스문자

#문자 클래스 시퀀스
grep("\\w+", words2, value= TRUE) #단어,문자
grep("\\s+", words2, value = TRUE) #스페이스문자
grep("\\D+", words2, value = TRUE) #숫자를제외한문자
