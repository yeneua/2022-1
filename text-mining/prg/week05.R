## Lecture 5 : 문자 벡터의 이해
#텍스트 데이터를 원하는대로 handling

# 5-1

x <- "We have a dream"
x
class(x) #character
nchar(x) # 15 char가 몇개인지 불러오기. 공백 포함
length(x) #1  전체 요소.벡터가 몇개인지

 y <- c("We", "have", "a", "dream")
class(y) #character
nchar(y) # 2 4 1 5 공백을 제외한 각각의 벡터의 구성요소의 개수.길이
length(y) # 4
length(y[4]) #1  하나가 들어가있으니까
nchar(y[4]) #5  4번째 요소의 ncahr?
length(y[2]) #1

letters # 내장데이터 - 알파벳

#문자열sorting
sort(letters, decreasing = TRUE) #내림차순. 알파벳 역순으로 나옴

tolower() #소문자로 변환
toupper() #대문자로 변환

fox.says <- "It is only with the HEART that one can See Rightly"
fox.says
tolower(fox.says)
toupper(fox.says)

#text분할, 결합
strsplit() #분할할때 쓰는함수. 리스트 형태로 결과 리턴. 리스트 인덱싱으로 결과 액세스
fox.said <- "what is essential is invisible to the eye"
fox.said # 문자벡터
strsplit(fox.said, split=" ") #공백으로 분할. 리스트 타입으로 나옴
strsplit(fox.said, split="") # 공백까지도 하나의 요소로 반환함. 이런 방식은 잘 안씀, 주로 바로 윗행같이 단어위주로 ~
unlist(strsplit(fox.said, split= " ")) # 결과 리스트가 아니라 벡터형태로
fox.said.words <- unlist(strsplit(fox.said, split= " "))
fox.said.words #vetor 형태로
fox.said.words[3]
fox.said.words[1]

unlist(strsplit(fox.said, split= " "))[3]
strsplit(fox.said, split= " ")[[1]] #list니까 list indexing
strsplit(fox.said, split= " ")[[1]][[3]]



p1 <- "You come at four in the afternoon, then at three I shall bebine to be happy"
p2 <- "One runs the irst of weeping a litter, if one lets himself be tamed"
p3 <- "What makes the desert beautiful is that somewhrere it hides a well"

littleprince <- c(p1,p2,p3)
littleprince #3개의 문자 벡터가 저장
strsplit(littleprince, split= " ") #각각이 공백을 기준으로 분할. 리스트로 리턴.
# ? 왜 리스트로 리턴? => 각각의 데이터 마다 길이도 다르고, 타입이 다르기 때문에.  방법이 리스트밖에 없다.
strsplit(littleprince, split=" ")[[3]]
strsplit(littleprince, split=" ")[[3]][[5]] #beautiful
strsplit(littleprince, split=" ")[[1]][[7]] #afternoon

#the는 각각의 원소마다 다 들어있다. -> 따로따로 뽑는게 아니라 unique한것만 뽑고싶다.=> unique() 이용
fox.said <- "WHAT IS ESSENTIAL is invisible to the Eye"
strsplit(fox.said, split = " ") #공백을 기준으로 구분
fox.said.words <- strsplit(fox.said, split = " ")[[1]]
fox.said.words
unique(fox.said.words) # 대소문자 구문: IS, is 다 나옴
#고유한 단어들만 가져오고싶다. 중복없이
unique(tolower(fox.said.words)) #is가 하나만 나옴. 소문자로 바꿔줬기떄문에.
unique(toupper(fox.said.words))

paste() #결합할때. 벡터를 하나로 합쳐줌
paste("Everyone", "wants", "to","fly") # => "Everyone wants to fly"
#구분자를 지정하지 않으면 공백을 기본으로
paste("Everyone", "wants", "to","fly", sep = "-")
paste("Everyone", "wants", "to","fly", sep = "")
paste0("Everyone", "wants", "to","fly") #윗행과 동일한 결과. 전부 붙이는 것 : paste0

fox.said.words #8개로 구성된 하나의 벡터
paste(fox.said.words)  #하나의 벡터이기 때문에 수행할것X

#문자가 아닌경우, 문자로 변환시켜 return
paste(pi, sqrt(pi)) #pi : 3.14 ~~

#문자열과 결합하여 사용
paste("25 degrees Celsius is", 25*1.8+23, "degree Fahrenheight")


#벡터간의 연산도 지원
heroes <- c("Batman", "Captain America", "Hulk")
colors <- c("Black", "Blue", "Green")
  
# 두개의 벡터를 paste
paste(heroes, colors)

#하나의 요소가 부족하다면, 재사용되는 형태로 붙여짐
paste("Type",1:10) #공백을 기준으로 paste합침

paste(heroes, "wants", "to" , "fly") #heroes안에 있는 요소들을 하나하나 꺼내서 뒤에 거랑 붙임. 뒤에 내용용들이 재사

#paste함수안의 collapse인자
fox.said.words
paste(fox.said.words, collapse = " ") #공백으로 하나로 합침
paste(fox.said.words, collapse = "-")

# 중요한 것
#strsplit -> 리스트로 결과반환
#paste -> 벡터 결합. 벡터들간의 연산


### 5-2 ###

#strsplit 텍스트 분할. 리스트로 리턴.
#unlist 벡터형태로 변환
#paste 벡터를 하나로 붙이기.  sep : 구분자(기본 - 공백). 벡터
#paste  -  collapse 여러가지 벡터 형태로 나올때. 그 벡터를 한꺼번에 뭉쳐서 출력

paste(month.abb) #내장데이터
paste(month.abb, 1:12)
paste(month.abb, 1:12, sep = "-") #공백이 아닌 bar가 들어감
paste(month.abb, 1:12, sep = ":")
paste(month.abb, 1:!2, sep = "-", collapse = "_") #하나하나 벡터 아니고, 한꺼번에 가져오고 싶다. -> collapse
paste(month.abb, 1:12, sep = "-", collapse = " ")

outer()
outer(1:3, 1:3)
outer(c(1,2,3), c(1,2,3)) #각각의 숫자들과 쌍을 이루는 모든 가능한 조합들의 곱

outer(x,y, fun= ) #기본 func: 곱

countries <- c("KOREA", "US", "EU")
stat <- c("GDP", "Pop", "Area")

outer(countries, stat, FUN = paste) #함수 - 곱 아니고 paste
outer(countries, stat, FUN = paste, sep = "-")


#고객 주문 결과를 보여주는 시나리오

customer <- "Kim"
buysize <- 10
deliveryday <- 2

paste("hello" , customer, ",you order of ",buysize,
      "product(s) will be delivered within ", deliveryday,"days")

#sprintf() - 문장내에 변수랑 섞어서 사용 가능
sprintf("hello %s, you order of %s will be delivered within %s",
        customer, buysize, deliveryday) #한꺼번에 지정. 직관적

customer <- c("Kim","Choi","Ryu")
buysize <- c(10,8,9)
deliveryday <- c(2,3,7.5)

sprintf("hello %s, you order of %s will be delivered within %s", customer, buysize, deliveryday)

?

sprintf
#sprintf 장점 여러가지 유형으로 출력할 수 있다.
sprintf("hello %s, you order of %s will be delivered within %.2f",
        customer, buysize, deliveryday) #  소수점


# 텍스트 분석을 하다보면, 특정 부분만 뽑아서 작업을 하는 경우가 생김
#substr() : 특정부분만 가져오는것 
?substr
substr("Text Analytics", start = 1, stop = 4) #첫번째부터 4번째까지 인덱싱
substr("Text Analytics", start = 6, stop = 14)
substr("Text Analytics", start = 3, stop = 10) #공백포함

#substring() : 시작점을 지정해주면, 거기서부터 끝까지 가져오는 것
substring("Text Analytics", 6) #뒤에 인자가 하나. 6부터 끝까지

#벡터 형태도 당연히 가능
class <- c("Data analytics", "Data visualization", "Data science introduction")
substr(class, 1, 4) #각각의 요소에서 1~4번째까지. 동일한 위치
substring(class, 6) #substring - 끝 위치 정확히 모를때 사용하면 좋다.

countries <- c("Korea, KR", "United states, US", "China, CN") #길이가 다 다른 문자
#각각의 젤 뒤에 약자만 가지고 오고 싶다하면
substring(countries, nchar(countries)-1) #nchar() : 전체 길이를 구하는
substr(countries, nchar(countries)-1, nchar(countries)) #하지만 맨 마지막까지 작업할때는 substring 함수를 이용하자


#grep() : 특정 패턴을 가진 string에서, 일부분만 가져오는 것

?islands
head(islands)
names(islands)

landnames <- names(islands)

?grep
grep(pattern = "New", x = landnames) #New라는 단어가 포함된 섬 이름
# => 이름이 아니라 원소의 인덱싱 값을 보여줌

index <- grep(pattern = "New", x = landnames)
landnames[index]

landnames[grep(pattern = "New", x = landnames)]

grep(pattern = "New", x = landnames, value = TRUE) #value = TRUE를 하면 인덱스 값이 아니라 실제 인자값을 그대로 리턴해줌

#landnames가 두 단어 이상으로 된 것만 가져오자 - 두 단어라면 공백이 있음
grep(pattern = " ", x= landnames, value = TRUE)


#텍스트를 한줄한줄 읽어서 리스트로 들어온것을, unlist해서 전체적인 말뭉치. 그것을 다시 하나의 벡터로 collapse
#문자를 크롤링하면 동일한 패턴을 가지게 되는 경우가 많이 있다. 패턴들을 기준으로 분할, 추출, 그 이후에 것 가져오기, 기준이 포함된 .. 등등 작업을 하게 됨

