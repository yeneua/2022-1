# 7주차
## 7-1

string <- c("data analysis is useful",
            "business analytics is helpful",
            "visualization of data is interesting for data scientists")
string

grep("data", string) #string이 포함된 요소 자체
grep("data", string, value=TRUE)

string[grep("data", string)]

grepl("data", string) #true, false로 리턴


## regexpr(), gregexpr()
regexpr(pattern = "data", text=string) #첫번째 것만 리턴

gregexpr("data", string)


#regmatches() : regexpr() + gregexpr()
regmatches(x = string, m = regexpr(pattern="data", text=string)) #

regmatches(x = string, m = regexpr(pattern="data", text=string))

regmatches(string, regexpr("data", string), invert=TRUE)
regmatches(string, gregexpr("data", string), invert=TRUE)


## sub(), gsub() - 치환

?sub()
sub(pattern = "data", replacement = "text", x=string)

gsub(pattern = "data", replacement = "text", x=string)


#strsplit()
strsplit(string," ")
unlist(strsplit(string, " "))
unique(unlist(strsplit(string, " ")))

string
gsub("is|of|for", "", string)

gsub("is|of|for", "", unique(unlist(strsplit(string, " ")))) #is,of,for => ""
sub("data","date",unique(unlist(strsplit(string," "))))
unique(sub("date","data",unique(unlist(strsplit(string, " ")))))


### 7-2 ###

install.packages("stringr")
library(stringr)

string <- c("data analysis is useful",
            "business analytics is helpful",
            "visualization of data is interesting for data scientists")

#str_ 로 시작
?str_detect
str_detect(string = string, pattern = "data") #grepl()과 동일한 기능 수행
str_detect(string,"DATA") #R:대소문자구분
str_detect(string, fixed("DATA", ignore_case=TRUE))

str_detect(c("aby","acy","a.y"),"a.y") # . : 모든문자 -> 3개 전부 TRUE
str_detect(c("aby","acy","a.y"), fixed("a.y"))
str_detect(c("aby","acy","a.y"),"a\\.y")#메타문자 아니고, 문자 그대로 인식 => 이스케이프 활용

#위치검출
str_locate(string,"data")
str_locate_all(string,"data")

#문자열추출 regmatches()
str_extract(string, "data")
str_extract_all(string,"data")
str_extract_all(string,"data",simplify=TRUE)#행렬형태로반환

unlist(str_extract_all(string,"data"))

sentences5 <- sentences[1:5]

str_extract(string,"(a|A|the|The) (\\w)") #관사
str_extract(string,"(a|A|the|The) (\\w+)")
str_extract(sentences5,"(a|A|the|The) (\\w+)")

str_match(sentences5, "(a|A|the|The) (\\w+)")
str_match_all(sentences5, "(a|A|the|The) (\\w+)")


#치환
str_replace(string, "data", "text") #data를 text로(첫번째만)
str_replace_all(string,"data","text")

#분할
str_split(string," ") #공백을 기준으로 분할 -> 리스틓형태로반환
str_split(sentences5, " ")
unlist(str_split(sentences5, " "))
unique(unlist(str_split(sentences5, " ")))

str_split(sentences5, " ", n=5)
str_split(sentences5, " ", n=5,simplify=TRUE)


#추가기능들
str_length(string)
str_length(sentences5)

str_count(string, "data")
str_count(string, "\\w+")

str_pad() #채워넣는것
mon <- 1:12
#단위를 두자리로 맞추기
str_pad(mon, width=2, side="left", pad="0")

str_trim()#공백없애는 함수
string <- c("data analysis is useful",
            "business analytics is helpful",
            "visualization of data is interesting for data scientists")
string_pad <- str_pad(string, width=max(str_length(string)),
        side = "both", pad=" ")
str_trim(string_pad)
