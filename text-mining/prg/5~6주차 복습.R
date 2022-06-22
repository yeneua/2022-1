# 5~6주차 복습

x <- "We have a dream"
nchar(x)
length(x)

y <- c("We", "have", "a", "dream")
nchar(y)
length(y)
y[1]

sort(letters, decreasing = FALSE)
sort(letters, decreasing = TRUE)
sort(c(letters, LETTERS), decreasing = FALSE) # => a A b B c C ...
sort(c(letters,LETTERS), decreasing = TRUE) #=> Z z Y y X x ...


fox.says <- "It is only with the HEART that one can See Rightly"
tolower(fox.says)
toupper(fox.says)

fox.said <- "what is essential is invisible to the eye"
strsplit(fox.said, split= " ")
class(strsplit(fox.said, split= " ")) #=> list  
strsplit(fox.said, split="")

unlist(strsplit(fox.said, split = " "))
class(unlist(strsplit(fox.said, split=" "))) #=> character
fox.said.words <- unlist(strsplit(fox.said, split = " "))
class(fox.said.words)
fox.said.words[3]
fox.said.words[5]

unlist(strsplit(fox.said, split = " "))[2]
strsplit(fox.said, split = " ")[[1]]
class(strsplit(fox.said, split=" ")) # => list
length(strsplit(fox.said, split = " ")) # => 1(길이가 1인 리스트)
strsplit(fox.said, split = " ")[[1]]
strsplit(fox.said, split = " ")[[1]][[4]] #=> "is"
toupper(strsplit(fox.said, split = " ")[[1]][[4]]) #=> "IS"


# paste()  - sep VS collapse

paste(1,2,3,4, sep ="-")
paste(1,2,3,4, collapse="-")
paste('what','is','the','difference','sep','and','collapse',sep = " ")

paste(c(1,2,3,4),letters[1:4])

paste(c(1,2,3,4),letters[1:4], sep="/")
paste(c(1,2,3,4),letters[1:4], collapse="/")

paste(c(1,2),letters[1:2])
paste(c(1,2),letters[1:2], sep="/")
paste(c(1,2),letters[1:2], collapse="/")


# sep -> separate(구분하다) paste안에 나열된 원소 마다 구분 옵션
# collapse -> 결과값이 두 개 이상일때, 그 결과값들을 이어붙여서 결과값ㅇ르 하나로 만들어줌