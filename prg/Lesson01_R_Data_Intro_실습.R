######################
# Lesson01_R_Intro.R

#####################
# Lesson01_01. R 
#####################


dim(available.packages())

#####################
# Lesson01_02. R 기본
#####################

# Workspace

setwd("d:/r_workspace")
getwd()
dir()


#####################
# R Packages
#####################

pkgList <- installed.packages()

library()

install.packages("ggplot2")
#library(ggplot2)

# install.packages("ggplot2", depend=T)

update.packages()

#####################
# R Datatype
#####################

a <- 1
class(a)  #numeric
mode(a)   #numeric
typeof(a) #double

?class
?mode
?typeof

a <- 1L
class(a)  #integer
mode(a)   #numeric
typeof(a) #integer


a <- 'a'
class(a)  #character
mode(a)   #character
typeof(a) #character


a <- "a"
class(a)  #character
mode(a)   #character
typeof(a) #character


a <- 1+1i
class(a)  #complex
mode(a)   #complex
typeof(a) #complex


a <- TRUE
class(a)  #logical
mode(a)   #logical
typeof(a) #logical


a <- F
class(a)  #logical
mode(a)   #logical
typeof(a) #logical

?as.raw
a <- as.raw(0xa3)
a
class(a)  #raw
mode(a)   #raw
typeof(a) #raw

a <- as.raw(40)
a
class(a)   #raw
mode(a)    #raw
typeof(a)  #raw


a <- 1
a <- as.integer(1)
class(a)

a <- 1L
class(a)  #integer
mode(a)   #numeric
typeof(a) #integer

is.integer(a) #TRUE
is.numeric(a) #TRUE

?integer


## 생각해보세요
### mode(), class(), typeof()의 차이에 대해 이야기


?class
?mode
?typeof

a <- 10
class(a)  #numeric
mode(a)   #numeric
typeof(a) #double

b <- TRUE
class(b)  #logical
mode(b)   #logical
typeof(b) #logical

c <- "test"
class(c)  #character
mode(c)   #character
typeof(c) #character

d <- 1+2i
class(d)  #complex
mode(d)   #complex
typeof(d) #complex

e <- as.factor('e')
class(e)  #factor
mode(e)   #numeric
typeof(e) #integer

f <- "3"
class(f)  #character
mode(f)   #character
typeof(f) #character

#####################
# R Data structures
#####################

## vector

x <- c(1:20)
x
## matrix

m <- matrix(nrow = 2, ncol=3)
m

dim(m)
attributes(m)

m <- matrix(1:6, nrow=2, ncol=3) #열단위로 먼저 들어감
m


?matrix

# column-wise

m <- matrix(1:6, nrow=2, ncol=3, byrow=F)
m

# row-wise

m <- matrix(1:6, 2, 3, T) #T : byrow=T
m

m <- 1:10 #vector
m
is.vector(m) #TRUE
dim(m) <- c(2, 5) #m을 2행 5열로 차원 변경
m
is.matrix(m) #TRUE -> matrix로 바뀜

m[1,] #1행 데이터
m[,2] #2열 데이터

rownames(m) <- c("신입", "임원")
m

# cbind (column-binding)

x <- 1:3
y <- 10:12
xy <- cbind (x, y)
class(xy) #"matrix" "array"
typeof(xy) #"integer"
mode(xy) #"numeric"
xy<-cbind (x, y, x+y)
class(xy)

# rbind (row-binding)

rbind(x, y)
rxy <- rbind(x, y, x+y)
rownames(rxy) <- c("x","y","x+y")
rxy
class(rxy)  #matrix array
mode(rxy)   #numeric
typeof(rxy) #integer




m <- matrix(1:6, 2, 3)
m

m1 <- matrix(1:6, 2, 3, byrow = T) #행단위 먼저 집어넣기
m1
typeof(m1)

m2 <- matrix(1:6, 1)
m2
typeof(m2) #integer
is.matrix(m2) #TRUE
is.vector(m2) #FALSE -> 1행의 형태로 vector형태로 만들었지만 matrix다!!



## array

# 
x <- array(1:20, dim=c(2,2,5)) #2행 2열, 5개의 차원
x
x[,,5] #5차원
x[1,,] #모든 차원에서 1행
x[,,1]
x[1,1,1]
x[1,2,3] #1행 2열 3차원
x[2,1,4] #2행 1열 4차원
x[,,1] #1차원
x[1,,] #모든 차원에서 1행가져옴 - 2행(2개의 열이니까) 5열(5개의 차원)의 구조.
x[2,,] #모든 차원에서 2행가져옴
x[3,,] #3행 없으니까 가져올 수 없음
?array


a2 <- array(1:18, c(2,3,2))
a2
a2[1,,] #모든 차원에서 1행 - 3행 2열의 구조 : 3행(원래 데이터셋이 3개의 열이니까), 2열(2개의 차원이니까)
a2[2,,]


#구조형변수
seq(1,7, by=2) # 1 3 5 7
seq(1, -1, by=-0.5) # 1.0 0.5 0.0 -0.5 -1.0
seq(1,6,by=3) # 1 4
seq(1,7, length =3 ) #1 4 7 # 1~7을 3개로 추출(3개니까 간격은 2개겠져)
seq(1,7, length = 5) # 1.0 2.5 4.0 5.5 7.0

rep(c(1,2,3), 3) #123123123 반복
rep(1:3, 2) #123123


v1 <- c(1,2,3)
v1

v2 <- c("a","b","c")
v2

v3 <- c(T,F,T)
v3

weight <- c(56,45,70)
name <- c("홍길동","이순신","성춘향")
names(weight) <- name
weight
weight['이순신']
weight[56] # 이건 벡터다 !! 열이름 같은 건 없슴


v1 <- c(1,2,3,4,5)
v1[3]
v1[c(2,3)] #2,3번째 값만 추출
v1[-c(2,3)] #2,3번째 값빼고 추출
v1[c(-2,-3)] #2,3번째 값 빼고 추출
v1[3] <- 6 #3번째 값이 6으로 대체됨
v1

replace(v1, 2, 9) #v1 벡터의 2번째 값을 9로 대체
append(v1, 8, 5) #v1 벡터에, 8이라는 값을, 5번째 위치 다음에 추가

length(v1) #5

v1
sort(v1) #오름차순
rev(sort(v1)) #내림차순
rank(v1) #자료의 오름차순 순위
order(v1) #자료 오름차순에 의한 자료의 위치값

?matrix(data, nrow, ncol)
mtr <- matrix(1:6,2,3)
mtr

length(mtr)
mode(mtr)
dim(mtr)
dimnames(mtr)
rownames(mtr)
colnames(mtr)
#matrix 이름 지정
rownames(mtr) <- c("r1","r2")
colnames(mtr) <- c("c1", "c2", "c3")
mtr
rownames(mtr)
colnames(mtr)


mtr[1,]
mtr[,1]


arr <- array(1:12,c(2,2,3))
arr

length(arr) #12
mode(arr) #numeric
dim(arr) # 2 2 3
dimnames(arr)
#array 이름 지정
dimnames(arr) <- list(c("x1","x2"),      #행
                      c("y1","y2"),      #열
                      c("z1","z2","z3")) #차원
arr
dimnames(arr)


no <- c(1,2,3,4)
name <- c('apple','peach','banana','grape')
price <- c(500,200,100,50)
qty <- c(5,2,4,7)
sales <- data.frame(NO=no, NAME=name, PRICE=price, QTY=qty) #dataframe생성
saless <- data.frame(no,name,price,qty) #변수 이름 그대로 들어감
saless
names(sales) <- c("NO","이름","가격","개수")
sales

sales$NO

sales[,c(1,2)]#1,2열
sales[c(1,2),]#1,2행
sales[,1]
sales[1,]
subset(sales, qty<5) #subset으로 조건 부여
subset(sales, price==2000)
subset(sales, price==200)


sub1 <- subset(sales, select=c(이름, 가격)) #특정 열만 선택
sub1
sub2 <- subset(sales, qty<5)
sub2
sub3 <- sales[1,] #특정행만 추출하여 변수에 따로 할당
sub3




# 과제 
## vector, matrix, array 데이터 구조에 대해 설명하라


## list

#
mylist <- list(id=1001, name="song", scores=c(89, 90, 95, 99))
mylist

mylist$scores  #mylist[[3]]
mylist$scores[3] #mylist[[3]][3]
mylist[[3]][3]


mylist[[3]]
mylist[[1]]


list1 <- list(name="홍길동", address='부산', pay=5000)
list1
list1[[1]]         #실제 값을 가져오는 것
typeof(list1[[1]]) #character
list1[1]           #list자체를 가져오는것
typeof(list1[1])   #list

list1$name
list1$name <- NULL #name삭제
list1

unlist(list1) #list를 vector로 변환
typeof(list1) #list

## data.frame

kids <- c("홍길동", "이순신", "김동아")
ages <- c(7, 5, 10)
temp <- c(1000, 2500, 3500)
children <- data.frame(kids, ages, temp, stringsAsFactors=FALSE)
children <- data.frame(kids, ages)
children 

mode(children)   #list
class(children)  #data.frame
typeof(children) #list
attributes(children)
class(children$kids) #character
class(children$ages) #numeric

summary(children)
mean(children$temp)

children[-1, c(2,3)] #1행빼고, 2,3열 추출

#subset 데이터의 일부분만 발췌

c1 <- subset(children, ages<= 7)
c1

c1 <- subset(children, ages<= 7, select=c(kids, temp))
c1

## factor

ft <- c("f", "f", "m", "m", "f")
ft
class(ft)   #character
mode(ft)    #character
typeof(ft)  #character


ft <- factor(ft)
ft

table(ft) #f,m의 분할표 출력
?table()
class(ft) #factor
mode(ft) #numeric
typeof(ft)#numeric

## vertorization

x <- c(1, 3, 5, 7, 9)
x
sqrt(x)#루트
?sqrt

y <- c(2, 4, 6, 8, 10)
y

x+y

x+5

x*10


# recycling rule

x<-c(1, 2, 3, 4)
x

z <- c(2, 4)
z

x+z #z가 두 번 들어감

z <- c(2, 4, 6)
z

x+z   #계산은 됨. but 에러메시지. is not a multiple of ~ (배수가아니라는말)

## indexing

x <- c(1, 3, 5, 15, 3, 2)
x

as.vector(x)
x

x[1]

x[2]

x[c(1,3,5)]

x[1:3]

x[c(1,3,5)]

x[-1] #1번째 빼고 추출. 값 삭제아니다다

x[x>3]

x[x>3 & x<10]

x[x<=2 | x>10]

# intexing in Matrix 

x <- matrix(1:15, 3, 5)
x
x[1,3]

x[1,]
x[-2,1]
x[1:2, ]
x[c(1,3),]
x[x[,2]>5,] #행추출. 그 행은 2열의 값이 5보다 커야함

## NA, NaN, NULL 

x <- c(1, 2, NA, 10, 3)
is.na(x)
length(x)

x <- c(1, 2, NULL, NA, 4)
is.na(x)
is.nan(x)
length(x)
x[3]
x[4]
# NA vs NULL

x <- NULL
length(x)

y <- NA
length(y)

x <- c(88, 12, NA, 168, 13)
x

mean(x)
mean(x, na.rm=T)

x <- c(88, NULL, 12, 168, 13)
x

mean(x)

## File input/output
install.packages("xlsx")
library(xlsx)
file <- choose.files() #file open
popData <- read.xlsx(file, 1, encoding = "UTF-8")
head(popData)
class(popData)
str(popData) #데이터 구조확인


# 과제1
# area, sido, 2018년인구, 2010년 인구, 인구성장률을 계산 
# area는 factor로 변환하고, 1은 수도권, 2는 동남권 ... 라벨링을 함

# area, sido, pop10, pop18 컬럼만 선택함 
# 세종특별자치도의 row는 삭제
# 인구성장률을 계산함
# 해당 데이터셑의 이름을 popData.sido로 지정함

#과제2
#권역별 pop10과 pop18의 합을 계산함
#sqldf패키지 사용
#권역별 인구성장률 계산하고, 데이터셑 이름은 opData.area

# 데이터셑, R프로그램(반드시 주석달고), 인구성장과 관련된 데이터를 고민
## workspace 
getwd()
dir()

popData.area
popData.area1