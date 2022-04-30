########## 4-1 ##########

#아마 R을 복습하는 마지막 주
##### data frame indexing #####
?mtcars
head(mtcars) #행 이름 : 차종
str(mtcars)
dim(mtcars)
#datafrmame indexing- dataframe, list 형태의 인덱싱

?subset() #특정 조건을 만족하는 것을 리턴
#subset(데이터, 조건-논리식) , select를 이용해 가져오고자하는 열 선택
subset(mtcars, subset = (mpg > 30)) #mpg(연비)가 30이상인 것만
subset(mtcars, subset = (mpg > 30), select = mpg) #mpg(연비)가 30이상인 것 중 mpg 데이터만 가져옴
subset(mtcars, subset = (cyl == 4 & am == 1), select = c(mpg, hp, wt, am)) #mpg, hp, wt, am 열만
subset(mtcars, subset = (cyl == 4 & am == 1), select = c(mpg, hp, wt, am)) #mpg, hp, wt, am 열만 빼고

subset(mtcars, subset = (mpg < mean(mpg)), select = mpg) #mpg가 평균이하인 것만 추출

### 데이터 간 상관관계
?USArrests #violent crime rates by US state
head(USArrests)
str(USArrests)
subset(USArrests, select = -UrbanPop) #인구 데이터 제외
#상관관계 -> cor()
?cor()
cor(subset(USArrests, select = -UrbanPop)) # => Murder 와 Assault의 상관관계는 높다
subset(USArrests,select = -c(UrbanPop, Rape))
head(subset(USArrests,select = -c(UrbanPop, Rape)))
cor(subset(USArrests,select = -c(UrbanPop, Rape)))


##### tibble #####
#data frame 중 하나의 특수한 형태
#데이터프레임보다 표현이 조금 더 간결하고, 대용량 데이터 셋 처리에 용이
#데이터프레임이 가지고 있는 특성들 그대로. 호환해서 사용 가능
#tibble(), tribble(), as_tibble(), print()
#as.data.frame()을 데이터 프레임을 만드는 것처럼 tibble()을 이용해 tibble 을 만듦

install.packages("tibble")
library(tibble)

v1 <- c("A001", "A002", "A003")
v2 <- c("Mouse", "Keyboard", "USB")
v3 <- c(30000, 35000, 40000)
product <- tibble(id = v1, name = v2, price = v3)
str(product)
#data frame 과 가장 다른 점은 tibble은 각각의 열의 data type을 설명함. 데이터프레임과 유사
a <- as.data.frame(v1, v2,v3)

#tribble -> 직관적으로 만들 수 있다. 대용량 데이터에는 부적합. 적은 형태의 데이터
tribble( 
  ~id, ~name, ~price,
  "A001", "Mouse", 30000,
  "A002", "Keyboard", 35000,
  "A003", "USB", 40000
)

#tibble -> 하나의 열 안에 또 다시 list 형태의 구조를 가질 수 있다.
tb <- tibble(id = c(1,2,3),
       data=list(tibble(x=1, y=2),
                 tibble(x=4:5, y=6:7),
                 tibble(x=10)))
#dataset -> tibble 형태
#data 열에는 list 형태의 데이터가 들어가 있다
tb
tb$data
tb$data[[2]]

str(iris)
iris
head(iris)

as_tibble(iris)
#tibble - 10개 행, 화면에 맞게 열을 보여줌. 나머지는 요약해서 보여줌-> 직관적

as_tibble(mtcars)

install.packages("Lahman")
library(Lahman)
str(Batting)
head(Batting) #메이저리그. 타자들의 주요 지표들
?Batting

Batting
head(Batting) #변수가 많아서 한눈에 파악하기 쉽지 않다
Batting.tbl <- as_tibble(Batting)
Batting.tbl
#행, 열이 많은 데이터 셋을 탐색할 때, tibble 형태로 전환해서 보면 human-friendly한 분석 가능



########## 4-2 ##########

# Apply 계열의 함수

?apply() #apply(데이터, margin, fun) #matrix일 때, margin 에서 1->행, 2->열
x <- matrix(1:20, 4, 5)
x

#apply(데이터, 단위, 반복적으로 수행할 함수) matrix, array 형태 둘 다 접근 가능
apply(X = x, MARGIN = 1, FUN = max) #동일한 작업(max값을 찾는)을 행단위(1)로 반복. 1:행, 2:열
apply(X = x, MARGIN = 2, FUN = max) #min 값을 찾는 작업을 열 단위로 반복
apply(X = x, MARGIN = 2, FUN = min)
apply(X = x, MARGIN = 1, FUN = mean)
apply(X = x, MARGIN = 1, FUN = sum)

y <- array(1:24, c(4,3,2)) #4행 3열 2개의 차원
y

apply(y, 1, paste, collapse=",") #각각의 차원에서 첫번째 행을 paste 형태로 중간에 콤마를 써서 붙임 => 4개의 행
apply(y, 2, paste, collapse=",") #각각의 차원에서 첫번째 열을 paste 형태로 중간에 콤마를 써서 붙임 => 3개의 열

a <- c(1,5,9,13,17,21)
a

paste(a)#numeric을 각각의 문자열로 합쳐주는 함수
class(paste(a)) 

paste(a, collapse = ",") #각각의 구분을 없애고 하나로 합치면서 구분자를 콤마로

apply(y, 2, paste, collapse=",") #열에 대한 작업

apply(y, 3, paste, collapse=",") #두가지 차원으로 구분. array니까
#여러가지 차원의 배열 -> 각각의 차원에 대한 작업을 반복적으로 수행 : MARGIN = 3

apply(y, c(1,2), paste, collapse=",") #c(1,2) : 각각의 쌍

apply(y,1,sum)

Titanic #4가지 차원. 배열 형태의 데이터
?Titanic
str(Titanic)

apply(Titanic, 1, sum) #각각의 등급(class)별로 sum
apply(Titanic, 4,sum) #4번째 차원 : Survived
apply(Titanic, 2, sum)

apply(Titanic, "Class", sum) #이름이 지정되어 있으니 차원의 이름으로 indexing 가능

apply(Titanic, c(1,4), sum) #두가지 차원 한번에
apply(Titanic, c(1,2,4), sum)
apply(Titanic, -c(1,4), sum)


lapply() # list 구조에 적용 -> list 형태로 return
sapply() # 최대한 간편화해서 제공해줄수 있는 데이터 타입을 자동으로 지정해 return
#둘 다 list구조에 적용, 수행하는 함수도 동일 => 결과를 return하는 방법이 다르다

exams <- list(Spring_2020 = c(78,60,89,90,96,54),
              Spring_2021 = c(85,78,69,90,95),
              Spring_2022 = c(98,96,9,489,99,100,87),
              Spring_2023 = c(86,98,76,89,57,79))
exams #매년 수강생이 다르기 때문에 다른 숫자의 벡터들 -> 리스트가 적합

#lapply(list, function)
lapply(exams, length) #길이 측정. list형태
sapply(exams, length) #vector 형태

lapply(exams, mean)
sapply(exams, mean)

lapply(exams, sd)
sapply(exams, sd) #표준편차

sapply(exams, range) #range: 최대, 최소
class(sapply(exams, range)) #두개의 데이터 -> matrix 형태
lapply(exams, range) #list

?iris #분꽃데이터
head(iris)
str(iris)

lapply(iris, class) #각각의 열들의 type을 알고 싶다
sapply(iris, class)
sapply(iris, mean) #Warning message. Species 는 요인형이기 때문에 계산X  

#내장된 함수가 아닌 적용되는 함수 내가 만들수도 있음
sapply(iris, function(x) ifelse(is.numeric(x), mean(x), NA)) #인자가 numeric이면 평균 구하고, 아니면 NA => warning message 안나옴