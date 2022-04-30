#2017447 김예나
#3주차

### 복습 ###
product <- list(id="A001", name = "Mouse", price=30000) 
product
#원소값 바꾸기
product[[3]] <- 40000 #인덱싱으로 변경
product
product[["price"]] <- 50000 #이름으로 바꾸기
product
product$price <- 60000 # $
product

product[3] <- 70000 #원소가 하나만 있기 때문에 대괄호 두 개 안해도 값 바뀜
product

product[[3]] <- c(30000,40000) #값 두개
product

product[3] <- 70000 #두개였던 값이 70000 하나가 됨
product

product[3] <- list(c(50000,60000)) #대괄호호하나 : 리스트에 접근
product


#외부에 vector 형태로 데이터 저장. 불러들여서 list로 만드는
names <- c("Mon", "Tue","Wed","Thur","Fri","Sat","Sun")
values <- c(842, 729, 786, 844, 851, 750, 702)

traffic.death <- list() #빈리스트 생성
traffic.death #빈리스트 생성 확인

traffic.death[names] <- values #names를 요소로 쓰고, 요소에 들어갈 값을 value로 할당
traffic.death #각각 요일별 교통사고 발생 빈도 list

traffic.death < 750 #traffic.death가 750이하인 것

traffic.death[traffic.death < 750] <- NULL # [traffic.death < 750]이 참이었던 것을 NULL값으로 처리 => 인덱스 자체가 없어짐. 결측치 값으로 나오는 게 아니다
traffic.death


### data.frame ###
name <- c("KIM","LEE","HAN","SHIN","PARK")
gender <- as.factor(c("M","F","M","M","F"))
math <- c(100,80,75,95,65)
english <- c(80,100,60,70,80)
korean <- c(50,70,80,100,100)
attend <- c(T,F,T,T,F)
a <- data.frame(name,gender,math,english,korean,attend, stringsAsFactors=T)
a #stringsAsFactors = F -> data.frame()으로 데이터프레임 생성할 때, 변수에 문자가 있는 경우 자동으로factor타입으로 변환됨. factor는 연산이 되지 않으므로 stringsAsFactors()함수를 써서 factor타입으로 변환되지 않게 함

name <- c("JANG","JEONG")
gender <- as.factor(c("M","F"))
math <- c(100,80)
english <- c(80,100)
korean <- c(50,70)
attend <- c(T,F)
b <- data.frame(name,gender,math,english,korean,attend)
b
c <- rbind(a,b) #두개의 dataframe이 구조가 같아야함
c

social <- c(100,80,50,65,70)
science <- c(80,55,75,60,100)
d <- cbind(a,social,science)
d

e <- transform(a,social=c(100,80,50,65,70), science= c(80,55,75,60,100))
e

name <- c("KIM","LEE","HAN","SHIN","PARK")
japanese <- c(100,60,70,20,50)
f <- data.frame(name,japanese)
f
g <- merge(a,f,key="name") #key를 이용. 결측치 처리시 장점
g

#추출
a$name
a[,1:2] #1,2열
a[2:3,] #2,3행
a[2:3, 1:2] #2~3행, 1~2열
a[-1,] #1행빼고

a[a$math >= 70,] #math가 70이상인 행
a[a$name == "KIM",]#name이 KIM인 행
a[a$english<90,]#english가 90미만인 행
a[a$math >= 70 & a$korean >= 70,] #math와 korean 둘 다 70점 이상인 행(and)
a[a$math < 90 | a$korean < 90,] #math가 90미만이거나 korean이 90미만인 행(or)

h <- subset(a, select=c(name,math,korean), #열이름
            subset = (math >= 50 & korean >= 70))  #조건
h #데이터프레임 a에서 math가 50이상, korean이 70이상인 데이터 중에서 name,math,korean 열만 추출출
#subset(데이터프레임,
    #select=c(열번호 or 제목), subset=(조건))


### hospital - new born baby ###
df1 <- data.frame(sex="female", month=1,weight = 3.5)
df2 <- data.frame(sex="male", month=3,weight = 4.8)
df3 <- data.frame(sex="male", month=4,weight=5.3)
df4 <- data.frame(sex="female", month=9,weight=7.5)
df5 <- data.frame(sex="female", month=7,weight=8.3)

lst <- list(df1,df2,df3,df4,df5)

str(lst)#리스트 구조

#list에서 데이터를 뽑아와서 하나의 datafrmae으로
lst[[1]]
lst[[2]]
rbind(lst[[1]],lst[[2]]) #각각의 row가 하나로 합쳐진 형태

a <- do.call(rbind, lst) #내가 활용할 함수, 어디에 있는 것을 rbind에 적용할 건지 - for문을 돌린다고 생각

#dataframe이 아니라 list형태로 데이터가 구성되어있다고 가정
lst1 <- list(sex="female", month=1,weight = 3.5)
lst2 <- list(sex="male", month=3,weight = 4.8)
lst3 <- list(sex="male", month=4,weight=5.3)
lst4 <- list(sex="female", month=9,weight=7.5)
lst5 <- list(sex="female", month=7,weight=8.3)
lst <- list(lst1, lst2,lst3,lst4,lst5) #list안의 list -> 이중list
lst #5개의 list주머니 안에 각각의 3개의 주머니가 생김. 또 그 안에 값이 저장되어있음음
str(lst) #List

lst[[1]] #list형태가 반환됨
lst[[1]][2]

#list -> dataframe => as.data.frame() 
as.data.frame(lst[[1]])

do.call()
#as.data.frame()을 계속 반복적으로 수행해서 요소들을 뽑아내는 작업 -> lapply()
#lapply(list, function)
lapply(lst, as.data.frame) #list데이터들을 뽑아서 as.data.frame 적용
do.call(rbind,lapply(lst,as.data.frame)) #원소들을 뽑아서 하나의 dataframe으로 만들어줌



### 3-2 ###

?state
state.abb
state.area
state.name
state.region

#vector형태로 제공되는 데이터 -> dataframe 만들기
us.state <- data.frame(state.abb, state.name, state.region,
                       state.area, stringsAsFactors = FALSE)
us.state
str(us.state) #data.frame : 50 obs. of 4 variables

#리스트 인덱싱 방법 - 대괄호1개, 대괄호2개(원소를 그대로 vector형태로 가져옴)
us.state[[2]] #state.name
str(us.state[[2]]) #문자형 vector

us.state[2]
str(us.state[2]) #data.frame
us.state[c(2,4)] #state.name, state.area

#matrix 인덱싱 방법
us.state[,2] #열만 
us.state[2,] #행만
us.state[,2,drop=FALSE] #data.frame의 정렬방식 그대로
us.state[,c(2,4)]

#열 이름이 정의되어있을때
us.state[["state.name"]]
us.state$state.name
us.state["state.name"]
us.state[,"state.name"]

us.state[c("state.name","state.area")] #리스트 형태의 인덱싱
us.state[,c("state.name","state.area")] #matrix 형태의 인덱싱 방법#모든 것을 다 가져올때는 쉼표 생략가능


#condition based indexing

state.x77 #내장된데이터
str(state.x77)
head(state.x77)
#행의 이름을 열로 바꿀 것
#matrix -> dataframe
states <- data.frame(state.x77)
states
str(states)

row.names(states)

states$Name <- row.names(states) #Name이라는 파생변수생성 -> 원래있던 행이름 필요없으니까 지워주자
head(states)

row.names(states) <- NULL #행이름 없애기
head(states)


rich.states <- states[states$Income >= 5000, c("Name","Income")] #income이 $5000이상인 주의 이름과 income 열 가져오기
rich.states

#area가 넓은 주
large.states <- states[states$Area >= 100000, c("Name","Area")]
large.states

?merge
#merge two dataframes by common columns, JOIN operations. 기준 key 있어야한다
merge(rich.states, large.states, all=FALSE, key="Name") #all=FALSE : 교집합(default값) inner join
merge(rich.states, large.states, all=TRUE) #합집합. outer join
merge(rich.states, large.states, all.x=TRUE) #left join. logical !! T,F로 표시
merge(rich.states, large.states, all.y=TRUE) #outer join
