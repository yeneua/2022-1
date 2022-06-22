#install.packages("xlsx")
#library(xlsx) => xlsx #xlsx패키지는 자바가 설치되어야 함
file <- choose.files() #file open #data.csv 파일 불러오기
#popData <- read.xlsx(file, 1, encoding = "UTF-8") => 엑셀 파일을 불러올 때
popData <- read.csv(file, header=T)
class(popData)
popData$area = factor(popData$area, levels = c(1:7),
                      labels = c("수도권","동남권","대경권","충청권","전라권","강원권","제주도")) #순서대로 이름 라벨링링 
popData


###########################################
# query 문을 지원함) sqldf
##########################################
install.packages("sqldf")
library(sqldf)
data(iris)	#내장데이터 불러오기
str(iris) #데이터구조 확인
sqldf("select Species
      from iris
      where Species='setosa'")
iris.sql = sqldf("select distinct Species from iris") #distinct 하면 중복 값은 제외하고 나옴
iris.sql
sqldf("select *
      from iris
      where Species='virginica'")
sqldf('select Species, sum("Sepal.Length") as SepalLength
      from iris
      group by Species')
sqldf('select Species, avg("Petal.Length") as avg, stdev("Petal.Length") as sd
      from iris
      group by Species')

#권역별 인구수
pop.area <- sqldf("select area, sum(pop10) as area10, sum(pop18) as area18
                  from popData
                  group by area")
tot10 <- sum(pop.area$area10)
tot18 <- sum(pop.area$area18)
pop.pnt10 <- pop.area$area10/tot10*100 #vector
pop.pnt18 <- pop.area$area18/tot18*100
pop.area1 <- cbind(pop.area,pop.pnt10,pop.pnt18)


##################################
## apply(), lapply(), sapply(), tapply(), by()
##################################


## apply()

# Compute row and column sums for a matrix:
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8] #행이름부여
x[[1]]
x

apply(x, 1, mean) #x를 행(1)별로 mean
apply(x, 2, mean)
apply(x, 2, sort) #열별로 오름차순 정렬
apply(x, 2, sort(decreasing = TRUE))
row.sums <- apply(x, 1, sum)
row.sums
col.sums <- apply(x, 2, sum)
col.sums

rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))

# Sort the columns of a matrix
apply(x, 2, sort)


## lapply(), sapply()

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
x

# compute the list mean for each list element
lapply(x, mean) #lapply : list 형태로 리턴

# median and quartiles for each list element
lapply(x, quantile, probs = 1:3/4)
sapply(x, quantile) #리턴값: vector, matrix
class(sapply(x, quantile)) #"matrix""array"

sapply(x, class)


### ????????? 몰겟어요 ㅠ ###
i39 <- sapply(3:9, seq)    # list of vectors
sapply(i39, fivenum)
vapply(i39, fivenum,
       c(Min. = 0, "1st Qu." = 0, Median = 0, "3rd Qu." = 0, Max. = 0))

## tapply()

ages <- c(25, 26, 55, 37, 21, 42)
affils <- c("R", "D", "D", "R", "U", "D")
tapply(ages, affils, mean) #ages를 affils분류 기준에 맞춰 mean

d <- data.frame(
  gender = c("M", "M", "F", "F", "M", "F"),
  age = c(23, 26, 25, 37, 55, 22),
  income = c(150, 300, 250, 350, 800, 120))
d

d$over25 <- ifelse(d$age > 25, 1, 0) #d에 over25변수 추가, 25이상이면 1
d

tapply(d$income, list(d$gender, d$over25), mean) #성별&over25별로 income평균
split(d$income, list(d$gender, d$over25))#성별&over25별로 나누기(split)



### ????????? 몰겟어요 ㅠ ###
# by()

head(warpbreaks)
levels(warpbreaks$tension)
str(warpbreaks)
summary(warpbreaks)
colnames(warpbreaks)

by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
by(warpbreaks[, 1],   warpbreaks[, -1],       summary)


## 인구 데이터를 이용하여 권역별 합을 계산 ###
?tapply

tapply(pop.area$area10, pop.area$area,sum)
class(tapply(pop.area$area10, pop.area$area,sum)) #array
tapply(pop.area$area18, pop.area$area,sum)

###########################################
# plyr 패키지 사용 split->apply->combine
###########################################
# dd? 
library(plyr) #메모리를 사용함
data("baseball") #데이터 불러오기기
class(baseball)
dim(baseball)
head(baseball)
str(baseball)
summary(baseball$sh)
quantile(baseball$bb)
result = ddply(baseball,  #data
               .(id),     #기준
               summarise, #
               avg_g=mean(g, na.rm=T)) #수식
head(result, 3)
class(result)  #data.frame
result1 = ddply(baseball, .(id), summarise, minG = min(g,na.rm=T ), maxG=max(g, na.rm=T)) #.(id)는 기준필드
head(result1, 3)
result2 = ddply(baseball, .(id, team), summarise, minG = min(g,na.rm=T ), maxG=max(g, na.rm=T)) #.(id)는 기준필드
# 기준이 여러개 있을 경우 .(id, team, ....), 요약
head(result2)
result3 = ddply(baseball, .(id), transform, minYear = min(year,na.rm=T )) #tranform은 요약이 아니라 뒤에 추가
result3[1, c(1,23)] #첫번째행을. 첫번째, 마지막 열 값 가져오기
dd<-ddply( baseball ,
           .( id ) ,
           subset , #subset : 분할(추출)
           g == max( g ) ) #게임수가 최대게임수와같으면 출력
dd
dim(dd)
dd1 <- ddply(baseball,
             .(id, team),
             subset,
             g == max(g))
dd1
colnames(baseball)
#id기준으로 g의 최대값 구하기


##################################################################인구 데이터를 이용하여 인구를 계산###########################################################
library(plyr)
pop <- ddply(popData, .(area), summarise, pop.a10 = sum(pop10, na.rm=T),pop.a18 = sum(pop18, na.rm=T))#,
             grdp.a10=sum(grdp10, na.rm=T),grdp.a18=sum(grdp18, na.rm=T))
pop

#pop.pcnt10<-round(pop$pop.a10/sum(pop$pop.a10)*100,1)
pop.pcnt10<-with(pop, round(pop.a10/sum(pop.a10)*100,1))
pop.pcnt18<-with(pop, round(pop.a18/sum(pop.a18)*100,1))

label1<-paste(pop$area, "(", pop.pcnt10, "%)")
l <- sprintf("%s ( %f )" ,pop$area, pop.pcnt10) #이렇게도 가능
label2<-paste(pop$area, "(", pop.pcnt18, "%)")
par(mfrow=c(1,2))

with(pop, pie(pop.a10, labels=label1, col=rainbow(length(area)), main="2010년도 권역별 인구비율"))
with(pop, pie(pop.a18, labels=label2, col=rainbow(length(area)), main="2018년도 권역별 인구비율"))
# pie(pop$pop.a10,label=label1,col=rainbow(length(pop$area))) # with안쓰고 -> $dollorsign해줘야함


#권역별 grdp
#권역별 grdp - (내가 한 거)
pop.gpcnt10<-with(pop, round(grdp.a10/sum(grdp.a10)*100,1))
pop.gpcnt18<-with(pop, round(grdp.a18/sum(grdp.a18)*100,1))
labell1<-paste(pop$area, "(", pop.gpcnt10, "%)")
labell2<-paste(pop$area, "(", pop.gpcnt18, "%)")
par(mfrow=c(1:2))
with(pop, pie(grdp.a10, labels=labell1, col=rainbow(length(area)), main="2010년도 권역별 grdp"))
with(pop, pie(grdp.a18, labels=labell2, col=rainbow(length(area)), main="2018년도 권역별 grdp"))
#권역별 grdp - (교수님)

par(mfrow=c(2,2))
with(pop, pie(grdp.a10, labels=labell1, col=rainbow(length(area)), main="2010년도 권역별 grdp"))
with(pop, pie(grdp.a18, labels=labell2, col=rainbow(length(area)), main="2018년도 권역 grdp"))
with(pop, pie(pop.a10, labels=label1, col=rainbow(length(area)), main="2010년도 권역별 인구비율"))
with(pop, pie(pop.a18, labels=label2, col=rainbow(length(area)), main="2018년도 권역별 인구비율"))




###########################################
# 자료의 형태를 바꿈 / reshape(melt, cast)
#############################################

library( reshape2 )
data(smiths)
smiths
m = melt( id =1:2 , smiths )	#데이터형태 변경하기 id=1:2 기준(1,2열)
m
x = dcast(m , subject + time~... ) #원래대로 전환. dcast() : dataframe으로 cast
x

##################################################
#data.table (속도를 높여줌)
##################################################
library(data.table)
DF = data.frame( x = runif(520000) , 
                 y = rep(LETTERS , each =20000) )
head(DF)
system.time(x<- DF[ DF$y == "C" , ])

DT = as.data.table( DF )
setkey( DT , y )
system.time( x <- DT[ J( "C" ) , ])

