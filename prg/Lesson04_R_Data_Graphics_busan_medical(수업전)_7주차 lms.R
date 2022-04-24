#barplot 이용하기
if(!require("xlsx")) { #엑셀 파일을 불러오기 위해 필요한 패키지
  install.packages("xlsx") 
  require(xlsx)
}
if(!require("reshape2")) {  #데이터의 형태를 변환할 때. melt(),cast()
  install.packages("reshape2") 
  require(reshape2)
}
if(!require("dplyr")) { #그룹별집계
  install.packages("dplyr") 
  require(dplyr)
}
if(!require("RColorBrewer")) {    #색상 
  install.packages("RColorBrewer") 
  require(RColorBrewer)
}
#버지니어 사망률 그래프
graphics.off()
VADeaths #사망률 그래프 -> 2차분류표(도시, 시골별, 연령 남녀사망률) - 비율
# 누적막대그래프
barplot(VADeaths, col = heat.colors(5), #color속성 - 연령.  x축 - 열, y축 - 퍼센트(비율)
        border = "dark blue", legend = rownames(VADeaths)) #범례 - 연령(rownames)
title(main = list("버지니아주 사망율", font = 2)) #title 추가

#묶음 막대 그래프
barplot(VADeaths, col = heat.colors(5),
        border = "dark blue", legend = rownames(VADeaths),
        beside = T) #누적 막대 그래프에 beside=T만 추가해주면 됨
title(main = list("버지니아주 사망율", font = 2)) #title 추가


#부산 서구 - 부산 의료 관광 특구 => 의료데이터( 종합병원 외에 전문병원 들. . 부산시에 의료 기관 ..)
#인구가 증가하는 곳이 생활여건이 좋은지 ? .. 의료기관 수(복지지표 - 10만명당 병원수)

## 부산 병원 현황비교(2010년 vs 2022년)
rm(list=ls()) #dataset 지우기
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk-14.0.2') #엑셀파일을 읽기 위해
Sys.setenv(JAVA_HOME='C:\\Program Files\\OpenJDK\\openjdk-8u302-b08')
file=file.choose() #medical_busan.xlsx 파일 불러오기
data10 = read.xlsx(file, sheetIndex = 1, header=T, encoding = "UTF-8") #sheet 번호를 인덱스로 줌(엑셀파일)
data20 = read.xlsx(file, sheetIndex = 2, header=T, encoding = "UTF-8")
data10 = read.csv(file,header=T)

install.packages("readxl")
library(readxl)
data10 <- read_excel("medical_busan.xlsx", sheet = '데이터10') #16 obs, 25 var
data20 <- read_excel("medical_busan.xlsx", sheet = '데이터20') #16 obs, 27 var

# reshape2에서 제공하는 변환은 크게 melt(wide to long), cast(long to aggregate)
d10 <- melt(id=1, data10) #id=1(기준 : 1 - 시도별). 시도를 기준, 나머지는 변수들은 전부 값으로.
# 2020년 데이터를 구별로 long하에 데이터 변환
d20 <- melt(id=1, data20)

#전체 병원수 확인
sum.d10 <- d10  %>%
  group_by(sido) %>%
  summarise(count10 = sum(value))

#bar chart 작성
barplot(sum.d10$count10,
        main=paste("2010년 부산시 주요구별 과목별 병원현황"), #title
        ylab="병원수", #빈도수
        ylim=c(0,400), #y축 범위
        col=brewer.pal(16,"Set3"), 
        names=sum.d10$sido, #x축 label이름
        cex.names=0.6)
abline(h=seq(0,400,50),lty=3) #선 그리기(선종류 : 3)
#=> 진구>해운대수> ... >강서구
#but, 강서구는 인구가 늘어나고 있다.

#정렬해서 보여줌(직) - 빈도 순서대로 정렬하는 것이 좋다 .=> 직관적으로 !!
data<-sum.d10$count10
names(data)<-sum.d10$sido
barplot(sort(data, decreasing = T), #데이터정렬(내림차순)
        main=paste("부산시 주요구별 과목별 병원현황"),
        ylab="병원수", #빈도
        ylim=c(0,400),
        col=cm.colors(16), 
        cex.names=0.6)
abline(h=seq(0,400,50),lty=3, lwd=0.1) #

#2010년과 2020년도 병원현황 비교
#2020년 데이터 
sum.d20 <- d20  %>%
  group_by(sido) %>%
  summarise(count20 = sum(value))

data.t <- data.frame(sum.d20$sido, sum.d10$count10, sum.d20$count20)
with(data.t,
     barplot(cbind(sum.d10.count10,sum.d20.count20)~sum.d20.sido, 
             main=paste("2010과 2020년 부산시 주요구별 과목별 병원현황"),
             beside = T,
             ylab="병원수",
             xlab="구별",
             ylim=c(0,450),
             col=c("red", "blue"), 
             names=sum.d20.sido, cex.names=0.6))

graphics.off()

#모자이크 그림 - 비율을 나타내줌
#데이터 변환과정입니다.

#0이 많은 변수는 지움. 제외함
dd10<-subset(d10,
             variable !='진단검사의학과' & variable !='핵의학과'  & variable !='병리과' & variable !='결핵과' & variable !='흉부외과'
             & variable !='재활의학과' & variable !='신경과'  & variable !='영상의학과' & variable !='가정의학과' & variable !='신경외과' )
c <- dd10$value
dim(c) <- c(16,14) #16개 구. 진료과목 14개. => 행 : 구
rownames(c) <- dd10$sido[1:16] #행이름 부여(sido)
colnames(c) <- unique(dd10$variable) #열 이름 부여
c
mosaicplot(c, color=rainbow(14), main="부산 지역별 진료과목 현황")
c1<-t(c) #행과 열을 바꿔줌
mosaicplot(c1, color=rainbow(14), cex=1, main="2010년 부산 지역별 진료과목 현황")
#폭이 넓다 => 많다는 것


dd20<-subset(d20,
             variable !='진단검사의학과' & variable !='핵의학과'  & variable !='병리과' & variable !='결핵과' & variable !='흉부외과'
             & variable !='재활의학과' & variable !='신경과'  & variable !='영상의학과' & variable !='가정의학과' & variable !='신경외과'
             & variable !='직업환경의학과'& variable !='예방의학과' )
d <- dd20$value
dim(d) <- c(16,14) #16개 구. 진료과목 14개. => 행 : 구
rownames(d) <- dd20$sido[1:16] #행이름 부여(sido)
colnames(d) <- unique(dd20$variable) #열 이름 부여
d
mosaicplot(d, color=topo.colors(14), main="부산 지역별 진료과목 현황")
d1<-t(d) #행과 열을 바꿔줌
mosaicplot(d1, color=topo.colors(14), cex=1, main="2020년 부산 지역별 진료과목 현황")


library(readxl)
data21 <- read_excel("시군구별_표시과목별_의원_현황_20220424223414.xlsx", sheet = '21')

dd21 <- melt(id=1, data21)
dd21<-subset(d21,
             variable !='진단검사의학과' & variable !='핵의학과'  & variable !='병리과' & variable !='결핵과' & variable !='흉부외과'
             & variable !='재활의학과' & variable !='신경과'  & variable !='영상의학과' & variable !='가정의학과' & variable !='신경외과' )
e <- dd21$value
dim(e) <- c(16,14) 
rownames(e) <- dd21$sido[1:16]
colnames(e) <- unique(dd21$variable)
e
mosaicplot(e, color=rainbow(14), main="부산 지역별 진료과목 현황")
e1<-t(e)
mosaicplot(e1, color=terrain.colors(14), cex=1, main="2021년 부산 지역별 진료과목 현황")


