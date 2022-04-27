### Lotte Data #####
### Loading and installing packages ###
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(sqldf)){
  install.packages("sqldf")
  library(sqldf)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(lubridate)){ #날짜 데이터를 변환할 때 쓰는 패키지
  install.packages("lubridate")
  library(lubridate)
}
if(!require(dplyr)){ #집계할 때 쓰는 패키지
  install.packages("dplyr")
  library(dplyr)
}
### Define analysis data ####
file=choose.files() #1_1.DEMO 파일 불러오기
customer <- read.table(file, header=T, sep=",") #텍스트파일 table로 읽어옴. 구분자 콤마
file1 = choose.files() #1_2.구매내역정보 파일 불러오기
purchaseList <- read.table(file1, header=T, sep=",")

#사용할 데이터 : 고객id, 성별, 연령, 거래일자, 상품분류(대중), 거래식별id, 구매금액, 점포 id -> sqldf 이용

### 테이블 조인
tb <- sqldf("select A.id, A.성별, A.연령, B.거래일자, B.상품대분류명, B.상품중분류명, B.구매건수, B.거래식별id, B.구매금액, B.점포id
            from customer as A,
            purchaseList as B
            where a.id=b.id")
head(tb)
colnames(tb)


#월별로 판매되는 상품이 달라질 수도 있잖 ? ... 등등 변화요소 많음
#문자로 되어있는 데이터를 숫자로 바꾸자 -> '월'만 가져오기 : lubridate 패키지이용


## Create date field ##
####### lubridate 패키지 이용 ###
tb$거래일자 <- ymd(tb$거래일자) #y:year, m:month, d:date. ymd로 변환해서 거래일자에 넣기
class(tb$거래일자) #Date로 데이터형이 바뀜.(<-character)
tb$거래월 <- month(tb$거래일자) #월만 발췌. month함수 이용

# 원하는 데이터가 다 만들어졌다. 분석만 하믄됨
# sql 쿼리문 알아두기 !! dplyr로 조인하는 방법도 있지만 쿼리문 사용하자 ~

#점포별 매출현황, 품목별 매출현황, 월별 매출현황 ... 확인해보려고 한다.
# 집계 => dplyr


### Data exploration
# group : 월, 상품대분류, id, 구매금액, 구매 건수
s1 <- tb %>% 
  group_by(거래월, 상품대분류명, 점포ID) %>% 
  summarise(amount = sum(구매금액), cnt = sum(구매건수))
class(purchaseList$구매금액)
class(purchaseList$구매건수)

s2 <- tb %>% 
  group_by(거래월, 상품대분류명, 점포ID) %>% 
  summarise(amount = sum(round(구매금액/1000,0)), cnt = sum(구매건수))

?boxplot
boxplot(s1$amount ~s1$거래월)
# => 중앙값은 거의 비슷한 듯 ?. 근데 10월 머임 ?? 왜 그럴까? - 10월데이터가 일부만 있어서. 10월 빼고 분석 고고
boxplot(s1$amount~s1$점포ID)
boxplot(s1$cnt~s1$점포ID)


load(file="data/s1.rda")
load(file="data/tb.rda")
head(s1)
head(tb)

#10월 데이터 없앨것
s1 <- subset(s1,거래월 != 10) #거래월이 10월이 아닌 것
boxplot(s1$amount ~s1$거래월)

# 0. ggplot2 라이브러리 불러오기
library(ggplot2)

# 1. x,y축 무슨 데이터 쓸건지. 데이터 지정
g <- ggplot(s1, #데이터 : s1
            aes(점포ID, amount, fill = 점포ID)) #x:점포ID, y:amount. fill(채우기)

#ggplot : layer 단위, +로 계속 추가
g <- g+geom_boxplot()+
  scale_fill_brewer(palette = "Set3")+
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size = 3, fill = "red")+  #=>평균이 중앙값보다 높다. CC - outlier때문에 평균 높다 
  stat_boxplot(geom = "errorbar")+
  labs(title = "롯데 매장 매출액 Boxplot",
       subtitle = "매출액 vs 점포명",
       x = "점포명", y = "매출액",
       caption = "Source : 롯데데이터, red = 평균")
g
# => CC outlier가 조금 있다. AA가 매출 제일 많다.

# s1 데이터 확인
# 상품 대분류별로 그림그리기
(g1 <- ggplot(s1, aes(상품대분류명, amount))+
  geom_point(aes(col = 점포ID, size = cnt))+
  labs(title = "Bubble Chart", subtitle = "점포별 : 품목 vs 매출액",
       x = "품목", y = "매출"))

#동그라미가 왜 여러개 ?? -> 왜 그럴까 ? 
# => 월별로 데이터가 다 들어가서 그럼

s2 <- tb %>%  #s1은 월별, 점포와 다 섞인 데이터
  group_by(상품대분류명, 점포ID) %>% 
  summarise(amount = sum(round(구매금액/1000,0)), cnt = sum(구매건수))

(g1 <- ggplot(s2, aes(상품대분류명, amount))+
    geom_point(aes(col = 점포ID, size = cnt))+ #size:구매건수
    labs(title = "Bubble Chart", subtitle = "점포별 : 품목 vs 매출액",
         x = "품목", y = "매출"))
# => 축산; AA와 CC가 거래건수는 비슷한데 거래금액이 차이가 많이 난다. AA가 매출이 가장 높다.
# => 낙농; AA의 매출, 거래건수가 높다.

#다양한 속성을 가지고 그림을 그림. 시각 속성을 활용해 많은 정보 표시


