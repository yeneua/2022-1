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
tb$거래일자 <- ymd(tb$거래일자) #y:year, m:month, d:date. ymd로 변환해서 거래일자에 넣기 20140410 -> "2014-04-10"
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
boxplot(s1$amount ~s1$거래월) #월 그룹별로 amount
# => 중앙값은 거의 비슷한 듯 ?. 근데 10월 머임 ?? 왜 그럴까? - 10월데이터가 일부만 있어서. 10월 빼고 분석 고고
boxplot(s1$amount~s1$점포ID) #점포별로 amount 
boxplot(s1$cnt~s1$점포ID) #점포별로 cnt(구매건수)


load(file="../Lotte_Project/data/s1.rda")
load(file="../Lotte_Project/data/tb.rda")
head(s1)
head(tb)
getwd()

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
(g1 <- ggplot(s1, aes(상품대분류명, amount))+ #data:s1, x축:상품대분류명, y축:amount
  geom_point(aes(col = 점포ID, size = cnt))+ #point그래프
  labs(title = "Bubble Chart", subtitle = "점포별 : 품목 vs 매출액",
       x = "품목", y = "매출"))

#동그라미가 왜 여러개 ?? -> 왜 그럴까 ? 
# => 월별로 데이터가 다 들어가서 그럼

s2 <- tb %>%  #s1은 월별, 점포와 다 섞인 데이터
  group_by(상품대분류명, 점포ID) %>% 
  summarise(amount = sum(round(구매금액/1000,0)), cnt = sum(구매건수))

sqldf("select distinct 상품대분류명 from s1") #14개

(g1 <- ggplot(s2, aes(상품대분류명, amount))+
    geom_point(aes(col = 점포ID, size = cnt))+ #size:구매건수
    labs(title = "Bubble Chart", subtitle = "점포별 : 품목 vs 매출액",
         x = "품목", y = "매출"))
# => 축산; AA와 CC가 거래건수는 비슷한데 거래금액이 차이가 많이 난다. AA가 매출이 가장 높다.
# => 낙농; AA의 매출, 거래건수가 높다.

#다양한 속성을 가지고 그림을 그림. 시각 속성을 활용해 많은 정보 표시

#점포별로 월별로 차이가있는지
head(s1)
g2 <- ggplot(s1, aes(거래월, 점포ID)) +
  geom_tile(aes(fill = amount)) + #geom_tile :3 variables -거래월,점포id,amount
  scale_fill_gradientn(colors = brewer.pal(n = 5, name = "RdBu")) #gradientn : 색깔의 숫자 정함
g2

#품목도 궁금 ?
g3 <- ggplot(s1, aes(거래월, 상품대분류명)) +
  geom_tile(aes(fill = amount)) + #fill을 amount, cnt로 바꿔서 확인 바꾸기
  facet_wrap(~점포ID) +  #점포ID별로 그래프 모두 확인
  scale_fill_gradientn(colors = brewer.pal(n = 5, name = "RdBu")) #gradientn : 색깔의 숫자 정함
g3

#구매내역가지고만 분석을 해옴. RFM(최근성,빈도,금액)

### 고객별 분석(RFM) <- 10주차 대면수업 소스파일 복붙함
if (!require(didrooRFM)){
  install.packages("didrooRFM")
  require(didrooRFM)
}
?findRFM
customerData <- subset(tb, select=c(거래식별ID,ID, 거래일자,구매금액)) #findRFM에 맞게 데이터셋 만들어줌
result <- findRFM(customerData, 4, 3, 3) #가중치 부여
result$MonetoryScore
result$RecencyScore
result$FrequencyScore
result$FinalScore
result$FinalCustomerClass
table(result$FinalCustomerClass)
(d<-result %>%
    group_by(FinalCustomerClass) %>% # class별로 그룹핑
    summarise(frq = n(), # class별로 몇명인지
              sale = mean(MeanValue), # MeanValue의 평균
              score = mean(FinalScore))) # FinalScore의 평균

(g <- ggplot(result, aes(x=FinalCustomerClass, y=MeanValue*NoTransaction)) + 
    geom_boxplot(aes(size=NoTransaction))+
    labs(title="Bubble Chart", subtitle="점포별 : 품목 vs 매출액",
         x = "품목", y="매출액"))

#sub3 <-aggregate(x=round(tb$구매금액/1000,0), by=list(catDate=as.character(tb$거래월), catStore=tb$점포ID, ID=tb$ID), FUN=sum)
s3<-tb %>% 
  group_by(ID, 점포ID)  %>% #ID:고객ID
  summarise(amount=sum(round(구매금액/1000,0)), cnt=sum(구매건수))
data <- sqldf("select a.*, b.MeanValue, b.NoTransaction, b.FinalCustomerClass
                from s3 as a, result as b 
                where a.ID = b.CustomerID")

table(data$FinalCustomerClass, data$점포ID) #FinalCustomerClass, 점포ID 빈도표
g <- ggplot(data, aes(점포ID)) +
  geom_bar(aes(fill = FinalCustomerClass), position = position_stack(reverse = TRUE))

g

(g <- ggplot(data, aes(x=FinalCustomerClass, y=amount)) +
    geom_point(aes(col=점포ID)) +
    labs(title="Bubble Chart", subtitle="점포별 : 품목 vs 매출액",
         x = "품목", y="매출액")) # 점포 여러개 그대로 나옴

temp<- data %>% group_by(점포ID, FinalCustomerClass) %>% # 점포별 고객등급
  summarize(amount=mean(amount), cnt=mean(cnt))

(g <- ggplot(temp, aes(x=점포ID, y=amount)) + 
    geom_point(aes(col=FinalCustomerClass, size=cnt)) +
    labs(title="Bubble Chart", subtitle="점포별 : 품목 vs 매출액",
         x = "품목", y="매출액"))


#RFM으로 고객을 분류한 데이터를 불러옴
load(file="../Lotte_Project/data/data.rda")
table(data$FinalCustomerClass, data$점포ID)
# => AA : 거래고객多. class-5가 vip고객
# => 결과를 보고 어떤 그래프를 그리면 좋을까 ?

a <- ggplot(data, aes(점포ID)) +
  geom_bar(aes(fill = FinalCustomerClass),
           position = position_stack(reverse =TRUE)) #class-5인 사람을 제일 위로
a

g <- ggplot(data, aes(FinalCustomerClass, amount)) +
  geom_point(aes(col = 점포ID))
g
#=>grouping 필요
#temp활용한 plot확인(위에있음)
# ㄴ=> class별로 sum이 아니라 평균(class마다 사람수가 다르기때문에)


# 다음 시간 - 지역별로 인구 변화, 산업집적도(지역-산업) 볼 것. 지리정보활용
# 서울 - 서비스업↑
# 경기도 - 제조업(반도체, IT)
# 등등 ... 이런것이 인구이동에 얼마나 영향을 주는지
# 부동산가격 - 인구 증가 인동의 요인임



## 10주차 과제

lt <- sqldf("select A.성별, A.연령, A.거주지역, B.*
            from customer as A,
            purchaseList as B
            where a.id=b.id")

lt1 <- sqldf("select ID, 거래시간대, 구매금액, 구매건수
             from lt
             group by 거래시간대")

lt_time <- lt %>% group_by(거래시간대) %>%
  summarize(amount=sum(구매금액))

l1 <- ggplot(lt_time, aes(거래시간대,amount/10000, fill=거래시간대))
l1 + geom_bar(stat="identity") +
  labs(title="시간대별 총 매출액",
    x="시간",
    y="총 매출액 (단위:만원)") + 
  theme(legend.position = "none")


l2 <- ggplot(tb, aes(상품대분류명, 구매금액/10000, fill=상품대분류명))
l2<- l2 + geom_boxplot() +
  labs(title="상품 종류별 총 매출",
       x="상품대분류명",
       y="매출액(단위:만원)") 
l2

lt3 <- lt %>% group_by(점포ID,거래시간대) %>% summarise(amount=sum(구매금액))
lt3 <- sqldf("select 점포ID, 거래시간대, sum(구매금액) as amount, count(ID) as cnt
             from lt
             group by 점포ID, 거래시간대")
(l3 <- ggplot(lt3, aes(거래시간대,amount/10000))+
  geom_point(aes(col=점포ID, size=cnt))+
  scale_color_brewer(palette = "Dark2")+
  labs(title="거래시간대별 점포 매출액", x="거래시간대", y="매출액 (단위:만원)"))

ggMarginal(l3, type="boxplot")



temp<- data %>% group_by(점포ID, FinalCustomerClass) %>% 
  summarize(amount=mean(amount), cnt=mean(cnt))
(g <- ggplot(temp, aes(x=점포ID, y=amount)) + 
    geom_point(aes(col=FinalCustomerClass, size=cnt)) +
    labs(title="Bubble Chart", subtitle="점포별 : 품목 vs 매출액",
         x = "품목", y="매출액"))



g <- ggplot(s1, aes(거래월,amount))
g + geom_boxplot()



boxplot(s1$amount ~s1$거래월)

g <- ggplot(tb, aes(점포ID, 구매금액))
g + geom_boxplot() +  #분포형태 확인가능
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw()


+ 
  geom_smooth(method="lm",se=T)+#선형
  labs(title="g")
g




l3 <- ggplot(tb, aes(상품대분류명, 구매금액/10000, fill=상품대분류명))
l3<- l3 + geom_point() +
  labs(title="상품 종류별 총 매출",
       caption="Source: mpg",
       x="상품대분류명",
       y="매출액(단위:만원)") 
l3
ggMarginal(l2, type = "histogram", fill="trnsparent")






l2 <- ggplot(s1, aes(cnt,amount)) # 두개의 변수를 가진 hist
l2 + geom_bin2d(aes(fill=factor(cnt))) + 
  scale_fill_brewer(palette = "Paired") +
  labs(title="분포차트", 
       x="총 구매금액",
       fill="점포ID") +
  theme(legend.position = "bottom")

g <- ggplot(tb, aes(상품대분류명,구매금액))
g + geom_bar() +
  theme_bw() 













lt2 <- lt %>% group_by(성별, 연령, 거주지역) %>% summarise(amount=sum(구매금액))
lt2$거주지역 <- substring(lt2$거주지역, 1, 2)
unique(lt2$거주지역)
lt2[lt2$거주지역=="",]
customer[customer$거주지역=="",] #원데이터를 확인해보니 원래 거주지역이 없는 데이터
lt2 <- lt2[!(lt2$거주지역==""),] #거주지역 없는 행 삭제

ggplot(lt2,aes(거주지역, amount/10000)) + 
  geom_violin() +
  labs(title="시간대별 총 사용금액 분포",
       x="시간",
       y="총 사용금액 (단위:만원)") 

lt2_reg <- sqldf("select 거주지역, sum(amount) as amount from lt2 group by 거주지역 ")


###############map-실패########################
id <- c(9,8,15,14,4,2,5,1,0,6,3,13,12,16,11,10)
lt2_reg <- cbind(lt2_reg,id)

sidoshp <- readOGR("sido/ctp_rvn.shp")
class(sidoshp) #공간지리에 관련된 데이터 프레임
summary(sidoshp)
slotNames(sidoshp)
sidoshp@data
head(sidoshp@polygons)

### 시도 지도 표시 ###
korea<-fortify(sidoshp) #R데이터셑으로 변경
korea <- merge(korea,lt2_reg,by="id")
ggplot(korea, aes(x=long,y=lat, group=group, color=id)) + #x:경도, y:위도
  geom_polygon(fill="white") +
  theme(legend.position="none")

ggplot()+geom_polygon(data=korea, aes(x=long, y=lat, group=group,fill=amount),color="white")+
  scale_fill_gradient(low = "#DCEDC8", 
                      high = "#42B3D5", 
                      space = "Lab", 
                      guide = "colourbar") +
  theme_void()

tail(korea)

#################################





l2 <- ggplot(lt2, aes(거주지역, 연령))+
  geom_contour(aes(amount=amount))+
  scale_fill_gradientn(colors = brewer.pal(n = 5, name = "RdBu")) 

g2 <- ggplot(s1, aes(거래월, 점포ID)) +
  geom_tile(aes(fill = amount)) + #geom_tile :3 variables -거래월,점포id,amount
  scale_fill_gradientn(colors = brewer.pal(n = 5, name = "RdBu")) #gradientn : 색깔의 숫자 정함
g2



lt2 <- lt %>%  #s1은 월별, 점포와 다 섞인 데이터
  group_by(상품대분류명, 거래시간대) %>% 
  summarise(amount = sum(round(구매금액/1000,0)), cnt = sum(구매건수))


(l2 <- ggplot(lt2, aes(거래시간대, amount))+
    geom_point(aes(col = 거래시간대, size = cnt))+ #size:구매건수
    labs(title = "Bubble Chart", subtitle = "점포별 : 품목 vs 매출액",
         x = "거래시간", y = "매출"))













#dacon
time <- group_by(lt, 거래시간대)

lt_time <- summarise(time, amount=sum(구매금액))
lt1 <- ggplot(lt_time, aes(x=거래시간대, y=amount,fill=거래시간대))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette = "Greens")
lt1


time1 <- group




lt1<-lt %>% 
  group_by(ID, 거래 점포ID)  %>% #ID:고객ID
  summarise(amount=sum(round(구매금액/1000,0)), cnt=sum(구매건수))
