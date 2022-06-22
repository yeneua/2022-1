# findRFM(customerdata, recencyWeight, frequencyWeight, MonetoryWeight)
# customerdata(trainsactionID, customerID, Date, Amount) date format


# 데이터 작성
# 거래번호별 고객의 구매 데이터
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-14.0.2/")
if (!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
} 
if (!require(didrooRFM)){
  install.packages("didrooRFM")
  require(didrooRFM)
}
TransNo <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
CustomerID <- c('c01', 'c02', 'c03', 'c06', 'c05', 'c01', 'c02', 'c01', 'c03',
                'c01', 'c04', 'c07', 'c06', 'c02', 'c08', 'c08', 'c09', 'c10')
DateofPurch <- as.Date(c("2016-01-15", "2016-02-20", "2016-04-10", "2016-07-15",
                         "2016-07-15", "2017-01-15", "2017-03-10", "2017-05-10",
                         "2017-05-10", "2017-06-05", "2017-07-01", "2017-09-15",
                         "2017-10-10", "2017-11-12", "2017-12-03", "2017-12-10",
                         "2017-12-20", "2017-12-30"))
Amount <- c(24000, 10000, 12000, 60000, 110000, 15000, 60000, 30000, 8000, 18000,
            20000, 15000, 50000, 30000, 6000, 7000, 18000, 8000)
# RFM 입력 데이터 프레임 작성
customerData <- data.frame(TransNo, CustomerID, DateofPurch, Amount)
customerData

### RFM 점수계산 ####
result <- findRFM(customerData, 4, 3, 3)

result$MonetoryScore
result$RecencyScore
result$FrequencyScore
result$FinalScore
result$FinalCustomerClass
table(result$FinalCustomerClass)

(result %>%
    group_by(FinalCustomerClass) %>%
    summarise(frq = n(),
              sale = mean(MeanValue)))

data <- customerData %>%
  group_by(CustomerID) %>%
  summarise(F = n(),
            M = sum(Amount),
            date = max(DateofPurch))
data$R <- as.numeric(as.Date("2017-12-31")- as.Date(data$date))

quantile(data$M, probs = c(0.2, 0.4, 0.6, 0.8, 0.9))

data$Mscore <- ifelse(data$M>=110000, 5, 
                      ifelse(data$M>=46800, 4,
                             ifelse(data$M>=19200, 3,
                                    ifelse(data$M>=14600,2,1))))

#########################  sale 파일 이용하기 #####
if(!require("RColorBrewer")) {
  install.packages("RColorBrewer") 
  require(RColorBrewer)
}
file <- choose.files() #file open
sale <- read.csv(file,header=T)
file1 <- choose.files() #file open
cust <- read.csv(file1,header=T)

str(sale)
str(cust)

### data 정리
library(dplyr)
sale <- rename(sale, cust_id = 癤풻ust_id) #sale.csv
cust <- rename(cust, birth_data=癤풺irth_date) #custInfo.csv

cust$sex_flg <- factor(cust$sex_flg, levels = c(1,2), labels=c("남자", "여자")) #1,2로 된 성별변수에 라벨링
head(sale)
head(cust)
str(sale)
str(cust)
dim(sale)

tId <- c(1:298) #transaction id를 생성해주기 위해 1~298까지 들어있는 벡터 생성
sale$sale_date <- as.Date(sale$sale_date) #transaction date -> date 형식이어야됨. but, str(sale)로 확인해보면 chr형태임 -> as.Date로 바꿔주기
sale <- cbind(tId, sale) #sale 데이터에 transaction id를 추가해줌(cbind)

saleResult <- findRFM(sale) #고객아이디별로 정렬됨
saleResult$FrequencyPercentile 
saleResult$FinalWeightedScore
saleResult$FinalCustomerClass
table(saleResult$FinalCustomerClass)
table(saleResult$FinalCustomerClass,saleResult$FinalScore)
saleResult$FinalScore

with(saleResult, boxplot(MeanValue~FinalCustomerClass,  #FinalcustomerClass별로 MeanValue
                         col=brewer.pal(4,"Pastel2"),
                         xlab="고객분류",
                         ylab="거래액"))

hist(saleResult$MeanValue) #saleResult의 MeanValue 변수를 히스토그램으로
# ㄴ=> 구매금액 -> 20~40만원이 빈도 가장 多

hist(saleResult$NoTransaction) #saleResult의 NoTransaction 변수를 히스토그램으로
# => #거래건수(빈도) -> 30~35이 빈도 多, 40이상은 1, 나머지 25이하

(t<-as.numeric(as.Date("2013-12-31"))-as.numeric(saleResult$LastTransaction)) #현재날짜에서 뺀 날짜수로 계산. 2013-12-31 기준 -> 숫자가 작을수록 최근
hist(t) #날짜 변수를 히스토그램으로 => 최근 거래일이 20일 이하인 사람이 제일 많다.
t<- cbind(t, saleResult) #변수 t에 날짜변수(t)와 saleResult를 합쳐줌(cbind)

rfm <- subset(t, select=c(CustomerID, MeanValue, NoTransaction, t)) #subset함수를 이용해 t에서 원하는 컬럼만 뽑음(t, select=c(CustomerID, MeanValue, NoTransaction, t) -> rfm변수에 할당
rfm #변수확인
quantile(rfm$MeanValue, probs = c(0.1, 0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9, 1))
hist(rfm$MeanValue) #M : 금액
quantile(rfm$NoTransaction, probs = c(0.1, 0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9,0.95, 1))
hist(rfm$NoTransaction) #F : 빈도
quantile(rfm$t, probs = c(0.1, 0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9,1))
hist(rfm$t) #R:최근

#### rfm 점수
rfm$Mscore <- with(rfm, ifelse(MeanValue>=900000, 4, 
                               ifelse(MeanValue>=650000, 3,
                                      ifelse(MeanValue>=380000, 2,1))))

rfm$Fscore <- with(rfm, ifelse(NoTransaction>=33, 4, 
                               ifelse(NoTransaction>=20, 3,
                                      ifelse(NoTransaction>=10, 2,1))))
rfm$Rscore <- with(rfm, ifelse(t>=40, 1, 
                               ifelse(t>=25, 2,
                                      ifelse(t>=14, 3,4))))
rfm$FinalScore <- rfm$Rscore*3+rfm$Fscore*2+rfm$Mscore*5 #가중치를 주어 finalscore
boxplot(rfm$FinalScore)
quantile(rfm$FinalScore, probs = c(0.1, 0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9,1))
# 고객분류
rfm$Class <- with(rfm, ifelse(FinalScore>=27, "VVIP", 
                              ifelse(FinalScore>=25, "A", 
                                     ifelse(FinalScore>=20, "B", "C"))))
with(rfm, boxplot(MeanValue~Class, # Class별로 MeanValue
                  col=brewer.pal(4,"Pastel2"),
                  xlab="고객분류",
                  ylab="거래액")) #클래스별로 거래액 상자그림
if (!require(sqldf)){
  install.packages("sqldf")
  require(sqldf)
}
sale_cust <- sqldf("select r.*, c.*  from rfm as r, cust as c where r.CustomerId=c.cust_id") #id로 조인, rfm과  cust의 모든 열 가져오기
with(sale_cust, table(sale_cust$sex_flg, sale_cust$Class)) #성별, 클래스로 분할표


with(sale_cust, table(sale_cust$enter_date, sale_cust$sex_flg))

with(sale_cust, table(sale_cust$Class,sale_cust$sex_flg))
with(sale_cust, table(sale_cust$enter_date,sale_cust$Class))
with(sale_cust)



boxplot(sale_cust$t ~ sale_cust$sex_flg)

boxplot(sale_cust$t ~ sale_cust$Class)


rfm$FinalScore <- rfm$Rscore*3+rfm$Fscore*2+rfm$Mscore*5 #가중치를 주어 finalscore
boxplot(sale_cust$NoTransaction ~ sale_cust$Class)#등급 - F
boxplot(sale_cust$t~sale_cust$Class) #등급- R(최근성)
boxplot(sale_cust$MeanValue ~ sale_cust$Class) #구매금액 - M


#1. 구매금액은 남자가 높지만 거래빈도는 여자가 많다.
install.packages("wesanderson")
library(wesanderson)

boxplot(sale_cust$t ~ sale_cust$sex_flg)


if (!require(wesanderson)){
  install.packages("wesanderson")
  require(wesanderson)
}
with(sale_cust, boxplot(MeanValue~sex_flg,
                 main= "성별별 거래금액",
                 col=wes_palette("Royal1",2),
                 xlab="성별",
                 ylab="거래액"))

with(sale_cust, boxplot(NoTransaction~sex_flg,
                        main= "성별별 거래빈도",
                        col=wes_palette("Chevalier1",2),
                        xlab="성별",
                        ylab="거래빈도"))

table(sale_cust$Class,sale_cust$sex_flg)

rfm$Mscore <- with(rfm, ifelse(MeanValue>=900000, 4, 
                               ifelse(MeanValue>=650000, 3,
                                      ifelse(MeanValue>=380000, 2,1))))

rfm$Fscore <- with(rfm, ifelse(NoTransaction>=33, 4, 
                               ifelse(NoTransaction>=20, 3,
                                      ifelse(MeanValue>=10, 2,1))))
rfm$Rscore <- with(rfm, ifelse(t>=40, 1, 
                               ifelse(t>=25, 2,
                                      ifelse(t>=14, 3,4))))
rfm$FinalScore <- rfm$Rscore*3+rfm$Fscore*2+rfm$Mscore*5 #가중치를 주어 finalscore

rfm$Class <- with(rfm, ifelse(FinalScore>=27, "VVIP", 
                              ifelse(FinalScore>=25, "A", 
                                     ifelse(FinalScore>=20, "B", "C"))))
table(sale_cust$Class,sale_cust$sex_flg)


sale_cust <- sqldf("select r.*, c.* 
                   from rfm as r, cust as c
                   where r.CustomerId=c.cust_id") #id로 조인, rfm과  cust의 모든 열 가져오기

sale_cust <- sqldf("select r.*,c.*
                   from rfm as r inner join cust as c
                   on r.CustomerID = c.cust_id")
sale_cust$cust_id <- NULL
sale_cust
sqldf("select avg(r.MeanValue)
        from rfm as r, cust as c
        where r.CustomerId=c.cust_id
          and c.sex_flg='여자'")

sqldf("select avg(r.MeanValue)
        from rfm as r, cust as c
        where r.CustomerId=c.cust_id
          and c.sex_flg='남자'")

sqldf("select MeanValue, t
        from sale_cust
        order by MeanValue desc")






sqldf("select avg(MeanValue)
      from sale_cust
      group by sex_flg")

sqldf("select avg(NoTransaction)
      from sale_cust
      group by sex_flg")


sqldf("select Class, sex_flg
      from sale_cust")


sqldf("select CustomerID, MeanValue, t from sale_custorder_by t")

sqldf("select CustomerID, MeanValue, NoTransaction
      from sale_cust
      order by NoTransaction")

sqldf("select *
      from sale_cust
      where CustomerID=51")

sqldf("select CustomerID, MeanValue
      from sale_cust
      order by MeanValue desc")
