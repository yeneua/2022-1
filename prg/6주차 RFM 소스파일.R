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
### data 정리
library(dplyr)
sale <- rename(sale, cust_id = 癤풻ust_id) #sale.csv
cust <- rename(cust, birth_data=癤풺irth_date) #custInfo.csv
cust$sex_flg <- factor(cust$sex_flg, levels = c(1,2), labels=c("남자", "여자"))
head(sale)
head(cust)
tId <- c(1:298)
sale$sale_date <- as.Date(sale$sale_date)
sale <- cbind(tId, sale)
saleResult <- findRFM(sale)
saleResult$FrequencyPercentile
saleResult$FinalWeightedScore
saleResult$FinalCustomerClass
table(saleResult$FinalCustomerClass)
saleResult$FinalScore
with(saleResult, boxplot(MeanValue~FinalCustomerClass, 
                         col=brewer.pal(4,"Pastel2"),
                         xlab="고객분류",
                         ylab="거래액"))
hist(saleResult$MeanValue)
hist(saleResult$NoTransaction)
(t<-as.numeric(as.Date("2013-12-31"))-as.numeric(saleResult$LastTransaction))
hist(t)
t<- cbind(t, saleResult)
rfm <- subset(t, select=c(CustomerID, MeanValue, NoTransaction, t))
rfm
quantile(rfm$MeanValue, probs = c(0.1, 0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9, 1))
hist(rfm$MeanValue)
quantile(rfm$NoTransaction, probs = c(0.1, 0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9,0.95, 1))
hist(rfm$NoTransaction)
quantile(rfm$t, probs = c(0.1, 0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9,1))
hist(rfm$t)
#### rfm 점수
rfm$Mscore <- with(rfm, ifelse(MeanValue>=9000000, 4, 
                               ifelse(MeanValue>=650000, 3,
                                      ifelse(MeanValue>=380000, 2,1))))

rfm$Fscore <- with(rfm, ifelse(NoTransaction>=33, 4, 
                               ifelse(NoTransaction>=20, 3,
                                      ifelse(MeanValue>=10, 2,1))))
rfm$Rscore <- with(rfm, ifelse(t>=40, 1, 
                               ifelse(t>=25, 2,
                                      ifelse(t>=14, 3,4))))
rfm$FinalScore <- rfm$Rscore*3+rfm$Fscore*2+rfm$Mscore*5
boxplot(rfm$FinalScore)
quantile(rfm$FinalScore, probs = c(0.1, 0.2, 0.25, 0.4, 0.5, 0.6, 0.75, 0.8, 0.9,1))
rfm$Class <- with(rfm, ifelse(FinalScore>=27, "VVIP", 
                              ifelse(FinalScore>=25, "A", 
                                     ifelse(FinalScore>=20, "B", "C"))))
with(rfm, boxplot(MeanValue~Class, 
                  col=brewer.pal(4,"Pastel2"),
                  xlab="고객분류",
                  ylab="거래액"))
if (!require(sqldf)){
  install.packages("sqldf")
  require(sqldf)
}
sale_cust <- sqldf("select r.*, c.*  from rfm as r, cust as c where r.CustomerId=c.cust_id")
with(sale_cust, table(sale_cust$sex_flg, sale_cust$Class))
