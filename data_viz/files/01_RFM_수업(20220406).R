# 데이터 작성
# 거래번호별 고객의 구매 데이터
if (!require(dplyr)){
  install.packages("dplyr")
  require(dplyr)
} 
if (!require(didrooRFM)){
  install.packages("didrooRFM")
  require(didrooRFM)
}
if (!require(RColorBrewer)){
  install.packages("RColorBrewer")
  require(RColorBrewer)
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

#RMF 점수계산
result <- findRFM(customerData, 4, 3, 3)
result

# 고객분류별 빈도확인
table(result$FinalCustomerClass)
result %>% 
  group_by(FinalCustomerClass) %>%
  summarise(count = n(),
            money = mean(MeanValue),
            freq = mean(NoTransaction))

### data file 읽음 
file <- choose.files() #file open
sale <- read.csv(file,header=T)
### data 정리
sale <- rename(sale, cust_id = 癤풻ust_id)
head(sale)
tId <- c(1:298)
sale$sale_date <- as.Date(sale$sale_date)
sale <- cbind(tId, sale)
saleResult <- findRFM(sale)
saleResult$FinalCustomerClass
table(saleResult$FinalCustomerClass)
saleResult$FinalScore
with(saleResult, boxplot(MeanValue~FinalCustomerClass))
with(saleResult, boxplot(NoTransaction~FinalCustomerClass))

with(saleResult, boxplot(FinalScore~FinalCustomerClass,
                         col=brewer.pal(4, "Pastel2"),
                         xlab="고객분류",
                         ylab="평균거래금액"))
hist(saleResult$MeanValue)
hist(saleResult$NoTransaction)
hist(as.numeric(saleResult$LastTransaction))
