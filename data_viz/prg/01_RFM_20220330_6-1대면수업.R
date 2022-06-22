### 6-1 대면수업 ###

# 데이터 작성
# 거래번호별 고객의 구매 데이터
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-14.0.2/")
if (!require(dplyr)){    #dplyr이 없으면 install, 있으면 require(불러오기)
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

#RRM
#우수고객?
#R : 최근에 산 사람
#F : 거래빈도가 많은 사람
#M : 거래금액이 많은 사람

data <- customerData %>% 
  group_by(CustomerID) %>% 
  summarise(F = n(), #F : 빈도(구매건수)
            M = sum(Amount), #M : 거래금액의 합
            date = max(DateofPurch)) #R : max ->가장최근날짜
data            

class(data$date)
#R : 현재날짜-최근날짜 => 숫자가 작을 수록 최근
data$R <- as.numeric(as.Date("2017-12-31")-as.Date(data$date)) #as.Date : "" 문자로 쓰여진 날짜를 날짜형식으로 바꿈
data$R # R 컬럼추가됨

#사분위수로 데이터 파악하기
quantile(data$R, probs = c(0.2, 0.4, 0.6, 0.8)) #5분위수로 작성함
quantile(data$F, probs = c(0.2, 0.4, 0.6, 0.8))
quantile(data$M, probs = c(0.2, 0.4, 0.6, 0.8))
#분위를 나누어서 데이터를 보고, 몇퍼센트에는 몇점을 줄지 등등 결정하기

# *boxplot을 이용해서도 해보기
boxplot(data$R)
boxplot(data$F)
boxplot(data$M)


#평가 기준 설정- 점수 매기기 => 충성고객 파악

### RFM 점수계산 ####
result <- findRFM(customerData, 4, 3, 3) #가중치
str(result)
result$MonetoryScore
result$RecencyScore
result$FrequencyScore
result$FinalScore
result$FinalCustomerClass
table(result$FinalCustomerClass)

# 클래스별 몇명인지, MeanValue의 평균
(result %>%
    group_by(FinalCustomerClass) %>%
    summarise(frq = n(), # 몇명인지
              sale = mean(MeanValue)))

# 고객id, 빈도, 금액, 날짜
data <- customerData %>%
        group_by(CustomerID) %>%
        summarise(F = n(), # 몇번?->빈도
                  M = sum(Amount),
                  date = max(DateofPurch))
data$R <- as.numeric(as.Date("2017-12-31")- as.Date(data$date))

quantile(data$M, probs = c(0.2, 0.4, 0.6, 0.8, 0.9)) # 5개 그룹으로 나누기


# 점수부여 -> quantile로 확인하고 어디까지 나눌지 생각
data$Mscore <- ifelse(data$M>=110000, 5, 
                      ifelse(data$M>=46800, 4,
                             ifelse(data$M>=19200, 3,
                                    ifelse(data$M>=14600,2,1))))


# 대면수업
#findFRM(data, 최근성, 빈도, 금액)

### Sale과 Customer Info 데이터 활용 ###
f <- file.choose() #sale.csv 선택
sale <- read.csv(f, header=T)
head(sale)
library(dplyr) #rename 함수 들어있는 패키지
sale <- rename(sale, cust_id=癤풻ust_id)
sale #바뀐지 확인

length(sale) # 열 개수
count(sale) #행개수
t_id=c(1:298) #transaction id가 없으니까 넣어줌 1~298까지
t_id

str(sale)
# transaction date -> date 형식이어야됨. but, str(sale)로 확인해보면 chr형태임 -> as.Date로 바꿔주기
sale$sale_date <- as.Date(sale$sale_date)
str(sale) #date 포맷으로 바뀌었는지 확인

sale <- cbind(t_id, sale) #transaction id 결합해주기. 
head(sale) #확인

saleResult <- findRFM(sale,3,3,4) #가중치
saleResult
#saleResult$MeanValue - 고객별 평균 구매 금액
#saleResult$LastTransaction 최근에 구매한 날짜
#saleResult$NoTransaction 몇개(frequency)
#Score
#FinalScore : weight를 주고 나서 다 합쳐준 것(weight*score += ~)
# RFM 점수와 고객분류를 확인
str(saleResult)

saleResult$FinalScore
saleResult$FinalCustomerClass
table(saleResult$FinalCustomerClass)
saleResult$FinalScore



if (!require(RColorBrewer)){
  install.packages("RColorBrewer")
  require(RColorBrewer)
}

#고객별로 MeanValue(평균구매액)과 finalscore 확인 -> boxplot(분포 확인)
with(saleResult, boxplot(MeanValue~FinalCustomerClass)) #finalcustomerclass별로  meanvalue(구매액)  -  a formula, such as y ~ grps
with(saleResult, boxplot(FinalScore~FinalCustomerClass)) #finalcustomerclass별로 finalscore
with(saleResult, boxplot(FinalScore~FinalCustomerClass,
                         col = brewer.pal(4, "Pastel2"),
                         xlab = "고객분류", ylab = "평균거래금액"))
#meanvalue~finalcustomerclass vs finalscore~finalcustomerclass
#meanvalue는 큰 차이가 없고, 분포도 class2가 더 넓다.


with(saleResult, boxplot(MeanValue~FinalCustomerClass)) #(M)
with(saleResult, boxplot(NoTransaction~FinalCustomerClass)) #빈도가 영향을 주는 듯하다 (F)
with(saleResult, boxplot(LastTransaction~FinalCustomerClass)) #최근성 (R)


#고객들을 4개의 분류로 만들자
#histogram -> 분포의 형태 확인(종모양 ,, 등등)
hist(saleResult$MeanValue)  #구매금액 -> 20~40만원이 빈도 가장 多
hist(saleResult$NoTransaction) #거래건수(빈도) -> 30~35이 빈도 多, 40이상은 1, 나머지 25이하
hist(as.numeric(saleResult$LastTransaction)) #날짜(최근)
# ㄴ> 현재날짜에서 뺀 날짜수로 계산했다. 2013-12-31 -> 숫자가 작을수록 최근 => 날짜를 고치자 !

#분류를 했다고 가정. vip고객, a등급, b등급 고객 -> 활용
