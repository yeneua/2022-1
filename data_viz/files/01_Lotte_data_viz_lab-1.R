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

if(!require(lubridate)){ #날짜 데이터를 변환
  install.packages("lubridate")
  library(lubridate)
}
if(!require(dplyr)){ 
  install.packages("dplyr")
  require(dplyr)
}
### Define analysis data ####
file=choose.files()
customer <- read.table(file, header=T, sep=",")
file1 = choose.files()
purchaseList <- read.table(file1, header=T, sep=",")

### 테이블 조인
tb <- sqldf("select a.id, a.성별, a.연령, b.거래일자, b.상품대분류명,
             b.상품중분류명, b.구매건수, b.거래식별ID, b.구매금액, b.점포ID
             from customer as a, purchaseList as b where a.id=b.id ")

## Create date field ##
####### lubridate 패키지 이용 ###
tb$거래일자 <- ymd(tb$거래일자)
tb$거래월 <- month(tb$거래일자)
tb$구매금액 <- as.numeric(tb$구매금액)
tb$구매건수 <- as.numeric(tb$구매건수)
### Data exploration

s1<- tb %>%
     group_by(거래월, 상품대분류명, 점포ID) %>%
     summarise(amount=sum(round(구매금액/1000,0)), cnt=sum(구매건수))

boxplot(s1$amount~s1$거래월)
boxplot(s1$amount~s1$점포ID)
save(tb, file="tb.rda")
save(s1, file="s1.rda")
