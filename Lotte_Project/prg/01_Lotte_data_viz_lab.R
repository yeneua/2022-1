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
### Define analysis data ####
file=choose.files()
customer <- read.table(file, header=T, sep=",")
file1 = choose.files()
purchaseList <- read.table(file1, header=T, sep=",")

### 테이블 조인





## Create date field ##
####### lubridate 패키지 이용 ###
tb$거래일자 <- ymd(tb$거래일자)
tb$거래월 <- month(tb$거래일자)



### Data exploration




