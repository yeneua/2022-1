### file7:노후주택률
file7 <- read_xlsx(path = "data/건축연도별_주택.xlsx")
sum(is.na(file7))
head(file7)
file7 <- as.data.frame(file7)

file7 <- file7[-c(2,3,4,5,6,7),-1]
colnames(file7) <- file7[1,]
file7 <- file7[-1,]

str(file7)
file7$`1980~1989` <- as.numeric(file7$`1980~1989`)#사용할 변수들의 타입변환
file7$`1979년 이전` <- as.numeric(file7$`1979년 이전`)
file7$합계 <- as.numeric(file7$합계)

library(dplyr)
file7 <- file7 %>% mutate("30년이상" = round((`1980~1989`+`1979년 이전`)/합계,2))
life_oldhouse <- file7 %>% select("주택종류및구군별(2)","30년이상")
################life_oldhouse <- file7[,c("주택종류및구군별(2)","30년이상")]
names(life_oldhouse) <- c("구","oldhouse")
rownames(life_oldhouse) = NULL
life_oldhouse


### file8:지하철역사 수
file8 <- read.csv("data/부산교통공사_도시철도역사정보.csv", header=T, sep=',')
head(file8)
life_subway <- file8 %>%
  mutate(sub = substr(file8$역사도로명주소,7,9)) %>%
  filter(substr(file8$역사도로명주소,1,5) == "부산광역시") %>%
  group_by(sub) %>% summarise(n=n())
life_subway
y <- data.frame("영도구",0)
names(y) <- c("sub","n")
life_subway <- bind_rows(life_subway,y)

installed.packages("raster") #detach(package:raster)
library(raster)
sub_t <- trim(life_subway$sub)
life_subway$sub <- sub_t
life_subway <- as.data.frame(life_subway)
names(life_subway) <- c("구","subway")

life_subway <- life_subway[c(14,11,5,16,7,6,4,8,15,10,2,1,13,12,9,3),]
rownames(life_subway) = NULL

for (i in 1:15){
  if(substring(life_subway$구[i], nchar(life_subway$구[i])) != "구" ){
    life_subway$구[i] <- paste0(life_subway$구[i], "구")
  }
}
life_subway


### file9:자동차등록대수당 주차장면수(공영주차장)
file9 <- read_xlsx(path = "data/주차장.xlsx")
file10 <- read_xlsx(path = "data/구·군별_자동차_등록.xlsx")
head(file9)
head(file10)
sum(is.na(file9))
sum(is.na(file10))
file9 <- as.data.frame(file9)
file10 <- as.data.frame(file10)

file9 <- file9 %>% select("구군별(1)","2019...13")
names(file9) <- paste(file9[2,],file9[3,])
file9 <- file9[-c(1,2,3,4),]

file10 <- file10[-c(1,2,3),c(1,3)]

file_parking <- cbind(file9,file10$`2019...3`)
names(file_parking)[3]<- "등록자동차수"

str(file_parking)

file_parking$`공영 면수 (면)` <- as.numeric(file_parking$`공영 면수 (면)`)
file_parking$등록자동차수 <- as.numeric(file_parking$등록자동차수)

file_parking <- file_parking %>% mutate(life_parking = round(등록자동차수/`공영 면수 (면)` *100,2))
file_parking

life_parking <- data.frame(file_parking$`구군별(1) 구군별(1)`, file_parking$life_parking)
names(life_parking) <- c("구","parking")

life_parking





### life
life <- data.frame(life_oldhouse,life_subway$subway,life_parking$parking)
names(life) <- c("구","oldhouse","subway","parking")
life
