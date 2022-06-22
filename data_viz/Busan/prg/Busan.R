# 2017447 김예나

installed.packages("raster") #detach(package:raster)
install.packages("ggplot2")
install.packages("ggmap")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("knitr")

library(readxl)
library(dplyr)
library(raster)
library(ggplot2)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(knitr)

### 1. 데이터 전처리 ###

## file1 : 인구증감률
file1 <- read_xlsx(path="data/구·군별_세대_및_인구.xlsx")
sum(is.na(file1))
file1 <- as.data.frame(file1)
names(file1) #열이름 확인

file1 <- file1[,c("구군별(1)","2018...3","2019...18")] #2018,2019년 인구만 가져오기
file1 <- file1[-c(1,2,3),] #필요없는 행 제거
names(file1) <- c("구","2018","2019") #열이름변경
head(file1)

class(file1$`2018`)
file1$`2018` <- as.numeric(file1$`2018`) #character -> numeric으로 변경
file1$`2019` <- as.numeric(file1$`2019`) #character -> numeric으로 변경

file1$change <- round((file1$`2019` - file1$`2018`)/file1$`2018` * 100,2) #2019년 기준으로 인구증감률 구하기
pop_change <- file1[,c("구","change")] #인구증감률 변수만 저장
rownames(pop_change)=NULL #행이름 없애기
pop_change


##file2 : 고령화인구비율
file2 <- read_xlsx(path="data/구·군별_연령_각세__및_성별_한국인_현황.xlsx")
sum(is.na(file2))
file2 <- as.data.frame(file2)
names(file2)

file2 <- file2[,c("연령별(1)","연령별(2)","2019...54","2019...55","2019...56","2019...57","2019...58","2019...59","2019...60","2019...61","2019...62","2019...63","2019...64","2019...65",
                  "2019...66","2019...67","2019...68","2019...69","2019...70","2019...71","2019...72","2019...73","2019...74","2019...75","2019...76","2019...77","2019...78","2019...79",
                  "2019...80","2019...81","2019...82","2019...83","2019...84","2019...85","2019...86","2019...87","2019...88","2019...89","2019...90","2019...91","2019...92","2019...93",
                  "2019...94","2019...95","2019...96","2019...97","2019...98","2019...99","2019...100","2019...101","2019...102","2019...103","2019...104")] #2019년도 자료만 추출
file2 <- file2[,c("연령별(1)","연령별(2)",paste0("2019...", seq(from = 54, to = 104, by=3)))] #여자,남자 구분된 변수 빼고 "계"만
file2 <- file2[-2,] #필요없는 행 제거
t_file2 <- t(file2) #행렬 전환

names(t_file2)
colnames(t_file2) <- paste(t_file2[1,],":",t_file2[2,]) #열이름 지정
t_file2 <- t_file2[-c(1,2,3),] #필요없는 행 제거

t_file2 <- as.data.frame(t_file2)

str(t_file2)
t_file2$`총계 : 소계` <- as.numeric(t_file2$`총계 : 소계`) #숫자 타입으로 변환
t_file2$`65세 이상 : 소계` <- as.numeric(t_file2$`65세 이상 : 소계`)

t_file2$oldage <- round(t_file2$`65세 이상 : 소계`/t_file2$`총계 : 소계`*100,2) #고령인구비율 변수 생성

pop_oldage <- t_file2[,c("연령별(1) : 연령별(2)", "oldage")]
rownames(pop_oldage)=NULL #행 이름 없애기
names(pop_oldage) <- c("구","oldage")
pop_oldage


## file3 : 합계출산율
file3 <- read_xlsx(path="data/부산광역시_합계출산율__모의_연령별_출산율.xlsx")
sum(is.na(file3))
head(file3)
file3 <- as.data.frame(file3)

pop_birth <- file3[-c(1,2),c(1,2)] #필요한 행,열만 추출
rownames(pop_birth)=NULL #행 이름 없애기
names(pop_birth) <- c("구","birth")
pop_birth$birth <- as.numeric(pop_birth$birth)
pop_birth


## file4 : 재정자립도
file4 <- read_xlsx(path="data/재정자립도_시도_시_군_구.xlsx")
sum(is.na(file4))
file4 <- as.data.frame(file4)

eco_finance <- file4[-c(1,2),-c(1,3)] #필요없는 행, 열 지우기

rownames(eco_finance)=NULL #행 이름 없애기
names(eco_finance) <- c("구","finance")
eco_finance$finance <- as.numeric(eco_finance$finance) #숫자 타입으로 변환
eco_finance


## file5:인구천명당 사업체수
file5 <- read_xlsx(path="data/산업별__구·군별_사업체수_및_종사자수.xlsx")
sum(is.na(file5))
file5 <- as.data.frame(file5)
head(file5)

file5 <- file5[-c(1,2,3),c(1,2)] #필요한 행, 열 추출
class(file5$`2019...2`)
file5$`2019...2` <- as.numeric(file5$`2019...2`) #숫자 타입으로 변환
business <- round(file5$`2019...2`/file1$`2019`*1000,1)
eco_business <- as.data.frame(business)
eco_business$구 <- file1$구
eco_business <- eco_business[,c(2,1)]
eco_business


## file6:기초생활수급자비율
file6 <- read_xlsx(path="data/국민기초생활보장_수급자.xlsx")
sum(is.na(file6))
head(file6)
file6 <- as.data.frame(file6)

file6 <- file6[-c(1,2,3,4),c(1,2)] #필요한 행, 열 추출
class(file6$`2019...2`)
file6$`2019...2` <- as.numeric(file6$`2019...2`)#숫자 타입으로 변환
basic <- round(file6$`2019...2`/file1$`2019`*100,1) #기초생활수급자비율 변수 추가
eco_basic <- as.data.frame(basic)
eco_basic$구 <- file1$구
eco_basic <- eco_basic[,c(2,1)]
eco_basic


## file7:노후주택률
file7 <- read_xlsx(path = "data/건축연도별_주택.xlsx")
sum(is.na(file7))
head(file7)
file7 <- as.data.frame(file7)

file7 <- file7[-c(2,3,4,5,6,7),-1] #필요없는 행, 열 지우기
colnames(file7) <- file7[1,] #열이름 지정
file7 <- file7[-1,]

str(file7)
file7$`1980~1989` <- as.numeric(file7$`1980~1989`) #사용할 변수들의 타입변환
file7$`1979년 이전` <- as.numeric(file7$`1979년 이전`)
file7$합계 <- as.numeric(file7$합계)

file7 <- file7 %>% mutate("30년이상" = round((`1980~1989`+`1979년 이전`)/합계,2)) #노후주택률 변수 추가
life_oldhouse <- file7 %>% select("주택종류및구군별(2)","30년이상") #필요한 열 추출
names(life_oldhouse) <- c("구","oldhouse") #열이름 지정 
rownames(life_oldhouse) = NULL #행 이름 없애기
life_oldhouse


## file8:지하철역사 수
file8 <- read.csv("data/부산교통공사_도시철도역사정보.csv", header=T, sep=',')
head(file8)

life_subway <- file8 %>%
  mutate(sub = substr(file8$역사도로명주소,7,9)) %>% #도로명주소 칼럼에서 구 이름 추출
  filter(substr(file8$역사도로명주소,1,5) == "부산광역시") %>% #'부산광역시'인 행만 가져오기
  group_by(sub) %>% summarise(n=n()) #그룹별로 개수
life_subway

y <- data.frame("영도구",0) #지하철역이 없어서 데이터 없는 영도구 추가해주기
names(y) <- c("sub","n") #열이름 지정
life_subway <- bind_rows(life_subway,y) #합치기

library(raster) #trim()를 쓰기위해 필요한 패키지
sub_t <- trim(life_subway$sub) #띄어쓰기 제거
life_subway$sub <- sub_t
life_subway <- as.data.frame(life_subway)
names(life_subway) <- c("구","subway")

life_subway <- life_subway[c(14,11,5,16,7,6,4,8,15,10,2,1,13,12,9,3),] #순서 변경
rownames(life_subway) = NULL #행 이름 없애기

for (i in 1:15){
  if(substring(life_subway$구[i], nchar(life_subway$구[i])) != "구" ){
    life_subway$구[i] <- paste0(life_subway$구[i], "구")
  }
}
life_subway


## file9, file10 :자동차등록대수당 주차장면수(공영주차장)
file9 <- read_xlsx(path = "data/주차장.xlsx")
head(file9)
sum(is.na(file9))
file9 <- as.data.frame(file9)
head(file10)
file10 <- read_xlsx(path = "data/구·군별_자동차_등록.xlsx")
sum(is.na(file10))
file10 <- as.data.frame(file10)

detach(package:raster) #dplyr의 select 함수와 겹쳐서 에러

file9 <- file9 %>% select("구군별(1)","2019...13")
names(file9) <- paste(file9[2,],file9[3,]) #열이름 지정
file9 <- file9[-c(1,2,3,4),]   #필요없는 행 지우기

file10 <- file10[-c(1,2,3),c(1,3)] #필요한 행, 열 추출

file_parking <- cbind(file9,file10$`2019...3`)
names(file_parking)[3]<- "등록자동차수"

str(file_parking)

file_parking$`공영 면수 (면)` <- as.numeric(file_parking$`공영 면수 (면)`) #숫자 타입으로 변환
file_parking$등록자동차수 <- as.numeric(file_parking$등록자동차수)

file_parking <- file_parking %>% mutate(life_parking = round(등록자동차수/`공영 면수 (면)` *100,2)) #파생변수
file_parking

life_parking <- data.frame(file_parking$`구군별(1) 구군별(1)`, file_parking$life_parking)
names(life_parking) <- c("구","parking")

life_parking


## file11:초등학교
file11 <- read_xlsx(path = "data/초등학교.xlsx")
head(file11)
sum(is.na(file11))
file11 <- as.data.frame(file11)

names(file11) <- paste(file11[1,],":",file11[2,]) #열이름 지정
file11 <- file11[-c(1,2,3),]  #필요없는 행 지우기

social_school <- file11[,c(1,2)]  #필요한 열 추출
str(social_school)
social_school$`학교수 (개) : 소계` <- as.numeric(social_school$`학교수 (개) : 소계`)  #숫자 타입으로 변환
names(social_school) <- c("구","school")

rownames(social_school)=NULL #행 이름 없애기
social_school


## file12:문화기관
file12 <- read_xlsx(path = "data/문화공간.xlsx")
head(file12)
sum(is.na(file12))
file12 <- as.data.frame(file12)

names(file12) <- paste(file12[1,],"-",file12[2,]) #열이름 지정
file12 <- file12[-c(1,2,3),] #필요없는 행 지우기  

str(file12)

for (i in 2:13){ # "-"으로 된 데이터 "0"으로 바꾸고 숫자타입으로 전환하기
  file12[,i] <- as.vector(file12[,i])
  file12[,i] <- gsub("_","0",file12[,i])
  file12[,i] <- as.numeric(file12[,i])
}

name = file12[,1] #행이름
file12 <- data.frame(file12, row.names = name) #행이름
file12 <- file12[,-1]

sum(is.na(file12))
culture <- apply(file12,1,sum,na.rm=T) #행 기준으로 결측치 제외하고 합계

구 <- c("중구","서구","동구","영도구","부산진구","동래구","남구","북구","해운대구","사하구","금정구","강서구","연제구","수영구","사상군","기장군")

social_culture <- as.data.frame(cbind(구,culture))
rownames(social_culture) <- NULL #행 이름 없애기
social_culture$culture <- as.numeric(social_culture$culture) #숫자 타입으로 변환
social_culture


## file13:의료기관수
file13 <- read_xlsx(path = "data/의료기관.xlsx")
head(file13)
sum(is.na(file13))
file13 <- as.data.frame(file13)

names(file13) <- paste(file13[1,],"-",file13[2,])  #열이름 지정
file13 <- file13[-c(1,2,3),]  #필요없는 행 지우기
social_hospital <- file13[,c(1,2)] #필요한 열 추출

names(social_hospital) <- c("구","hospital") #열이름 지정
social_hospital$hospital <- as.numeric(social_hospital$hospital) #숫자 타입으로 변환
rownames(social_hospital) <- NULL#행 이름 없애기
social_hospital



### 2. 사용할 데이터 ###
## pop(인구활력)데이터
pop <- cbind(pop_change,pop_oldage$oldage,pop_birth$birth)
names(pop) <- c("구","change","oldage","birth")
pop


## eco(산업/경제)
eco <- cbind(eco_finance, eco_business$business,eco_basic$basic)
names(eco) <- c("구","finance","business","basic")
eco


## life(생활/주거)
life <- data.frame(life_oldhouse,life_subway$subway,life_parking$parking)
names(life) <- c("구","oldhouse","subway","parking")
life


## social(교육/문화/복지)
social <- cbind(social_school,social_culture$culture,social_hospital$hospital)
names(social) <- c("구","school","culture","hospital")
social

## 순위데이터
# 숫자가 클수록(좋을수록) 낮은 순위부여 -> 높은 점수를 주기위해
change_r <- rank(pop_change$change)
pop_change <- cbind(pop_change,change_r)
pop_change <- pop_change %>% mutate(change_score = change_r/136*100)


oldage_r <- rank(-pop_oldage$oldage)
pop_oldage <- cbind(pop_oldage,oldage_r)
pop_oldage <- pop_oldage %>% mutate(oldage_score = oldage_r/136*100)


birth_r <- rank(pop_birth$birth)
pop_birth <- cbind(pop_birth,birth_r)
pop_birth <- pop_birth %>% mutate(birth_score = birth_r/136*100)


pop <- pop %>% mutate(pop_score = pop_change$change_score+pop_oldage$oldage_score+pop_birth$birth_score)
pop


finance_r <- rank(eco_finance$finance)
eco_finance <- cbind(eco_finance,finance_r)
eco_finance <- eco_finance %>% mutate(finance_score = finance_r/136*100)
eco_finance

business_r <- rank(eco_business$business)
eco_business <- cbind(eco_business,business_r)
eco_business <- eco_business %>% mutate(business_score = business_r/136*100)
eco_business


basic_r <- rank(-eco_basic$basic)
eco_basic <- cbind(eco_basic,round(basic_r,0))
eco_basic <- eco_basic %>% mutate(basic_score = basic_r/136*100)
eco_basic


eco <- eco %>%  mutate(eco_score=eco_finance$finance_score+eco_business$business_score+eco_basic$basic_score)
eco

oldhouse_r <- rank(-life_oldhouse$oldhouse)
life_oldhouse <- cbind(life_oldhouse,round(oldhouse_r,0))
life_oldhouse <- life_oldhouse %>% mutate(oldhouse_score = oldhouse_r/136*100)
life_oldhouse


subway_r <- rank(life_subway$subway)
life_subway <- cbind(life_subway,round(subway_r,0))
life_subway <- life_subway %>% mutate(subway_score = subway_r/136*100)
life_subway


parking_r <- rank(life_parking$parking)
life_parking <- cbind(life_parking,parking_r)
life_parking <- life_parking %>% mutate(parking_score = parking_r/136*100)
life_parking

life <- life %>%  mutate(life_score=life_oldhouse$oldhouse_score+life_subway$subway_score+life_parking$parking_score)
life


school_r <- rank(social_school$school)
social_school <- cbind(social_school,round(school_r,0))
social_school <- social_school %>% mutate(school_score = school_r/136*100)
social_school

culture_r <- rank(social_culture$culture)
social_culture <- cbind(social_culture,round(culture_r,0))
social_culture <- social_culture %>% mutate(curture_score = culture_r/136*100)
social_culture

hospital_r <- rank(social_hospital$hospital)
social_hospital <- cbind(social_hospital,hospital_r)
social_hospital <- social_hospital %>% mutate(hospital_score = hospital_r/136*100)
social_hospital


social <- social %>%  mutate(social_score=social_school$school_score+social_culture$curture_score+social_hospital$hospital_score)
social


score <- as.data.frame(cbind(구,pop$pop_score,eco$eco_score,life$life_score,social$social_score))
names(score) <- c("구", "pop_score","eco_score","life_score","social_score")
score$pop_score <- as.numeric(score$pop_score)
score$eco_score <- as.numeric(score$eco_score)
score$life_score <- as.numeric(score$life_score)
score$social_score <- as.numeric(score$social_score)


score <- score %>% mutate(total_score = score$pop_score+score$eco_score+score$life_score+score$social_score) #4개 영역 점수를 합한 총합계점수 변수 추가
score

pop$change_score <- pop_change$change_score
pop$oldage_score <- pop_oldage$oldage_score
pop$birth_score <- pop_birth$birth_score

eco$finance_score <- eco_finance$finance_score
eco$business_score <- eco_business$business_score
eco$basic_score <- eco_basic$basic_score

life$oldhouse_score <- life_oldhouse$oldhouse_score
life$subway_score <- life_subway$subway_score
life$parking_score <- life_parking$parking_score

social$school_score <- social_school$school_score
social$culture_score <- social_culture$curture_score
social$hospital_score <- social_hospital$hospital_score


### 3. 데이터 탐색 ###

library(ggplot2)
(pop1 <- ggplot(data= pop_change, aes(x = 구, y= change)) + geom_bar(stat="identity") + labs(title = '2019년 인구증감률', y='증감률'))
(pop1 <- ggplot(data= pop_change, aes(x = reorder(구,-change), y= change)) + geom_bar(stat="identity") + labs(title = '2019년 인구증감률', y='증감률'))
quantile(pop$change,c(0.25, 0.5, 0.75))
barplot(pop_change$change)

(pop2 <- ggplot(pop_oldage, aes(구, oldage))+geom_point() + labs(y="고령인구비율"))
(pop2 <- ggplot(pop_oldage, aes(reorder(구,oldage), oldage))+geom_point() + labs(y="고령인구비율"))
(pop3 <- ggplot(pop_birth, aes(x = 구, y=birth)) + geom_bar(stat="identity"))
(pop_g <- ggplot(pop, aes(구,pop_score))+geom_bar(stat="identity"))

(pop_g1 <- ggplot(pop, aes(구,pop_score,fill=change_score))+geom_bar(stat="identity"))
(pop_g2 <- ggplot(pop, aes(구,pop_score,fill=oldage_score))+geom_bar(stat="identity"))
(pop_g3 <- ggplot(pop, aes(구,pop_score,fill=birth_score))+geom_bar(stat="identity"))

#eco
(eco1 <- ggplot(eco_finance, aes(구, eco_finance$finance))+geom_bar(stat="identity"))
(eco2 <- ggplot(eco_business, aes(구, business))+geom_bar(stat="identity"))
(eco3 <- ggplot(eco_basic, aes(구, basic))+geom_bar(stat="identity"))
(eco_g <- ggplot(eco,aes(구,eco_score))+geom_bar(stat="identity"))
(eco_g_r <- ggplot(eco,aes(reorder(구,eco_score),eco_score))+geom_bar(stat="identity"))
quantile(eco$finance)

(eco_g1 <- ggplot(eco, aes(구,eco_score,fill=finance_score))+geom_bar(stat="identity"))
(eco_g2 <- ggplot(eco, aes(구,eco_score,fill=business_score))+geom_bar(stat="identity"))
(eco_g3 <- ggplot(eco, aes(구, eco_score, fill=basic_score))+geom_bar(stat="identity"))

#life
(life1 <- ggplot(life_oldhouse,aes(구,oldhouse))+geom_bar(stat="identity"))
(life2 <- ggplot(life_subway,aes(구,subway))+geom_bar(stat="identity"))
(life3 <- ggplot(life_parking,aes(구,parking))+geom_bar(stat="identity"))
(life_g <- ggplot(life,aes(구,life_score))+geom_bar(stat="identity"))
(life_g_r <- ggplot(life,aes(reorder(구,-life_score),life_score))+geom_bar(stat="identity"))

(life_g1 <- ggplot(life, aes(구,life_score,fill=oldhouse_score))+geom_bar(stat="identity"))
(life_g2 <- ggplot(life, aes(구,life_score,fill=subway_score))+geom_bar(stat="identity"))
(life_g3 <- ggplot(life, aes(구, life_score, fill=parking_score))+geom_bar(stat="identity"))


#social
(social1 <- ggplot(social_school,aes(구,school))+geom_bar(stat="identity"))
(social2 <- ggplot(social_culture,aes(구,culture))+geom_bar(stat="identity"))
(social3 <- ggplot(social_hospital,aes(구,hospital))+geom_bar(stat="identity"))
(social_g <- ggplot(social,aes(구,social_score))+geom_bar(stat="identity"))
(social_g_r <- ggplot(social,aes(reorder(구,social_score),social_score))+geom_bar(stat="identity"))

(social_g1 <- ggplot(social,aes(구,social_score,fill=school_score))+geom_bar(stat="identity"))
(social_g2 <- ggplot(social,aes(구,social_score,fill=culture_score))+geom_bar(stat="identity"))
(social_g2 <- ggplot(social,aes(구,social_score,fill=hospital_score))+geom_bar(stat="identity"))

(total_g <- ggplot(score,aes(구,total_score))+geom_bar(stat="identity"))
(total_g1 <- ggplot(score,aes(구,total_score,fill=pop_score))+geom_bar(stat="identity"))
(total_g2 <- ggplot(score,aes(구,total_score,fill=eco_score))+geom_bar(stat="identity"))
(total_g3 <- ggplot(score,aes(구,total_score,fill=life_score))+geom_bar(stat="identity"))
(total_g4 <- ggplot(score,aes(구,total_score,fill=social_score))+geom_bar(stat="identity"))


### 4. 시각화 ###

library(ggplot2)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(knitr)

map <- shapefile("data/SIG_201703/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
new_map$id <- as.numeric(new_map$id)
busan_map <- new_map[between(new_map$id,26000,27000),]
busan_map

id <- c(26110,26140,26170,26200,26230,26260,26290,26320,26350,26380,26410,26440,26470,26500,26530,26710) #구군별 행정 코드
score <- cbind(score,id)
score

u <- merge(busan_map, score, by='id')

gu <- as.data.frame(cbind(구, score$id))
names(gu) <- c("구","id")

#구군별 좌표 위치 - label 표시하기 위해
long  <- c(129.0335,129.0141,129.0438,129.0628,129.0433,129.078149,129.0922,129.0242,129.1523,128.9751,129.0894,128.9207,129.0816,129.1118,128.9887,129.1967)
lat <- c(35.1046,35.1149,35.1297, 35.083,35.1648,35.208852,35.1303,35.2286,35.1955,35.0842,35.258,35.1617,35.186,35.1645,35.1624,35.2884) 
gu_id <- data.frame(gu,long,lat)

plot <- ggplot() +
  geom_polygon(data = u, aes(x=long, y=lat, group=group, fill = total_score)) +
  scale_fill_gradient(low = "#DCEDC8", high = "#6799FF", space = "Lab", guide = "colourbar") +
  theme_bw() +
  labs(title='부산 불균형 실태',fill = "합계 점수") +
  theme_void() + 
  theme(legend.position = c(.15, .77)) +
  geom_text(data = gu_id, 
            aes(x = long, 
                y = lat, 
                label = 구))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 17, hjust = 0.5))
plot

par(mfrow=c(2,2))
plot_pop <- ggplot() +
  geom_polygon(data = u, aes(x=long, y=lat, group=group, fill = pop_score)) +
  scale_fill_gradient(low = "#DCEDC8", high = "#6799FF", space = "Lab", guide = "colourbar") +
  theme_bw() +
  labs(title='부산 불균형 실태 - 인구활력',fill = "합계 점수") +
  theme_void() + 
  theme(legend.position = c(.15, .77)) +
  geom_text(data = gu_id, 
            aes(x = long, 
                y = lat, 
                label = 구))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 17, hjust = 0.5))
plot_pop


plot_eco <- ggplot() +
  geom_polygon(data = u, aes(x=long, y=lat, group=group, fill = eco_score)) +
  scale_fill_gradient(low = "#DCEDC8", high = "#6799FF", space = "Lab", guide = "colourbar") +
  theme_bw() +
  labs(title='부산 불균형 실태 - 산업/경제',fill = "합계 점수") +
  theme_void() + 
  theme(legend.position = c(.15, .77)) +
  geom_text(data = gu_id, 
            aes(x = long, 
                y = lat, 
                label = 구))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 17, hjust = 0.5))
plot_eco

plot_life <- ggplot() +
  geom_polygon(data = u, aes(x=long, y=lat, group=group, fill = life_score)) +
  scale_fill_gradient(low = "#DCEDC8", high = "#6799FF", space = "Lab", guide = "colourbar") +
  theme_bw() +
  labs(title='부산 불균형 실태 - 주거생활환경',fill = "합계 점수") +
  theme_void() + 
  theme(legend.position = c(.15, .77)) +
  geom_text(data = gu_id, 
            aes(x = long, 
                y = lat, 
                label = 구))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 17, hjust = 0.5))
plot_life

plot_social <- ggplot() +
  geom_polygon(data = u, aes(x=long, y=lat, group=group, fill = social_score)) +
  scale_fill_gradient(low = "#DCEDC8", high = "#6799FF", space = "Lab", guide = "colourbar") +
  theme_bw() +
  labs(title='부산 불균형 실태 - 교육문화복지',fill = "합계 점수") +
  theme_void() + 
  theme(legend.position = c(.15, .77)) +
  geom_text(data = gu_id, 
            aes(x = long, 
                y = lat, 
                label = 구))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 17, hjust = 0.5))
plot_social
