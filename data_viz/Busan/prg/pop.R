library(readxl)
library(dplyr)

### file1 : 인구증감률
file1 <- read_xlsx(path="data/구·군별_세대_및_인구.xlsx")
sum(is.na(file1))#2차 데이터지만, 전처리 과정을 ~하고자
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
rownames(pop_change)=NULL
pop_change


### file2 : 고령화인구비율
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
colnames(t_file2) <- paste(t_file2[1,],":",t_file2[2,])
t_file2 <- t_file2[-c(1,2,3),]

t_file2 <- as.data.frame(t_file2)

t_file2$`총계 : 소계` <- as.numeric(t_file2$`총계 : 소계`)
t_file2$`65세 이상 : 소계` <- as.numeric(t_file2$`65세 이상 : 소계`)

t_file2$oldage <- round(t_file2$`65세 이상 : 소계`/t_file2$`총계 : 소계`*100,2)

pop_oldage <- t_file2[,c("연령별(1) : 연령별(2)", "oldage")]
rownames(pop_oldage)=NULL
names(pop_oldage) <- c("구","oldage")
pop_oldage


### file3 : 합계출산율
file3 <- read_xlsx(path="data/부산광역시_합계출산율__모의_연령별_출산율.xlsx")
sum(is.na(file3))
head(file3)
file3 <- as.data.frame(file3)
pop_birth <- file3[-c(1,2),c(1,2)]
rownames(pop_birth)=NULL
names(pop_birth) <- c("구","birth")
pop_birth$birth <- as.numeric(pop_birth$birth)
pop_birth


### pop(인구활력)데이터
pop <- cbind(pop_change,pop_oldage$oldage,pop_birth$birth)
names(pop) <- c("구","change","oldage","birth")
pop



### 시각화
library(ggplot2)
g <- ggplot(data= pop_change, aes(x = 구, y= pop_change)) + geom_bar(stat="identity")
g #감소-빨강, 증가-파랑 등등