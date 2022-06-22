###  2017447 김예나 빅데이터분석 시각화 프로그램 ###

setwd("~/GitHub/schoolWorks/data_viz/finalTest")

# 문제1.1
load(file="testData-1.rda")
data <- testData

str(data)
dim(data)


# 문제1.2
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(ggcorrplot)){
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

# 문제2.1
data$size <- ifelse(data$pop21 >= 500000, "대",
                    ifelse(data$pop21 >= 200000,"중","소"))


# 문제2.2(1)
summary(data$index)
quantile(data$index, prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))


# 문제2.2(2)
summary(data$financeRate21)

# 문제2.3
boxplot(data$index~data$size, xlab = "인구규모", ylab = "사회안전지수", main="2017447 김예나")


# 문제3.1
seoul <- subset(data, si == "서울")
seoul %>% arrange(index) # 최소 - 중랑구 41.0675
seoul %>% arrange(desc(index)) # 최대 - 용산구 71.2725

# 문제3.2
barplot(seoul$index~seoul$gu, xlab="서울시행정구역(구명)", ylab="사회안전지수", main = "2017447 김예나",
        col = rainbow(nrow(seoul)))
nrow(seoul)

# 문제4
str(data)
cor <- subset(data, select= -c(id,si,gu,size))
corr <- round(cor(cor, use = "na.or.complete"), 3)
corr

# 문제4.2
cor <- subset(data,select=-c(id,si,gu,size,popMove21,pop21))
corr <- round(cor(cor, use = "na.or.complete"), 3)
ggcorrplot(corr,
           hc.order = TRUE, type = "upper", lab = TRUE,lab_size = 3,
           method="circle", colors = c("yellowgreen", "yellow", "skyblue"),
           title="2017447 김예나", ggtheme=theme_bw)

# 문제4.3

cor1 <- cor.test(c(data$financeRate21),data$index)
cor2 <- cor.test(c(data$popMove21),data$index)
cor1
cor2

# 문제5.1, 문제5.2
library(plyr)
q5_data <- ddply(data, .(si,size), summarise,mean_index =mean(index, na.rm=T),
          mean_finance= mean(financeRate21, na.rm=T),
          pop_move = sum(popMove21, na.rm = T),
          economi_mean = mean(EconomicRate,na.rm=T))
dim(q5_data)
q5_data

# 문제5.3
q5_data_select <- subset(data, size == "대")
q5_data_select
(ggplot(q5_data_select, aes(index, financeRate21))+
    geom_point(aes(col = si, size = EconomicRate, fill = index)) +
    labs(title="2017447 김예나", x = "사회안전지수", y="재정자립도") +
    scale_color_brewer(palette = "Dark2")+theme_minimal())



# 문제7
library(stringr)
# 문제7.1
reviews <- c("통쾌한 액션 영화입니다",
             "이런 통렬한 사이다같은 맛은 맛별로 시리즈물 계속 나와야 함 ",
             "ㄹㅇ 오랜만에 깔깔거리면서 봄ㅋㅋ ",
             "추천순보고 재미없는 줄 알았네 ㅋㅋㅋ 엄청 재밌는데, 액션은 평범하지만 어벤져스 까지의 내용전개에 진짜 꼭 필요한 내용, 꿀잼",
             "이렇게나 건조하고 차가운 블랙코미디. 이 자는 정말 나쁜 놈이며 그간 저질러온 숱한 악행들은 벌해야 하지만, 이놈도 결국 벌거벗기면 한낱 개인일 뿐이다.",
             "이런걸 보는 사람들이 우리나라에 같이 살고있다는게 참 한심스럽다 " ,
             "우상화 ",
             "전작에 비해 아쉬움 ㅁㅁ",
             "믿고 보는 범죄의 도시! 계속된 재미",
             "저도 범죄의 도시 정주행중 ㅋㅋ 그냥 1편이나 2편이나 비슷하게 재미 있는 것 같음",
             "범죄의 도시 정주행중!! 마동석 멋져",
             "확실히 2편은 뭔가 약해..쩝",
             "1편보단 못하다... 처음이랑 끝에 밖에 액션신도 없고.. 기대보단 좀... 매력이 없다",
             "악당 리얼 최민식 닮음 나만 느낌???",
             "전작에 비해 아쉽지만 여전히 어벤져서 라인업 중 가장 세련된 시리즈"
)
titles <- c("범죄도시2", "범죄도시2", "탑건: 매버릭", "범죄도시2", "탑건: 매버릭", "탑건: 매버릭", "범죄도시2", "범죄도시2", "탑건: 매버릭", "탑건: 매버릭", "범죄도시2", "범죄도시2", "탑건: 매버릭", "탑건: 매버릭")

## 2. 사전 만들기
pos.words <- c("멋져", "재미", "믿고", "정주행중", "세련된", "꿀잼", "통쾌한", "깔깔거리면서", "사이다", "세련된")
neg.words <- c("기대보단", "못하다", "아쉬움", "약해..쩝", "허무했고", "우상화", "건조하고", "한심스럽다", "기대보단")


length(reviews) #=>15
table(titles) 

# 문제7.2
index = which(titles == "범죄도시2")
index

# 문제7.3
predict <- NULL
for(i in index){ # i:범죄도시만
  pos.cnt <- str_count(reviews[i], pos.words)
  p.cnt <- sum(pos.cnt)
  neg.cnt <- str_count(reviews[i], neg.words)
  n.cnt <- sum(neg.cnt)
  if(p.cnt > n.cnt){
    predict <- c(predict,1)
  } else if(p.cnt < n.cnt){
    predict <- c(predict,-1)
  } else {
    predict <- c(predict,0)
  }
}
predict

# 문제7.4
p = length(predict[predict==1])
n = length(predict[predict==-1])
polarity=(p-n)/(p+n)
polarity


# 문제7.5
pie(table(predict), col=c("red", "blue"))
