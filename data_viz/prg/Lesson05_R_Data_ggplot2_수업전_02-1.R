
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  require(RColorBrewer)
}

###########################################
# diamonds 데이터 설명(다이아몬드의 캐럿, 커팅 정보)
# price      : 가격 ($)v- 계량형데이터
# carat      : 다이아몬드의 무게 - 계량형데이터
# cut        : cut 품질
# colour     : J ~ D (D가 최상품) - 범주형데이터
# clarity    : 투명도, I1 ~ IF (IF가 최상품) - 범주형데이터
#              - I1, SI1, SI2, VS1, VS2, VVS1, VVS2, IF
# x, y       : 크기 (mm), - 계량형데이터
# z          : 깊이 (mm) - 계량형데이터
# table      : 다이아몬드 꼭지의 폭과 제일 넓은 곳의 거리 - 계량형데이터
##########################################

# 무게(carat), 투명도(clarity)가 가격과 어떠한 관계?

head(diamonds)
plot(diamonds$carat, diamonds$price, type="p") #graphics => 캐럿이 클수록 가격이 높다. 
qplot(carat, price, data = diamonds, geom="point") #ggplot2
### ggplot으로 변경
g <- ggplot(diamonds, aes(x = carat, y = price))
g <- g+geom_point()
g #ggplot이 qplot보다 속도가 느리다

#qplot bar chart
qplot(clarity, data=diamonds, fill=cut, geom="bar") #변수한개
#ggplot bar chart -> same output
(ggplot(diamonds, aes(x=clarity,fill=cut))+geom_bar())
graphics.off()

# 1 단계 : 미적 매핑
# ggplot()을 생성하면서 데이터와 미적 매핑 정보를 지정합니다.(x축 caret, y축은 price)
# data : 데이터 지정
# aes() : 시각적 속성 지정
# x – X축 데이터 지정(위치정보) + color, fill 등도 넣을 수 있음
# y – Y축 데이터 지정(위치정보) + color, fill 등도 넣을 수 있음
g <-ggplot(diamonds, aes(carat, price)) # data:diamond, x축:carat,y축:price

# 2 단계 : 통계 변환 (생략)
# 3 단계 : 기하객체 적용
# 각각의 기하 객체는 미적 매핑 정보를 상속 받습니다. - layer를 올리는 것
# 기하객체를 여러 개 지정할 경우, 레이어(Layer) 형태로 중첩되어 표시됩니다.
# 상속 받은 x, y와 여기서 지정한 color 정보를 사용하여 산점도를 그림
# 상속 받은 x, y로 회귀선을 그립니다. geom_smooth()
g <- g+geom_point(aes(col=clarity))+geom_smooth()

# 제목과 x축과 y축 label를 적음
g <- g+labs(title="다이아몬드 차트",x="무게", y="가격")
g


# 4 단계 : 위치 조정 
g <- g+facet_wrap(~cut) #품질(cut)별로 그림(group별 plot)

#그래프를화면에표시
g
summary(g) #그래프의 정보 표시
ggsave("diamonds.png")
ggsave("C/Users/yena/Documents/GitHub/schoolWorks/data_viz/pizza.png") #그래프이미지저장
png("plot.png",width=2000,height=4000,res=500)

setwd("C/Users/yena/Documents/GitHub/schoolWorks/data_viz/")
setwd("~/GitHub/schoolWorks/data_viz/prg")
#무게(carat), 가격(price), 투명도(clarity) - 무게와 투명도가 가격에 어떤 관계?? => 투명도가 가격에 영향을 많이 미친다. 같은 선상이라도 무게(carat)가 크면 가격에 영향을 준다.

###################################################
# mpg 데이터 셑 
# 'data.frame'                      : 234 obs. of  11 variables:
#  $ manufacturer(제조회사)         : chr  "audi" "audi" "audi" "audi" ...
#  $ model(모델)                    : chr  "a4" "a4" "a4" "a4" ...
#  $ displ(배기량)                  : num  1.8 1.8 2 2 2.8 2.8 3.1 1.8 1.8 2 ...
#  $ year(생산연도)                 : int  1999 1999 2008 2008 1999 1999 2008 1999 1999 2008 ...
#  $ cyl(실린더 개수)               : int  4 4 4 4 6 6 6 4 4 4 ...
#  $ trans(변속기 종류)             : chr  "auto(l5)" "manual(m5)" "manual(m6)" "auto(av)" ...
#  $ drv(구동 방식)                 : chr  "f" "f" "f" "f" ...
#  $ cty(도시 연비)-계량형자료      : int  18 21 20 21 16 18 18 18 16 20 ...
#  $ hwy(고속도로 연비)-계량형자료  : int  29 29 31 30 26 26 27 26 25 28 ...
#  $ fl(연료 종류)                  : chr  "p" "p" "p" "p" ...
#  $ class(자동차 종류)-범주형자료  : chr  "compact" "compact" "compact" "compact" ..
##############################################################
data(mpg, package="ggplot2")
head(mpg)

# 자동차종류별 도시연비
# //나혼자그려봄 
ggplot(mpg, aes(class, cty)) + geom_boxplot(aes(fill=class)) + scale_fill_grey()


# boxplot
g <- ggplot(mpg, aes(class,cty,fill=class)) #1단계(필수)
g <- g + geom_boxplot() + #3단계(필수)
  scale_fill_brewer(palette = "Set2")+   #클래스별로 컬러
  labs(title="Box plot",
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw()
g
# => compact가 연비가 가장 좋음. 변동성도 적음(박스크기가 작다). outlier도 있음(운전방법에 따라서 연비가 엄ㅊ청 좋아짐)


data(mpg, package="ggplot2")
mpg$manufacturer
table(mpg$manufacturer)
## audi, ford, hyundai, toyota만 추출
mpg_select = mpg[mpg$manufacturer %in% c("audi","ford","hyundai","toyota"),]
# subset()이용, 같은결과 mpg_select = subset(mpg, manufacturer == "audi" | manufacturer == "ford" | manufacturer == "hyundai" | manufacturer == "toyota")
mpg_select

m <- head(mpg)
m <- as.vector(m)
m[c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),] # 6행 中 3,5,6행선택

# bubble chart 차트 
g <- ggplot(mpg_select, aes(x=displ, y=cty)) +  #x:배기량,y:도시연비
     geom_point(aes(col=manufacturer, size=hwy)) + #color:제조사, size:고속도로연비
     labs(title="Bubble Chart", subtitle="mpg : 배기량 vs 도시연비",
          x = "배기량", y="도시연비")
g
# => 배기량이 높을수록 도시연비는 안좋다.
# => 어떤 제조사가 연비 좋은지 ? 같은 배기량에서 어떤 제조사가 연비가 좋은지? 확인해줄 수 있는 그림
# ㄴ-> 같은배기량일때, toyota(보라색)의 연비가 대체로 좋다. 연비로 비교해보았을때는 toyota가 좋고, ford의 연비가 안좋다. 
# => hwy, cty는 연관이 높다.
# => 배기량-연비, hwy-cty, cyl 상관관계확인

##### Corrleogram #################
if(!require(ggcorrplot)){
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

data1<- subset(mpg, select=c(displ,cty,hwy,cyl)) #속성(열) 4개
corrk <- round(cor(data1), 3) #linear(선형) 상관계수 : -1~1 -> 상관관계가 높을수록 절대값 1
corrk
# cor() : 상관관계
# corr => 배기량-연비 : 관련이 높다(음의 상관관계)
#         cty-hwy : 양의상관관계
#         cyl-displ : 양의상관관계
with(mpg, cor.test(hwy,cty)) #p-value확인, 가설검증 : cor.test
cor.test(mpg$hwy, mpg$cty)

# Plot
ggcorrplot(corrk,
           hc.order = TRUE, #정렬
           type = "upper",
           # 상관계수를 표시
           lab = TRUE,
           lab_size = 3, #글자크기
           method="circle",  #상관정도를 원으로 표시 - 원의크기
           colors = c("yellowgreen", "yellow", "skyblue"), #음-tomato, 0-white, 양-springgreen
           title="Correlogram of mcar", 
           ggtheme=theme_bw)

data(mpg, package="ggplot2")

if(!require(ggExtra)){
  install.packages("ggExtra")
  library(ggExtra)
}
# Marginal Histogram / Boxplot
#(10-2-2)
g1 <- ggplot(mpg, aes(cty, hwy)) + 
     geom_point() + 
     geom_smooth()+ labs(title="g1")
g1

(g2 <- ggplot(mpg, aes(cty, hwy)) +
  geom_count()+ #geom_count()로 바꿔보기
  geom_smooth()+
    labs(title="g2 geom_count()"))

g3 <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm",se=F)+ # method = "lm" : 선형, se=F:오차표시안됨
  labs(title="g3 se=F")
g3

g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm",se=T) + # method = "lm" : 선형
  labs(title="g 선형관계")
g#선형관계를 알아보는 그림

library(gridExtra) #ggplot에서 한번에 여러그래프 나타내기(mfrow처럼)
grid.arrange(g,g1,g2,g3, nrow=2,ncol=2)

#선형관계와 더불어, cty는 어떤분포를 하고 있는지, hwy는 어떤 분포를 하고있는지 ? 를 보고 싶다.
library(ggExtra)
ggMarginal(g, type = "histogram")
ggMarginal(g, type = "histogram", fill="transparent") #가장자리에 그림을 그리겠다. 연비에 해당하는 hist, hwy에해당하는 hist - 분포도 확인 가능 

# boxplot을 그려봄
ggMarginal(g, type="boxplot") #cty - outlier 존재하는 것을 확인 가능
# => hwy-중심보다 큰 값들이 중심보다 낮은값들보다 밀집되어있음

# density를 그려봄 - 곡선으로
ggMarginal(g, type="density") #봉우리 확인가능 - 대부분의 추론에서  정규분포를 따른다는 가정을 하기 때문에 봉우리,가운데 확인 중요

ggMarginal(g, type="violin") #type="violin"
ggMarginal(g, type="densigram") #type="densigram"

##################viloin chart #####################
g <- ggplot(mpg, aes(class, cty))
g + geom_violin() +  #분포형태 확인가능
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw() 
# => 종류별로 연비 (violin으로)
# color .. 등등 넣어보기

## boxplot
g <- ggplot(mpg,aes(class,cty))
g + geom_boxplot()+
  labs(title="Box plot",
       subtitle="도시연비vs자동차종류",
       caption="Source:mpg",
       x="자동차종류", y="도시연비") +
  theme_bw()

#데이터에 따라 boxplot,violin 명확하게 나오는 정도 다르다

#######################Density plot #############
g <- ggplot(mpg, aes(cty)) #x축만
g + geom_density(aes(fill=factor(cyl)), alpha=0.5) +  #cyl(실린더)로 채움. alㄴpha:투명도(기본값:1)
    scale_fill_brewer(palette = "Pastel1") +
    labs(title="분포차트", 
       subtitle="실린더수에 의한 도시연비",
       caption="Source: mpg",
       x="도시연비",
       fill="# 실린드수") +
    theme(legend.position = "bottom")
# => 범주형데이터별로 분포확인
# => cyl=5: 봉우리2개, 분포작음(편차가 작다.)
# => cyl-4: 연비가 퍼져있는 정도가 크다.

# 하나의 그래프로 여러가지 정보를 주는것이 핵심적