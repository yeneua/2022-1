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
# price : 가격 ($)
# carat : 다이아몬드의 무게
# cut : cut 품질
# colour : J ~ D (D가 최상품)
# clarity : 투명도, I1 ~ IF (IF가 최상품)
#  - I1, SI1, SI2, VS1, VS2, VVS1, VVS2, IF
# x, y : 크기 (mm),
# z : 깊이 (mm)
# table : 다이아몬드 꼭지의 폭과 제일 넓은 곳의 거리
##########################################
head(diamonds)
plot(diamonds$carat, diamonds$price, type="p") #graphics 
qplot(carat, price, data = diamonds, geom="point") #ggplot2 
### ggplot으로 변경
g <- ggplot(diamonds, aes(x=carat, y= price))
g <- g+ geom_point() 
g

#qplot bar chart
qplot(clarity, data=diamonds, fill=cut, geom="bar")
#ggplot bar chart -> same output
graphics.off()
ggplot(diamonds, aes(clarity, fill=cut))+geom_bar()


# 1 단계 : 미적 매핑
# ggplot()을 생성하면서 데이터와 미적 매핑 정보를 지정합니다.(x축 caret y축은 price)
# data : 데이터 지정
# aes() : 시각적 속성 지정
# x – X축 데이터 지정
# y – Y축 데이터 지정
g <-ggplot(diamonds, aes(carat, price))

# 2 단계 : 통계 변환 (생략)
# 3 단계 : 기하객체 적용
# 각각의 기하 객체는 미적 매핑 정보를 상속 받습니다.
# 기하객체를 여러 개 지정할 경우, 레이어(Layer) 형태로 중첩되어 표시됩니다.
# 상속 받은 x, y와 여기서 지정한 color 정보를 사용하여 산점도를 그림
# 상속 받은 x, y로 회귀선을 그립니다.

g<- g+geom_point(aes(color=clarity))+geom_smooth()


# 제목과 x축과 y축 label를 적음

g<- g+labs(title="다이어몬드 차트", x='무게', y='가격')

# 4 단계 : 위치 조정 

g<-g+facet_wrap(~cut)

# 그래프를 화면에 표시
g
summary(g) # 그래프의 정보 표시
ggsave("D:/r_workspace/temp.png") # 그래프를 ~.png라는 이름의 이미지 파일로 저장합니다



###################################################
# mpg 데이터 셑 
# 'data.frame': 234 obs. of  11 variables:
#  $ manufacturer(제조회사): chr  "audi" "audi" "audi" "audi" ...
#  $ model(모델)       : chr  "a4" "a4" "a4" "a4" ...
#  $ displ(배기량)       : num  1.8 1.8 2 2 2.8 2.8 3.1 1.8 1.8 2 ...
#  $ year(생산연도)        : int  1999 1999 2008 2008 1999 1999 2008 1999 1999 2008 ...
#  $ cyl(실린더 개수)         : int  4 4 4 4 6 6 6 4 4 4 ...
#  $ trans(변속기 종류)       : chr  "auto(l5)" "manual(m5)" "manual(m6)" "auto(av)" ...
#  $ drv(구동 방식)         : chr  "f" "f" "f" "f" ...
#  $ cty(도시 연비)         : int  18 21 20 21 16 18 18 18 16 20 ...
#  $ hwy(고속도로 연비)         : int  29 29 31 30 26 26 27 26 25 28 ...
#  $ fl(연료 종류)          : chr  "p" "p" "p" "p" ...
#  $ class(자동차 종류)       : chr  "compact" "compact" "compact" "compact" ..
##############################################################3
data(mpg, package="ggplot2")
head(mpg)
# boxplot
g <- ggplot(mpg, aes(class, cty, fill=class))
g<- g + geom_boxplot() + 
  scale_fill_brewer(palette = "Set2")+
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") 
g

data(mpg, package="ggplot2")
mpg$manufacturer
table(mpg$manufacturer)
## audi, ford, hyundai, toyota만 추출



# bubble chart 차트 
g <- ggplot(mpg_select, aes(x=displ, y=cty)) + 
     geom_point(aes(col=manufacturer, size=hwy)) +
     labs(title="Bubble Chart", subtitle="mpg : 배기량 vs 도시연비",
          x = "배기량", y="도시연비")
g


##### Corrleogram #################
if(!require(ggcorrplot)){
  install.packages("ggcorrplot")
  library(ggcorrplot)
}

data1<- subset(mpg, select=c(displ,cty,hwy,cyl))
corr <- round(cor(data1), 3)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "upper", 
           # 상관계수를 표시 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mcar", 
           ggtheme=theme_bw)

data(mpg, package="ggplot2")

if(!require(ggExtra)){
  install.packages("ggExtra")
  library(ggExtra)
}
# Marginal Histogram / Boxplot

g <- ggplot(mpg, aes(cty, hwy)) + 
     geom_point() + 
     geom_smooth()

g
ggMarginal(g, type = "histogram")
# boxplot을 그려봄


# density를 그려봄 



##################viloin chart #####################
g <- ggplot(mpg, aes(class, cty))
g + geom_violin() + 
  labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw()



#######################Density plot #############
g <- ggplot(mpg, aes(cty))
g + geom_density(aes(fill=factor(cyl)), alpha=0.5) + 
    scale_fill_brewer(palette = "Pastel1") +
    labs(title="분포차트", 
       subtitle="실린더수에 의한 도시연비",
       caption="Source: mpg",
       x="도시연비",
       fill="# 실린드수") +
    theme(legend.position = "bottom")
