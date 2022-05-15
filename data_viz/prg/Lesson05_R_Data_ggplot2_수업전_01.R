## 9-2

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
# price : 가격 ($)v- 계량형데이터
# carat : 다이아몬드의 무게 - 계량형데이터
# cut : cut 품질
# colour : J ~ D (D가 최상품) - 범주형데이터
# clarity : 투명도, I1 ~ IF (IF가 최상품) - 범주형데이터
#  - I1, SI1, SI2, VS1, VS2, VVS1, VVS2, IF
# x, y : 크기 (mm), - 계량형데이터
# z : 깊이 (mm) - 계량형데이터
# table : 다이아몬드 꼭지의 폭과 제일 넓은 곳의 거리 - 계량형데이터
##########################################
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
# ggplot()을 생성하면서 데이터와 미적 매핑 정보를 지정합니다.(x축 caret y축은 price)
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
g <- g+facet_wrap(~cut) #품질별로 그림(group별 plot)

#그래프를화면에표시
g
summary(g) #그래프의 정보 표시
ggsave("C/Users/yena/Documents/GitHub/schoolWorks/data_viz/temp.prg") #그래프이미지저장

#무게(carat), 가격(price), 투명도(clarity) - 무게와 투명도가 가격에 어떤 관계?? => 투명도가 가격에 영향을 많이 미친다. 같은 선상이라도 무게(carat)가 크면 가격에 영향을 준다.

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
#  $ cty(도시 연비)-계량형자료         : int  18 21 20 21 16 18 18 18 16 20 ...
#  $ hwy(고속도로 연비)-계량형자료         : int  29 29 31 30 26 26 27 26 25 28 ...
#  $ fl(연료 종류)          : chr  "p" "p" "p" "p" ...
#  $ class(자동차 종류)-범주형자료       : chr  "compact" "compact" "compact" "compact" ..
##############################################################
data(mpg, package="ggplot2")
head(mpg)
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





