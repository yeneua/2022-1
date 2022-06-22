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
# clearity : 투명도, I1 ~ IF (IF가 최상품)
#  - I1, SI1, SI2, VS1, VS2, VVS1, VVS2, IF
# x, y : 크기 (mm),
# z : 깊이 (mm)
# table : 다이아몬드 꼭지의 폭과 제일 넓은 곳의 거리
##########################################
head(diamonds)
plot(diamonds$carat, diamonds$price, type="p")
qplot(carat, price, data = diamonds, geom="point")
### ggplot으로 변경


#qplot bar chart
qplot(clarity, data=diamonds, fill=cut, geom="bar")
#ggplot bar chart -> same output



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



# 제목과 x축과 y축 label를 적음



# 4 단계 : 위치 조정 



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
##############################################################

# boxplot
g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot() + 
    labs(title="Box plot", 
       subtitle="도시연비 vs 자동차 종류",
       caption="Source: mpg",
       x="자동차종류",
       y="도시연비") +
  theme_bw()



