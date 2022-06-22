#예제
graphics.off() #그래프취소

#예제1)
x <- c(1,5,8,5,7,10,11,3,4,7,12,15)
y <- c(1,2,3,4,5,6,7,8,9,10,11,12)
plot(x, y, main = "Plot 그리기", sub = "부제목", xlab = "number", ylab = "value")


### 예제
cars

# plot():두 계량형 데이터의 선형관계

#파라미터
par(mfrow=c(1,2), # row기준으로 1행 2열로 나누기
    bg = "#CCCCCC") 
plot(cars$speed, cars$dist,                           # x축:speed, y축:dist(거리)
     main="산점도 그림", sub="두변수간의 선형 관계",  # 제목
     cex=0.5,                                         #크기
     col="#0000FF",                                   #색. "#FFFFFF"=>흰색
     xlab="속도", ylab="거리",                        # x, y축 제목(?)
     xlim=c(0,30), ylim=c(0, 140),                    # 축범위
     pch=1,                                           # 점 모양
     type = "p")                                      # 그래프 종류                                         

text(10, 20, "확인하세요", col="red") #low-level

#abline() -> 직선추가함수
#cars 데이터를 이용하여 속도에 대한 제동거리의 신뢰구간을 구하세요.
z <- lm(dist ~ speed, data = cars) # lm:linear model, 종속변수~독립변수. => speed와 dist간에 인과관계 확인
# => z확인. intercept&speed -> 회귀식
plot(cars, main="Stopping Distance versus Speed")
abline(z, col="red")  # 추정된 회귀식그래프추가
abline (h = mean(cars$dist), lty = 2, col = "blue") # h : 가로(horizontal)
abline (v = mean(cars$speed), lty = 4, col = "green") # v: 세로(vertical)

if(!require("RColorBrewer")) { # require() -> 논리형으로결과반환
  install.packages("RColorBrewer") 
  library(RColorBrewer)
}

#boxplot 예제 -> 데이터분포확인, 이상치확인
graphics.off() # 그래프, 파라미터 옵션 모두해제
v1 <- c(10,12,15,11,20)
v2 <- c(5,7,15,8,9)
v3 <- c(11,20,15,18,13)
boxplot(v1,v2,v3,
        col = brewer.pal(3,"Set1"),  # 색상
        names = c("Blue", "Yellow", "Pink"),
        horizontal = T) #수평으로표시
# => yellow에 이상점존재 왜??
#   boxplot - 좌우대칭,분포,흩어진정도 확인 가능

#histogram 그래프
hist(cars$speed, main="속도의 히스토그램", cex=0.5, #ylim=c(0,40),
     col="#FFFF00", xlab="속도", ylab="빈도")
legend("right",c("Speed 빈도")) #범례
#histogram 연습
# => 봉우리가 하나이고, 완벽하게 대칭은 아닌것을 확인가능

# hist,boxplot : 데이터 탐색 시, 기본적으로 사용 多

# 평균이 5이고 표준편차가 1인 정규분포에서 100의 샘플을 생성한다.
x <- rnorm(100, mean = 5, sd = 1) # 난수발생
hist(x)
# freq=F : 빈도가 아닌 밀도로 표시
hist(x, freq=F)
hist(x, prob=T)
# 평균이 5이고 표준편차가 1인 정규분포의 밀도 함수 곡선을 그린다.
curve(dnorm(x, mean = 5, sd = 1))
curve(dnorm(x, mean = 5, sd = 1),add = T) # add = T : 겹쳐 그림.

#histogram
graphics.off()
par(mfrow=c(1,2)) # 1행2열
height<-c(182,175,167,172,163,178,181,166,159,155)
hist(height, main="histogram of height")
hist(height, main="histogram of height",
     prob=T, #height의 히스토그램, 확률척도 사용
     col='lightblue')
hist(height, freq = F)

# 추정된 확률밀도를 그래프로 그림(density)
lines(density(height), col = "red") # lines():low-level,선추가함수

# 줄기잎그림
stem(height) # => 개별관찰값확인가능+퍼져있는정도확인가능(hist역할) -> but, 샘플수가많으면부적절

# 두 변수간관계
# 분포, 흩어진정도 -> boxplot()
# 분포형태, 대칭정도, 봉우리 -> hist()
# hist와비슷, 개별값->줄기와 잎그림
# 범주형 - pie(), barplot()
