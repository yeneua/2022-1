#예제
graphics.off()

cars
plot(cars$speed, cars$dist, main="산점도 그림", sub="두변수간의 관계",
         cex=0.5, col="#FF0000", xlab="속도", ylab="거리",
         xlim=c(0,30), ylim=c(0, 140), pch=0)
text(10, 20, "확인하세요", col="blue")

#abline
#
z <- lm(dist ~ speed, data=cars)
plot(cars, main="Stopping Distance versus Speed")
abline(z, col="red")
abline (h=mean(cars$dist), lty=2, col="blue")
abline (v=mean(cars$speed), lty=2, col="green")

if(!require("RColorBrewer")) {
  install.packages("RColorBrewer") 
  library(RColorBrewer)
}

#boxplot 예제
graphics.off()
v1 <- c(10,12,15,11,20)
v2 <- c(5,7,15,8,9)
v3 <- c(11,20,15,18,13)
boxplot(v1,v2,v3,col=brewer.pal(3,"Pastel2"),
        names=c("Blue","Yellow","Pink"),
        horizontal=T)

#histogram 그래프
hist(cars$speed, main="속도의 히스토그램", cex=0.5, 
     col="#FFFF00", xlab="속도", ylab="빈도")
legend("right",c("Speed 빈도"))
#histogram 연습

# 평균이 5이고 표준편차가 1인 정규분포에서 100의 샘플을 생성한다.
hist(x)
# freq=F : 빈도가 아닌 밀도로 표시
hist(x, freq=F)
# 평균이 5이고 표준편차가 1인 정규분포의 밀도 함수 곡선을 그린다.
#add=T : 겹쳐 그림.
curve(dnorm(x, mean=5, sd=1))

#histogram
graphics.off()
par(mfrow=c(1,2))
height<-c(182,175,167,172,163,178,181,166,159,155)
hist(height, main="histogram of height")
hist(height, main="histogram of height", prob=T, col='lightblue') #height의 히스토그램, 확률척도 사용
# 추정된 확률밀도를 그래프로 그림

# 줄기잎그림
stem(height)




