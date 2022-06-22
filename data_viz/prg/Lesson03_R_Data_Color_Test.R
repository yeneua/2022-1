install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggplot2")
library(ggplot2)
display.brewer.all() #
display.brewer.pal(n=8, name="Pastel1")
display.brewer.all(n = NULL, type = "div", select = NULL, #div:발산형
                   colorblindFriendly = TRUE)

data(iris)
head(iris, 7)

# 종별로 Sepal.Length
bp <- ggplot(iris, aes(Species, Sepal.Length)) + # 데이터, 축 지정(step1)
      geom_boxplot(aes(fill=Species)) + # 기하객체(step2)
      theme_minimal() +
      theme(legend.position = "top") # 범례 위치
(bp + scale_fill_brewer(palette = "Dark2"))
(bp+scale_fill_grey(start=0.8,end=0.4))
(bp+scale_color_grey(start=0.8,end=0.4))

bp <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) + 
  geom_point(aes(color = Species)) + # 계량형데이터 -> color / aes(color=Species) : 종별로 색깔들어감
  # geom_point()+
  theme_minimal()+
  theme(legend.position = "bottom") +
  ggtitle(" IRIS scatter plot")+
  scale_color_brewer(palette = "Set2")
bp


library(viridis) # viridis는 sequence color만 있음(->계량형데이터)
sp <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point(aes(color = Species)) +
  scale_color_viridis(option = "D") +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggtitle("IRIS point Plot")
sp

# 오잉 viridis fill도 되는디 -> 내가 뭘 말하고 싶은 모르겠다 ..
(sp <- ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
  geom_point(aes(color = Species)) + #point그래프는 fill
  scale_fill_viridis_b(option = "D"))


sdata("mtcars")
head(mtcars)
mtcars
str(mtcars) #데이터구조 확인하기

library(viridis)
(ggplot(mtcars, aes(disp, mpg)) +
  geom_point(aes(color = hp, size = wt, shape = as.factor(cyl))) + # cyl 계량형 벼수인데 shape는 범주형속성이니까 as.factor로 해줌
  scale_color_viridis(option = "rocket") +
  scale_size(range = c(0, 6)) +
  theme_minimal() +
  theme(legend.position = "top"))
?viridis



### Wes Anderson Pallette ###
library(wesanderson)
names(wes_palettes)
wes_palette("Royal2", 3, type = "discrete")


(ggplot(iris, aes(Species, Sepal.Length)) +
    geom_boxplot(aes(fill = Species)) +
    scale_fill_manual(values = wes_palette("Moonrise1", n = 3)) +
    theme_minimal() +
    theme(legend.position = "top"))
