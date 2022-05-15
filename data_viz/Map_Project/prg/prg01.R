rm(list=ls()) #모든 데이터셋을 삭제
install.packages("GISTools")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
library(GISTools)
library(maptools)
library(ggplot2)
library(dplyr)
library(rgdal)

sidoshp <- readOGR("sido/ctp_rvn.shp")
class(sidoshp) # => sp (데이터프레임 아니다-지리정보)
summary(sidoshp)
slotNames(sidoshp) #slot을 분류해서 확인
sidoshp@data #내용확인 id가 중요 !!
head(sidoshp@polygons)

## 시도 지도 표시 ##
korea <- fortify(sidoshp) #R데이터셋으로 변환.데이터프레임으로 변환해주기. fortify() : ggplot2내장함수
ggplot(korea, aes(x=long,y=lat, group=group, color=id)) + #x:경도, y:위도
  geom_polygon(fill="white") +
  theme(legend.position="none")

##부산만 
ggplot(korea[korea$id==1,], aes(x=long,y=lat, group=group, color=id)) + 
  geom_polygon(fill="white") +
  theme(legend.position="none")

##제주도
(ggplot(korea[korea$id==16,], aes(x=long,y=lat, group=group, color=id)) + 
  geom_polygon(fill="white") +
  theme(legend.position="none"))


#위치만 표시할때는 polygon 할 필요X. 데이터랑 연결이 필요하기 때문에 polygon해야함

sidoshp@proj4string #좌표계 확인
#spTransform()함수를 사용하여 좌표계 정보를 변환
korea <- (spTransform(sidoshp, CRS("+proj=longlat"))) #long,lat만 필요
korea@proj4string#proj4string은 prj확장자 내용을 처리한 것

pop <- read.csv("data/popData.csv")
pop
pop <- subset(pop, id!=7) #데이터가 없는 세종시 제거
korea <- fortify(korea)
korea <- subset(pop, id!=7) #데이터가 없는 세종시 제거

#인구 데이터 + sido데이터 병합 ; id기준으로
korea_map <- merge(korea, pop, by="id")

library(RColorBrewer)
ggplot(korea, aes(x=long, y=lat, group=group, fill=diff)) +
  geom_polygon() +
  scale_fill_gradientn(colors = brewer.pal(10, name = "YlGnBu"))
