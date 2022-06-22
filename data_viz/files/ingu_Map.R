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
class(sidoshp)
summary(sidoshp)
slotNames(sidoshp)
sidoshp@data
head(sidoshp@polygons)
### 시도 지도 표시 ###
korea <- fortify(sidoshp) # R 데이터 셑으로 변환
ggplot(korea, aes(long, lat, group=group, color=id)) +
  geom_polygon(fill="white")+
  theme(legend.position = "none")
ggplot(korea[korea$id==1,], aes(long, lat, group=group, color=id)) +
  geom_polygon(fill="white")+
  theme(legend.position = "none")
#graphics.off()
sidoshp@proj4string
#spTansform() 함수를 이용하여 좌표계 변환
korea <- (spTransform(sidoshp, CRS("+proj=longlat")))
korea@proj4string
pop<-read.csv("data/popData.csv")
pop <- subset(pop, id!=7)
korea <- fortify(korea)
korea <- subset(korea, id!=7)
korea_map <- merge(korea, pop, by="id")
library(RColorBrewer)
ggplot(korea_map, aes(long, lat, group=group, fill=diff))+
  geom_polygon()+
  scale_fill_gradientn(colors = brewer.pal(9, name="YlGnBu"))
