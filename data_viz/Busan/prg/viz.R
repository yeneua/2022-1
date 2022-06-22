library(ggplot2)
install.packages("ggmap")
library(ggmap)
register_google(key="AIzaSyD0aMNGw2T1KNkqI7hYDAOyDESg8ehru24")

gg_busan <- get_googlemap("busan",maptype="roadmap")
ggmap(gg_busan)


library(dplyr)
geo_code <- 
  
  
  
install.packages("ggplot2")
install.packages("ggmap")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("knitr")

library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(knitr)

p <- read.csv("data/sample.csv",  header=TRUE)
map <- shapefile("data/SIG_201703/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
View(new_map)

new_map$id <- as.numeric(new_map$id)



seoul_map <- new_map[new_map$id <= 11740,]
P_merge <- merge(seoul_map,p,by='id')
ggplot()+geom_polygon(data=P_merge,aes(x=long,y=lat,group=group),fill='white',color='black')
ggplot()+geom_polygon(data=P_merge,aes(x=long,y=lat,group=group,fill=A))
ggplot() + geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill = A))



busan_map <- new_map[between(new_map$id,26000,27000),]
busan_map
u <- merge(busan_map, score, by='id') 
ggplot()+geom_polygon(data = u,aes(x=long,y=lat,group=group),fill='white',color='black')




ggplot()+geom_polygon(data=u, aes(x=long,y=lat, group=group,fill=total_score))


###################################

install.packages("ggplot2")
install.packages("ggmap")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("knitr")

library(ggmap)s
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(knitr)

map <- shapefile("data/SIG_201703/TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
new_map$id <- as.numeric(new_map$id)
busan_map <- new_map[between(new_map$id,26000,27000),]
busan_map
u <- merge(busan_map, score, by='id')
#ggplot()+geom_polygon(data = u,aes(x=long,y=lat,group=group),fill='white',color='black')
#ggplot()+geom_polygon(data=u, aes(x=long,y=lat, group=group,fill=total_score))

gu <- as.data.frame(cbind(구, score$id))
names(gu) <- c("구","id")

long  <- c(129.0335,129.0141,129.0438,129.0628,129.0433,129.078149,129.0922,129.0242,129.1523,128.9751,129.0894,128.9207,129.0816,129.1118,128.9887,129.1967)
lat <- c(35.1046,35.1149,35.1297, 35.083,35.1648,35.208852,35.1303,35.2286,35.1955,35.0842,35.258,35.1617,35.186,35.1645,35.1624,35.2884) 
gu_id <- data.frame(gu,long,lat)


plot <- ggplot() +
  geom_polygon(data = u, aes(x=long, y=lat, group=group, fill = total_score)) +
  scale_fill_gradient(low = "#DCEDC8", high = "#42B3D5", space = "Lab", guide = "colourbar") +
  theme_bw() +
  labs(title='부산 불균형',fill = "합계점수") +
  theme_void() + 
  theme(legend.position = c(.15, .77)) +
  geom_text(data = gu_id, 
            aes(x = long, 
                y = lat, 
                label = 구))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), plot.title = element_text(face = "bold", size = 18, hjust = 0.5))






ggplot()+geom_polygon(data=u, aes(x=long, y=lat, group=group,fill=total_score),color="white")+
  scale_fill_gradient(low = "#DCEDC8", 
                      high = "#42B3D5", 
                      space = "Lab", 
                      guide = "colourbar") + 
  labs(title='부산 불균형 지수',fill = "합계점수")+
  theme_void() + 
  theme(legend.position = c(.15, .77)) +
  geom_text(data = gu_id, 
            aes(x = long, 
                y = lat, 
                label = 구))
