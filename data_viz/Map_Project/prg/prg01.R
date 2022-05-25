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

ggplot(korea[korea$id==0,], aes(x=long,y=lat, group=group, color=id)) + 
  geom_polygon(fill="white") +
  theme(legend.position="none") #0:서울,1:부산,2:대구 ,3:인천, 4:광주, 5:대전, 6:울산, 7: 세종시, 8:경기도,9:강원도,10:충청북도,11:충남,12:전북,13:전남,14:경북,15:경남,16:제주

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




##미주
#graphics.off()
sidoshp@proj4string
#spTransform() 함수를 이용하여 좌표계 변환
korea <- (spTransform(sidoshp, CRS("+proj=longlat"))) 
korea@proj4string
pop <- read.csv("data/popData.csv")
pop <- subset(pop, id!=7) #세종시 제거
korea <- fortify(korea)
korea <- subset(korea, id!=7)

korea_map <- merge(korea, pop, by="id")
library(RColorBrewer)
ggplot(korea_map, aes(long, lat, group=group, fill=diff)) +
  geom_polygon()+
  scale_fill_gradientn(colors = brewer.pal(9, name= "YlGnBu"))



#12-1(5/18)
busan <- read.csv("./data/busan_sigungu.csv")
busan #사회안전지수(4개의 영역)
#사회안전시수가 높을수록 살기 좋은 지역
#지표를 구하기에 구가 너무 넓다 - 통 단위로 연구 했었음 ..
#안전지수 무엇하고 관련이 있을까? grdp?? 고민해보기 ..
#무엇이 가장 영향?

cor(busan[,4:8]) #상관계수
#index - economic과(경제활동) 관련이 가장 높다
#기장군, 강서구-점점 좋아짐


rm(list=ls()) #모든 데이터셋을 삭제
install.packages("GISTools")
#install.packages("rgeos")
#install.packages("maptools")
install.packages("rgdal")
library(GISTools)
#library(maptools)
library(ggplot2)
library(dplyr)
library(rgdal)
library(RColorBrewer)

sigungushp = readOGR("sigungu/sig.shp")
summary(sigungushp)
df_map = fortify(sigungushp) # R 데이터 프레임으로 변경
head(df_map,20)
df_map_info = sigungushp@data
head(df_map_info,50)
df_map_info[, "id"] = (1:nrow(df_map_info)) - 1 #0~249.  id로 merge
head(df_map_info,50) #생성된 id컬럼 확인

df_map_info[, "SIDO"] = as.numeric(substr(df_map_info$SIG_CD,
                                          start = 1, stop = 2)) #SIDO컬럼 생성
head(df_map_info,50) #생성된 SIDO컬럼 - substr로 앞에 숫자 2개만 추출했음(서울인지, 부산인지 ,, 부산은 26)

df_map_info_busan = df_map_info[df_map_info$SIDO==26,]  #부산만추출. 서울은 11, 대구는27
df_map_info_busan

df_map_busan = df_map[df_map$id %in% df_map_info_busan$id, ] #df_map(전체맵)에서 아이디가 같은 것만 가지고  오기
head(df_map_busan,20)

busan <- read.csv("./data/busan_sigungu.csv")
busan 

# 2. map과 사회안전지수 데이터를 병합
busan_map <- merge(busan, df_map_busan,by="id")
head(busan_map, 20)

(ggplot(busan_map, aes(long,lat,group=group, fill=index))+
  geom_polygon()+
  scale_fill_gradientn(colors = brewer.pal(9, name = "YlGnBu")))


#ggplot으로 map chart를 그림
(ggplot(busan_map, aes(long,lat,group=group, fill=economic))+ #fill=economic
    geom_polygon()+
    geom_text(busan_map, aes(long,lat,group=group,label=economic))+
    scale_fill_gradientn(colors = brewer.pal(9, name = "YlGnBu")))




## 시험 -> 지금 공부하는 내용. 작년 시험문제 줄 것 - 유형파악.실습.오픈북


### 좌표파일 테스트
library(readxl)
my = read_excel("./data/부산 시군구 중앙 좌표.xlsx")
my

b <- merge(my, busan_map, by="SIG_CD") #x가 중앙좌표
head(b)
head(b)

(ggplot(busan_map, aes(long,lat,group=group, fill=economic))+ #fill=economic
    geom_polygon()+
    geom_text(data = my, 
              aes(x = long, 
                  y = lat,
                  label = SIG_CD))+
    scale_fill_gradientn(colors = brewer.pal(9, name = "YlGnBu")))

(ggplot()+
    geom_polygon(busan_map, aes(long,lat, group=group, fill=economics))+
    geom_text(data = my, aes(x=long,y=lat, label=SIG_CD))+
    scale_fill_gradientn(colors=brewer.pal(9, name="YlGnBu")))



#13-1
c <- as.data.frame(busan_map %>% 
  group_by(group) %>% 
  summarise(long = median(long), lat = median(lat),
            index = paste(SIG_KOR_NM,"(",index,")")), .groups = "keep")
c
##options(dplyr.summarise.inform = TRUE)

ggplot()+geom


