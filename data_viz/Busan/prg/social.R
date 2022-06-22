### file11:초등학교
file11 <- read_xlsx(path = "data/초등학교.xlsx")
head(file11)
sum(is.na(file11))
file11 <- as.data.frame(file11)
names(file11) <- paste(file11[1,],":",file11[2,])
file11 <- file11[-c(1,2,3),]

social_school <- file11[,c(1,2)]
str(social_school)
social_school$`학교수 (개) : 소계` <- as.numeric(social_school$`학교수 (개) : 소계`)
names(social_school) <- c("구","school")

rownames(social_school)=NULL
social_school

###file12:문화기관
file12 <- read_xlsx(path = "data/문화공간.xlsx")
head(file12)
sum(is.na(file12))
file12 <- as.data.frame(file12)
names(file12) <- paste(file12[1,],"-",file12[2,])
file12 <- file12[-c(1,2,3),]

str(file12)

for (i in 2:13){
  file12[,i] <- as.vector(file12[,i])
  file12[,i] <- gsub("_","0",file12[,i])
  file12[,i] <- as.numeric(file12[,i])
}

name = file12[,1]

file12 <- data.frame(file12, row.names = name)
file12 <- file12[,-1]

sum(is.na(file12))
culture <- apply(file12,1,sum,na.rm=T)
social_culture <- as.data.frame(cbind(구,culture))
rownames(social_culture) <- NULL
social_culture$culture <- as.numeric(social_culture$culture)
social_culture

구 <- c("중구","서구","동구","영도구","부산진구","동래구","남구","북구","해운대구","사하구","금정구","강서구","연제구","수영구","사상군","기장군")




###file13:의료기관수
file13 <- read_xlsx(path = "data/의료기관.xlsx")
head(file13)
sum(is.na(file13))
file13 <- as.data.frame(file13)
names(file13) <- paste(file13[1,],"-",file13[2,])
file13 <- file13[-c(1,2,3),]
social_hospital <- file13[,c(1,2)]


names(social_hospital) <- c("구","hospital")
social_hospital$hospital <- as.numeric(social_hospital$hospital)
rownames(social_hospital) <- NULL
social_hospital


social <- cbind(social_school,social_culture$culture,social_hospital$hospital)
names(social) <- c("구","school","culture","hospital")
social
