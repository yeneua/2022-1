library(readxl)
### file4 : 재정자립도
file4 <- read_xlsx(path="data/재정자립도_시도_시_군_구.xlsx")
sum(is.na(file4))
file4 <- as.data.frame(file4)
eco_finance <- file4[-c(1,2),-c(1,3)]

rownames(eco_finance)=NULL
names(eco_finance) <- c("구","finance")
eco_finance$finance <- as.numeric(eco_finance$finance)
eco_finance


### file5:인구천명당 사업체수
file5 <- read_xlsx(path="data/산업별__구·군별_사업체수_및_종사자수.xlsx")
sum(is.na(file5))
file5 <- as.data.frame(file5)
head(file5)

file5 <- file5[-c(1,2,3),c(1,2)]
class(file5$`2019...2`)
file5$`2019...2` <- as.numeric(file5$`2019...2`)
business <- round(file5$`2019...2`/file1$`2019`*1000,1)
eco_business <- as.data.frame(business)
eco_business$구 <- file1$구
eco_business <- eco_business[,c(2,1)]
eco_business

###file6:기초생활수급자비율
file6 <- read_xlsx(path="data/국민기초생활보장_수급자.xlsx")
sum(is.na(file6))
head(file6)
file6 <- as.data.frame(file6)

file6 <- file6[-c(1,2,3,4),c(1,2)]
class(file6$`2019...2`)
file6$`2019...2` <- as.numeric(file6$`2019...2`)
basic <- round(file6$`2019...2`/file1$`2019`*100,1)
eco_basic <- as.data.frame(basic)
eco_basic$구 <- file1$구
eco_basic <- eco_basic[,c(2,1)]
eco_basic

eco <- cbind(eco_finance, eco_business$business,eco_basic$basic)
names(eco) <- c("구","finance","business","basic")
eco