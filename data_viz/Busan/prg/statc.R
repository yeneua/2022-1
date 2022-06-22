unfair <- cbind(pop,eco[,c(2,3,4)],life[,c(2,3,4)],social[,c(2,3,4)])
unfair

id <- c(26110,26140,26170,26200,26230,26260,26290,26320,26350,26380,26410,26440,26470,26500,26530,26710)
unfair <- cbind(unfair,id)
head(unfair)
unfair <- unfair[c(1,14,2:13)]


library(ggplot2)
(pop1 <- ggplot(data= pop_change, aes(x = 구, y= change)) + geom_bar(stat="identity") + labs(title = '2019년 인구증감률', y='증감률'))
(pop1 <- ggplot(data= pop_change, aes(x = reorder(구,-change), y= change)) + geom_bar(stat="identity") + labs(title = '2019년 인구증감률', y='증감률'))
quantile(unfair$change,c(0.25, 0.5, 0.75))
barplot(pop_change$change)

(pop2 <- ggplot(pop_oldage, aes(구, oldage))+geom_point() + labs(y="고령인구비율"))
(pop2 <- ggplot(pop_oldage, aes(reorder(구,oldage), oldage))+geom_point() + labs(y="고령인구비율"))
(pop3 <- ggplot(pop_birth, aes(x = 구, y=birth)) + geom_bar(stat="identity"))
(pop_g <- ggplot(pop, aes(구,pop_score))+geom_bar(stat="identity"))

(pop_g1 <- ggplot(pop, aes(구,pop_score,fill=change_score))+geom_bar(stat="identity"))
(pop_g2 <- ggplot(pop, aes(구,pop_score,fill=oldage_score))+geom_bar(stat="identity"))
(pop_g3 <- ggplot(pop, aes(구,pop_score,fill=birth_score))+geom_bar(stat="identity"))

#eco
(eco1 <- ggplot(eco_finance, aes(구, eco_finance$finance))+geom_bar(stat="identity"))
(eco2 <- ggplot(eco_business, aes(구, business))+geom_bar(stat="identity"))
(eco3 <- ggplot(eco_basic, aes(구, basic))+geom_bar(stat="identity"))
(eco_g <- ggplot(eco,aes(구,eco_score))+geom_bar(stat="identity"))
(eco_g_r <- ggplot(eco,aes(reorder(구,eco_score),eco_score))+geom_bar(stat="identity"))
quantile(unfair$finance)

(eco_g1 <- ggplot(eco, aes(구,eco_score,fill=finance_score))+geom_bar(stat="identity"))
(eco_g2 <- ggplot(eco, aes(구,eco_score,fill=business_score))+geom_bar(stat="identity"))
(eco_g3 <- ggplot(eco, aes(구, eco_score, fill=basic_score))+geom_bar(stat="identity"))

#life
(life1 <- ggplot(life_oldhouse,aes(구,oldhouse))+geom_bar(stat="identity"))
(life2 <- ggplot(life_parking,aes(구,parking))+geom_bar(stat="identity"))
(life3 <- ggplot(life_oldhouse,aes(구,oldhouse))+geom_bar(stat="identity"))
(life_g <- ggplot(life,aes(구,life_score))+geom_bar(stat="identity"))
(life_g_r <- ggplot(life,aes(reorder(구,-life_score),life_score))+geom_bar(stat="identity"))

(life_g1 <- ggplot(life, aes(구,life_score,fill=oldhouse_score))+geom_bar(stat="identity"))
(life_g2 <- ggplot(life, aes(구,life_score,fill=subway_score))+geom_bar(stat="identity"))
(life_g3 <- ggplot(life, aes(구, life_score, fill=parking_score))+geom_bar(stat="identity"))


#social
(social1 <- ggplot(social_school,aes(구,school))+geom_bar(stat="identity"))
(social2 <- ggplot(social_culture,aes(구,culture))+geom_bar(stat="identity"))
(social3 <- ggplot(social_hospital,aes(구,hospital))+geom_bar(stat="identity"))
(social_g <- ggplot(social,aes(구,social_score))+geom_bar(stat="identity"))
(social_g_r <- ggplot(social,aes(reorder(구,social_score),social_score))+geom_bar(stat="identity"))

(social_g1 <- ggplot(social,aes(구,social_score,fill=school_score))+geom_bar(stat="identity"))
(social_g2 <- ggplot(social,aes(구,social_score,fill=culture_score))+geom_bar(stat="identity"))
(social_g2 <- ggplot(social,aes(구,social_score,fill=hospital_score))+geom_bar(stat="identity"))
