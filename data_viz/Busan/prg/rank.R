unfair <- cbind(pop,eco[,c(2,3,4)],life[,c(2,3,4)],social[,c(2,3,4)])
unfair

id <- c(26110,26140,26170,26200,26230,26260,26290,26320,26350,26380,26410,26440,26470,26500,26530,26710)
unfair <- cbind(unfair,id)
head(unfair)
unfair <- unfair[c(1,14,2:13)]


library(ggplot2)

change_r <- rank(pop_change$change)
pop_change <- cbind(pop_change,change_r)
pop_change <- pop_change %>% mutate(change_score = change_r/136*100)


oldage_r <- rank(-pop_oldage$oldage) #
pop_oldage <- cbind(pop_oldage,oldage_r)
pop_oldage <- pop_oldage %>% mutate(oldage_score = oldage_r/136*100)


birth_r <- rank(pop_birth$birth)
pop_birth <- cbind(pop_birth,birth_r)
pop_birth <- pop_birth %>% mutate(birth_score = birth_r/136*100)


pop <- pop %>% mutate(pop_score = pop_change$change_score+pop_oldage$oldage_score+pop_birth$birth_score)
pop


finance_r <- rank(eco_finance$finance)
eco_finance <- cbind(eco_finance,finance_r)
eco_finance <- eco_finance %>% mutate(finance_score = finance_r/136*100)
eco_finance

business_r <- rank(eco_business$business)
eco_business <- cbind(eco_business,business_r)
eco_business <- eco_business %>% mutate(business_score = business_r/136*100)
eco_business


basic_r <- rank(-eco_basic$basic)
eco_basic <- cbind(eco_basic,round(basic_r,0))
eco_basic <- eco_basic %>% mutate(basic_score = basic_r/136*100)
eco_basic


eco <- eco %>%  mutate(eco_score=eco_finance$finance_score+eco_business$business_score+eco_basic$basic_score)
eco

oldhouse_r <- rank(-life_oldhouse$oldhouse)
life_oldhouse <- cbind(life_oldhouse,round(oldhouse_r,0))
life_oldhouse <- life_oldhouse %>% mutate(oldhouse_score = oldhouse_r/136*100)
life_oldhouse


subway_r <- rank(life_subway$subway)
life_subway <- cbind(life_subway,round(subway_r,0))
life_subway <- life_subway %>% mutate(subway_score = subway_r/136*100)
life_subway


parking_r <- rank(life_parking$parking)
life_parking <- cbind(life_parking,parking_r)
life_parking <- life_parking %>% mutate(parking_score = parking_r/136*100)
life_parking

life <- life %>%  mutate(life_score=life_oldhouse$oldhouse_score+life_subway$subway_score+life_parking$parking_score)
life


school_r <- rank(social_school$school)
social_school <- cbind(social_school,round(school_r,0))
social_school <- social_school %>% mutate(school_score = school_r/136*100)
social_school

culture_r <- rank(social_culture$culture)
social_culture <- cbind(social_culture,round(culture_r,0))
social_culture <- social_culture %>% mutate(curture_score = culture_r/136*100)
social_culture

hospital_r <- rank(social_hospital$hospital)
social_hospital <- cbind(social_hospital,hospital_r)
social_hospital <- social_hospital %>% mutate(hospital_score = hospital_r/136*100)
social_hospital


social <- social %>%  mutate(social_score=social_school$school_score+social_culture$curture_score+social_hospital$hospital_score)
social


score <- as.data.frame(cbind(구,pop$pop_score,eco$eco_score,life$life_score,social$social_score))
names(score) <- c("구", "pop_score","eco_score","life_score","social_score")
score$pop_score <- as.numeric(score$pop_score)
score$eco_score <- as.numeric(score$eco_score)
score$life_score <- as.numeric(score$life_score)
score$social_score <- as.numeric(score$social_score)


score <- score %>% mutate(total_score = score$pop_score+score$eco_score+score$life_score+score$social_score)
score

score <- cbind(score,id)
score
