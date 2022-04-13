set("C:\Users\yena\Documents\GitHub\data_viz\data") ##set working directory로 지정함
load(file="rfm.rda")
load(file="sale_cust.rda")

library(RColorBrewer)
with(rfm, boxplot(Amount~Class)) #Amount를 class별로
with(sale_cust, table(sex_flg, Class)) #table함수(분할표)
