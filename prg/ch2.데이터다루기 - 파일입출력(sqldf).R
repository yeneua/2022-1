# ch.2데이터 다루기
# 2. 파일입출력
#sqldf

install.packages("sqldf")
library(sqldf)

warpbreaks
head(warpbreaks)

sqldf("select * from warpbreaks")
sqldf("select * from warpbreaks limit 3") #3행까지만
sqldf("select * from warpbreaks where tension = 'L'")
sqldf("select * from warpbreaks where tension = 'M'")
sqldf("select * from warpbreaks where tension ='L' 
      union all select *from warpbreaks where tension ='M'") #tension이 L,M인 것 union해서추축
sqldf("select count(*)
      from warpbreaks
      where tension='L'") #통계함수적용

iris
sqldf("select distinct Species
      from iris")
sqldf("select * from iris
      where Species = 'virginica'")
sqldf("select Species, sum('Sepal.Length') as SepalLength #반드시 안에는 작은따옴표
      from iris
      group by Species")
sqldf('select "Sepal.Length"
      from iris
      order by "Sepal.Length" asc #오름차순
      limit 20')
sqldf('select "Sepal.Length"
      from iris
      order by "Sepal.Length" desc  #내림차순
      limit 20') #20개 행까지만
sqldf("select * from iris limit 10")
sqldf('select avg("Petal.Length") as avg, stdev("Petal.Length") as sd
      from iris
      group by Species')
