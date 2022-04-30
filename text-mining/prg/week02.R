s <- read.csv('product.csv', header=T, sep=',', encoding='latin1', na.strings ='-')
s <- read.csv(file.choose(), header=T, sep=',', encoding='latin1', na.strings='-') #위의 코드에서 파일이름을 file.choose()로 바꾸면 됨
write.table(s, 'textmining.csv', row.names=F, quote=F, sep=',')

install.packages("pander") #R환경을 벗어나지 않고도 파일 open 가능
library(pander)
openFileInOS("product.csv")
openFileInOS("product-with-no-header.csv")  
openFileInOS("product-missing.csv")
openFileInOS("product.txt") #구분자가 공백으로 되어있는 텍스트
openFileInOS("product-fwf.txt") #고정된 너비의 파일일 id - 5개의 필드, name-10개의 필드, price-8개의 필드드
openFileInOS("won-dollar.txt") #비정형형태의 데이터


install.packages("readr")
library(readr)

read_csv(file = "product.csv")

read_csv(file = "product-with-no-header.csv",   #그냥 읽어오면 첫번째행을 열의 이름으로 지정함
         col_names = c("id", "name", "price"))#열 이름 지정

read.csv(file = "product-missing.csv",          #그냥 불러오면 결측값이 그대로 불러와짐
         na = ".")                              # 결측치처리

read_delim("product.txt", delim = " ")  #공백을 구분자로 해서 불러옴

read_delim("product-with-no-header.csv",
           delim = ",",
           col_names = c("id", "name", "price"))

fwf_empty("product-fwf.txt")
fwf_positions()
fwf_widths()
fwf_cols()
#이 함수들을 이용해서 파일의 위치 파악
read_fwf(file = "product-fwf.txt",
         col_positions = fwf_empty(file = "product-fwf.txt", #공백의 위치파악
                                   col_names = c("id", "name", "price")))
#각각의 칸의 너비를 알고 있을 때, 위의 결과와 동일하게 불러올 수 있음
read_fwf(file = "product-fwf.txt",
         col_positions = fwf_widths(widths = c(5, 10, 8),
                                    col_names = c("id", "name", "price")))

read_table(file = "product-fwf.txt", #공백이 여러칸으로 구성된 데이터 형태 -> read_table로 읽어들이는 게 보편적 => 공백이 얼마던, 공백을 기준으로 데이터를 구분하겠다
           col_names = c("id", "name", "price"))
read_table(file = "product.txt")

read_lines(file = "won-dollar.txt") #5개가 불러와짐
read_lines(file = "won-dollar.txt", skip = 1) #첫번째 데이터 생략하고, 두번째 데이터부터
read_lines(file = "won-dollar.txt", skip = 1, n_max = 2) #n_max = 2: 위에서부터 2개만 가져오겠다

read_file(file = "won-dollar.txt") #하나의 인자로 불러들임

#csv 파일로 저장
write_csv(x = Orange, file = "orange.csv")
read_csv(file = "orange.csv")
write_delim(x = Orange, file = "orange.txt", delim = ";") #특정 구분자로 저장
read_delim(file = "orange.txt", delim = ";")

parse_number("$100") #숫자만 뽑아내고 싶을때
class(parse_number("$100")) #=> numeric
parse_number("10%") #=> 1
parse_number("salary per year : $30,000")


##### 2-2 list indexing #####

#[[]] vs [] : 데이터의 유형을 그대로 유지하느냐 아니냐

product <- list("A001", "Mouse", 30000)
product

product[[3]] #list의 3번째 자루에 들어있는 실제 데이터 '값'(자루 안에 들어있는 vector 그 자체를 가져옴)
product[[2]]
product[[1]]

product[3] #list자체를 가져온 것

class(product[[3]]) #numeric
class(product[3]) #list
class(product[[2]]) #character
class(product[2]) #list
class(product[[1]]) #character
class(product[1]) #list

product[3]*0.8 #error : list에 숫자 곱하기 .. ?
product[[3]]*0.8

#여러개 요소 추출 => c() 이용
product[c(1, 2)] #1,2번째 자루 추출
product[c(FALSE, TRUE, TRUE)] #논리연산자 이용 - 2,3번째 추출
product[-1] #특정원소제외
product[-2]

#이름 부여
product1 <- list(id="A001", name="Mouse", price=30000)
product1

product1[["name"]] #이름으로 접근
class(product1[["name"]]) #character
product1["name"]
class(product1["name"]) #list

product1$name
class(product1$name) #character

product1[c("name", "price")] #list 자체 리턴
product1[[c("name", "price")]] #error : subscript out of bounds
         
product1[["holder"]] # => NULL
product1$holder # => NULL
product1[[4]] #error : subscript out of bounds

product[c(4,2,5)] #있는 값만 나옴. 없는 값은 NULL

#list - 중첩구조가능
list <- list(one = 1, two = 2, three = list(alpha = 3.1, beta = 3.2))
list #세번째 자루 안에 두 개의 자루가 들어가 있다

#3.1을 추출하고 싶다면 -> 개별 원소니까 대괄호 두 개,
list[["three"]]
list[["three"]][["alpha"]]
list$three$alpha
list[["three"]]$alpha #혼용 가능
list$three[["alpha"]]

