product <- c("사과", "딸기", "수박")
price <- c(1800, 1500, 3000)
salecnt <- c(24, 38, 13)

fruit <- data.frame(product, price, salecnt)
str(fruit)
## fruit 데이터 프레임 만든 코드 

fruit[1,]
## 사과 행 데이터만 뽑아 출력한 코드 

fruit[c(1,3)]
## product와 salecnt 한번에 출력 코드

sapply(c(list(fruit$price),list(fruit$salecnt)), mean)
## price와 salecnt의 평균 출력 코드 

product <- c("사과", "딸기", "수박")
price <- c(1800, 1500, 3000)
salecnt <- c(24, 38, 13)

fruit <- data.frame(product, price, salecnt, stringsAsFactors = FALSE)
str(fruit)
## 데이터 프레임 문자열 살리는 코드

attach(fruit)
fruit$totprice <- price*salecnt
detach(fruit)
fruit
## totprice 파생변수(price*salecnt) 만든 코드 

