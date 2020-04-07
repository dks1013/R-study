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

write.csv(fruit, file = "C:/data/fruit.csv", quote = FALSE)
##fruit데이터프레임을 csv파일로 저장하기 

install.packages("readxl")
library(readxl)
air_df <- data.frame(read_xlsx("C:/data/air.xlsx"))
air_df
## air 엑셀파일 불러와서 데이터 프레임 만들기

cold <- nrow(air_df)  ##nrow 데이터 프레임의 행의 수
condition <- c()
for(i in 1:cold){     ##행의 수 까지만 반복
  if(air_df$Temp[i] <= 64){
    condition[i] <- "C"
  }else if(air_df$Temp[i] >= 78){
    condition[i] <- "H"
  }else{
    condition[i] <- "F"
  }
}
air_df$condition <- condition   ## condition 데이터 프레임에 추가
air_df
## air_df 데이터 프레임에서 Temp가 64이하이면 C, 78이상이면 H, 
## 나머지는 F로 분류하여 condition 벡터를 생성한 후 air_df에 추가