install.packages("stringr")
library(stringr)
library(dplyr)
library(readr)
#데이터 불러오기 
x <- read_csv("d:/work/R/googleplaystore.csv")
glimpse(x)
#데이터 정제하기 
colnames(x)
x <- rename(x, "Content.Rating"= `Content Rating`)
x <- rename(x, "Last.Updated"= `Last Updated`)
x <- rename(x, "Current.Ver"= `Current Ver`)
x <- rename(x, "Android.Ver"= `Android Ver`)  #공백을 .으로 바꿈(공백으로 사용할 경우 안좋을 수 있음)
colnames(x)

View(x)
x$Price <- str_replace(x$Price, '\\$', '')  #$를 공백으로 
x$Size <- str_replace(x$Size, "Varies with device", "") #값대신 Varies with device를 공백으로
x$Size <- str_replace(x$Size, "M", "e6")  #M을 e6으로
x$Size <- str_replace(x$Size, "k", "e3")  #K를 e3으로 
View(x)

x$Size <- as.numeric(x$Size)  #size를 숫자형으로 

x$Installs <- str_replace(x$Installs, '\\+', '')  #Installs열에서 +를 공백으로 
x$Installs <- gsub(',','',x$Installs)   #1000단위 숫자 구분에 사용되는 ,구분 기호를 공백으로 
x$Installs <- as.numeric(x$Installs)  #숫자형으로 변경

dim(x)
table(is.na(x))

x <- na.omit(x)

#타입 변환
x$Category <- as.factor(x$Category) 
x$Reviews <- as.numeric(x$Reviews)
x$Type <- as.factor(x$Type)
x$Price <- as.numeric(x$Price)
x$Content.Rating <- as.factor(x$Content.Rating)
x$Genres <- as.factor(x$Genres)

dim(x)
table(is.na(x))

View(x)
library(lubridate)
x$Last.Updated <- mdy(x$Last.Updated) #날짜 데이터 형태로 저장
glimpse(x)

library(ggplot2)
x %>% ggplot(aes(Rating)) + geom_histogram()  #Rating 분포표로 히스토그램 그리기

x %>% ggplot(aes(Rating, fill = Type)) +
  geom_histogram(position = "dodge")  #무료와 유료를 나눈 히스토그램

x %>% ggplot(aes(Rating, col=Type)) + geom_density()  #무료와 유료의 밀도를 알아보기 위해 선그래프 출력

x %>% ggplot(aes(Reviews, Rating, col=Type)) +
  geom_point(alpha=0.2) + scale_x_log10() #리뷰 추가와 log스케일을 사용해서 편차를 줄인 그래프 

x %>% ggplot(aes(Reviews, Rating, col=Type, size=Installs))+
  geom_point(alpha=0.2) + scale_x_log10() #Installs추가로 앱 설치 횟수를 표현한 log스케일을 사용한 그래프
 
x %>%  ggplot(aes(Reviews, Rating, size=Installs))+
  geom_point(alpha=0.2) + scale_x_log10() + facet_wrap(~Type) #무료와 유로를 나누어서 출력

x %>% filter(Type=='Paid') %>% 
  ggplot(aes(Reviews, Rating, size=Installs)) +
  geom_point(alpha=0.2)+scale_x_log10() #유료앱만 출력

x %>% ggplot(aes(Reviews, Installs)) + geom_point(alpha=0.2) +
  scale_x_log10() + scale_y_log10() #리뷰와 설치 횟수 그래프

x %>% filter(Type=="Paid" & Price<5) %>% 
  ggplot(aes(Price)) + geom_histogram(binwidth = 0.01)  #유료 앰의 가격 분포를 5달러 이하 범위에서 확대 

x %>% filter(Type=="Paid") %>% 
  ggplot(aes(Price, Rating)) + geom_point(alpha = 0.2)  #평점과 가격의 관계 그래프 

x %>% filter(Type=="Paid") %>% 
  ggplot(aes(Price, Rating)) + geom_point(alpha = 0.2)+
  scale_x_log10() #로그스케일로 늘리기

x %>% filter(Type=="Paid" & Price < 50) %>% 
  ggplot(aes(Price, Rating)) + geom_point(alpha = 0.2)  #50달러 이상

#주요 카테고리 앱의 가격 분포
top4 <- x %>%  group_by(Category) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(4) #33개 카테고리 중 앱의 개수가 많은 4종류를 추출
top4

x %>% filter(Type=="Paid" & Price <50 & Category %in% top4$Category) %>% 
  ggplot(aes(Price, Rating)) + geom_point(alpha=0.2)+
  facet_wrap(~Category) #4개의 주요 카테고리 앱의 가격 분포 알아보기

x %>% ggplot(aes(Size, Rating)) + geom_point(alpha=0.1) #평점과 크기의 관계 그래프 

x %>% ggplot(aes(Content.Rating, Rating)) + 
  geom_point(alpha=0.1)+geom_jitter() #등급별로 평점을 나타낸 그래프 
  
x %>% filter(Content.Rating!="Adults only 18+") %>% 
  ggplot(aes(Rating, col=Content.Rating)) +
  geom_density()  #18세 이상은 제외한 밀도함수 그래프

x %>% ggplot(aes(Category, Rating)) +
  geom_point(size = 0.1, position = "jitter") +
  coord_flip()  #카테고리와 평점 그래프

#모델링
ps <- select(x, Rating, Category, Size, Content.Rating)
install.packages("rlang")
library(rlang)
library(caret)
library(dplyr)
data <- ps[sample(nrow(ps)),]
k <- 10
q <- nrow(data)/k
l <- 1:nrow(data)

pe_total <- c()
tot_mse <- 0

x$Reviews <- as.numeric(x$Reviews)

for(i in 1:k){
  test_list <- ((i-1)*q+1):(i*q)
  testData <- data[test_list,]
  train_list <- setdiff(l, test_list)
  trainData <- data[train_list,]
  
  m <- lm(Rating~., data=trainData)
  
  prd <- predict(m, newdata = testData)
  
  pe <- mean((prd - testData$Rating)^2)
  pe_total <- c(pe_total, pe)
  tot_mse <- sum(pe_total)
}
last_error()
summary(last_error())
pe_total
(average_mse <- tot_mse/k)

#결정트리
library(rpart)
library(randomForest)
pe2_total <- c()
tot2_mse <- 0

for(i in 1:k){
  test_list <- ((i-1)*q+1):(i*q)
  testData <- data[test_list,]
  train_list <- setdiff(l, test_list)
  trainData <- data[train_list,]
  
  rf <- randomForest(Rating ~., data=trainData, ntree = 20)
  
  prd2 <- predict(rf, newdata = testData)
  
  pe2 <- mean((prd2 - testData$Rating)^2)
  pe2_total <- c(pe2_total, pe2)
  tot2_mse <- sum(pe2_total)
  
}
pe2_total
(average_mse <- tot2_mse/k)