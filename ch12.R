logiiris <- iris
str(logiiris)

logiiris$Species <- as.integer(logiiris$Species)
head(logiiris)

#모델 만들기 
modeliris <- glm(Species ~. , data = logiiris)

#예측하기
ndata <- data.frame(rbind(c(5.1, 3.5, 1.4, 0.2))) #rbind행결합
names(ndata) <- names(logiiris)[1:4]
ndata

pred <- predict(modeliris, newdata = ndata)
pred

pred <- round(pred, 0)  #반올림
levels(iris$Species)
levels(iris$Species)[pred]

#여러가지 예측
test <- iris[, 1:4]
pred <- predict(modeliris, newdata = test)
pred <- round(pred, 0)
pred

answer <- as.integer(iris$Species)
pred == answer
#예측 정확도 계산
acc <- mean(pred == answer)
acc #예측의 정확도 97퍼센트

#UCLA admission데이터(ucla대학원 입학 데이터)
ucla <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(ucla)
#시각화 
plot(ucla)
#ggplot을 이용한 그래프를 보다 정교하게 시각화 
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
#1
ucla %>% ggplot(aes(gre, admit)) + geom_point()
#2
ucla %>% ggplot(aes(gre, admit)) + geom_jitter()
#3
ucla %>% ggplot(aes(gre, admit)) + geom_jitter(aes(col = admit))
#4
ucla %>% ggplot(aes(gre, admit)) +
  geom_jitter(aes(col = factor(admit)))
#5
ucla %>% ggplot(aes(gre, admit)) +
  geom_jitter(aes(col = factor(admit)), height = 0.1, width = 0.0)

#gridExtra 라이브러리를 사용하여 한 행에 2개 그래프 표현
install.packages("gridExtra")
library(gridExtra)

p1 <- ucla %>% ggplot(aes(gpa, admit)) +
  geom_jitter(aes(col = factor(admit)), height = 0.1,
              width = 0.0)

p2 <- ucla %>% ggplot(aes(rank, admit)) +
  geom_jitter(aes(col = factor(admit)), height = 0.1,
              width = 0.1)
grid.arrange(p1, p2, ncol = 2)


#glm 적용
m <- glm(admit ~., data = ucla, family = binomial)
coef(m)
summary(ucla)
#예측하기 
s <- data.frame(rbind(c(376, 3.6, 3)))
names(s) <- names(ucla)[2:4]
predict(m, s, type = 'response')  #response 1과 0사이의 값을 얻기 위해 

#colon 데이터 (환자 치료 데이터)
install.packages("survival")
library(survival)
str(colon)
View(colon)
plot(colon)
#시각화하기
p1 <- colon %>% ggplot(aes(extent, status)) +
  geom_jitter(aes(col = factor(status)), height = 0.1,
              width = 0.1)
p2 <- colon %>% ggplot(aes(age, status)) +
  geom_jitter(aes(col = factor(status)), height = 0.1,
              width = 0.1)
p3 <- colon %>% ggplot(aes(sex, status)) +
  geom_jitter(aes(col = factor(status)), height = 0.1,
              width = 0.1)
p4 <- colon %>% ggplot(aes(nodes, status)) +
  geom_jitter(aes(col = factor(status)), height = 0.1,
              width = 0.1)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

table(is.na(colon))
clean_colon <- na.omit(colon)
View(clean_colon)
clean_colon <- clean_colon[c(TRUE, FALSE),]
m <- 
  glm(status ~ rx + sex + age + obstruct + perfor + adhere +
        nodes + status + differ + extent + surg + node4,
      data = clean_colon, family = binomial)
m

#결정 트리 함수
library(rpart)
r <- rpart(Species ~., data = iris)
printcp(r)

par(mfrow = c(1,1), xpd = NA) #xpd=NA 그림이 클 경우 그래픽 장치 내부에 그림 출력 
plot(r)
text(r, use.n = TURE)

#predict함수를 사용하여 예측 
p <- predict(r, iris, type = 'class')
table(p, iris$Species)

#rpart옵션 이용하기
r_prior <- rpart(Species ~., data = iris,
                 parms = list(prior = c(0.1, 0.1, 0.8)))
plot(r_prior)
text(r_prior, use.n = TRUE)

#결정 트리로 예측하기
newd <- data.frame(Sepal.Length = c(5.11, 7.01, 6.32),
                   Sepal.Width = c(3.51, 3.2, 3.31),
                   Petal.Length = c(1.4, 4.71, 6.02),
                   Petal.Width = c(0.9, 1.4, 2.49))
print(newd)
predict(r, newdata = newd)