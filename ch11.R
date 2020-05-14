#car 모델링
car_model <- lm(dist ~ speed, data = cars)
coef(car_model) #최적 모델 dist = 3.932409x + (-17.579095)

plot(cars)
abline(car_model, col = 'red')

fitted(car_model) #예측 훈련


residuals(car_model) #잔차 확인

#시속 21.5로 달리고 있었다면 제동 거리가 얼마일까?
nx1 <- data.frame(speed=21.5)
predict(car_model, nx1)

#각 스피드의 제동 거리
nx2 <- data.frame(speed = c(25.0, 25.5, 26.0,
                            26.5, 27.0, 27.5, 28.0))
predict(car_model, nx2)

#예측값으로 그래프 그리기
plot(nx2$speed, predict(car_model, nx2),
                        cex=2, pch=20)
abline(car_model)

#lm에 고차 방정식 적용
#lm은 기본적으로 1차 방정식, 직선모델 적합 
plot(cars, xlab='속도', ylab='거리')
x <- seq(0, 25, length.out = 200)
for (i in 1:4) {
  m <- lm(dist~ poly(speed, i), data = cars) #poly옵션을 사용하면 고차 방정식 적용 가능
  assign(paste('m', i, sep = '.'), m)
  lines(x, predict(m, data.frame(speed=x)), col = i)
}

#anova함수로 분산분석 하기 
anova(m.1, m.2, m.3, m.4)
#Pr(>F), 즉 p-값은 모두 0.05보다 커서 통계적으로 차이가 없다고 판정할 수 있음(가장 단순한 1차 모델->m.1을 사용 권장)

#women데이터로 모델의 통계량 해석
#모델링, 시각화
women_model1 <- lm(weight ~ height, data = women)
coef(women_model1) # y = 3.45x + (-87.51667)

plot(women)
abline(women_model1, col='red')

summary(women_model1)
#Pr(>|t|) -> 1.09e-14...는 0.05보다 작아서 유의미 하다는 것을 알 수 있음
#Adjusted R-squared를 보면 높은 확률로 예측이 가능하다는 것을 알 수 있음

#cars데이터 모델
car_model <- lm(dist ~ speed, data = cars)
summary(car_model)
#Pr(>|t|)를 보니  0.0123. 이고 0.05이하의 값이 나오기 때문에 유의미 함 
#Adjusted R-squared를 보면 women모델 보다 낮은 확률로 예측이 가능하다는 것을 알 수 있음

#영희의 물리 실험
install.packages("scatterplot3d")
library(scatterplot3d)

x <- c(3,6,3,6)
u <- c(10,10,20,20)
y <- c(4.65, 5.9, 6.7, 8.02)

m <- lm(y ~ x + u)
coef(m) #y = 0.4283333x + 0.2085u + 1.262

fitted(m) #예측값
residuals(m) #잔차
deviance(m) #잔차합 
deviance(m)/length(x) #잔차 평균

#샘플 2개 ((7.5, 15.0), (5.0, 12.0))에 대한 예측 수행
nx <- c(7.5, 15.0)
nu <- c(5.0, 12.0)
new_data <- data.frame(x=nx, u=nu)
ny <- predict(m, new_data)
ny

#tree데이터
str(trees)
summary(trees)
#Girth 나무의 지름, Height 나무의 키,  Volume 목재의 부피

#설명 변수 Girth, Height, 반응 변수 Volume
model <- lm(Volume ~ Girth + Height, data = trees)
model

new_data <- data.frame(Girth = c(8, 13, 19),
                       Height = c(72, 86, 85))
predict(model, new_data)

#변수 선별 
install.packages("car")
library(car)
install.packages("MASS")
library(MASS)

newdata <- Prestige[, c(1:5)]
head(newdata)

mod2 <- lm(income ~ education + prestige + 
             women + census, data = newdata)
#변수 선택 stepAIC()
mod3 <- stepAIC(mod2)
mod3
summary(mod2)
summary(mod3)

#일반화 선형 모델
#영희의 머플러 장사
discount <- c(2, 4, 6, 8, 10)
profit <- c(0, 0, 0, 1, 1)
muffler = data.frame(discount, profit)
plot(muffler, pch = 20, cex = 2, xlim =c(0, 12))

#a선형 모델을 적용 
lmodel <- lm(profit ~ discount, data = muffler)
coef(lmodel) #y = 0.15x + (-0.5)
fitted(lmodel) #-0.2  0.1  0.4  0.7  1.0 -> 5번쨰만 유효 
residuals(lmodel)
deviance(model)

#예측값 
newdt <- data.frame(discount = c(1, 5, 12, 20, 30))
p <- predict(lmodel, newdt)
p

#선형 모델을 적용한 예측값을 사용한 결과 그래프
plot(muffler, pch = 20, cex = 2, xlim = c(0, 32),
     ylim = c(-0.5, 4.2))
abline(lmodel)
res <- data.frame(discount = newdt, profit = p)
points(res, pch = 20, cex = 2, col = 'red')
legend("bottomright", legend = c("train data", "new data"),
       pch = c(20, 20), cex = 2,
       col = c("black", "red"), bg="gray")

#glm사용
discount <- c(2, 4, 6, 8, 10)
profit <- c(0, 0, 0, 1, 1)
muffler = data.frame(discount, profit)
g <- glm(profit ~ discount, data = muffler, family = binomial)

coef(g)

fitted(g)

residuals(g)

deviance(g)

plot(muffler, pch = 20, cex = 2)
abline(g, col = 'blue', lwd = 2)

newdt <- data.frame(discount = c(1, 5, 12, 20, 30))
p <- predict(g, newdt)
p

p <- predict(g, newdt, type = 'response')
p

#Haberman survival 데이터
haberman <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data", header = FALSE)

names(haberman) <- c('age', 'op_year', 'no_nodes', 'survival') #열이름 지정
str(haberman)

haberman$survival <- as.factor(haberman$survival) #survival을 팩터값으로 설정
str(haberman)

h <- glm(survival ~ age + op_year + no_nodes, data = haberman, family = binomial)
coef(h)
deviance(h)
fitted(h)
summary(h) #0.05보다 다 크기떄문에 고만고만 하다.

age <- c(37, 66)
op_year <- c(58, 60)
no_nodes <- c(5, 32)

new_patients <- data.frame(age, op_year, no_nodes)

predict(h, newdata = new_patients, type = 'response')
