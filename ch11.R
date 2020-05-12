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

