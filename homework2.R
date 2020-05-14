#영희의 물리 실험 데이터 X,Y에 (x, u) = (7.7, 14.8), y = 7.7을 각각 추가한 후 모델링과 예측을 수행하세요.
library(scatterplot3d)

x <- c(3,6,3,6,7.7)
u <- c(10,10,20,20,14.8)
y <- c(4.65, 5.9, 6.7, 8.02,7.7)

m <- lm(y ~ x + u)
coef #y = 0.4363035x + 0.2084552u + 1.2329109

#-deviance()함수를 사용하여 평균 제곱 오차를 구하세요.
deviance(m)/length(x)

#-샘플 2개 ((7.5, 15.0), (5.0, 12.0))에 대한 예측을 수행하세요.
nx <- c(7.5, 15.0)
nu <- c(5.0, 12.0)
new_data <- data.frame(x=nx, u=nu)
ny <- predict(m, new_date)
ny
