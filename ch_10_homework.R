x <- c(10.0, 12.0, 9.5, 22.2, 8.0) 
y <- c(360.2, 420.0, 359.5, 679.0, 315.3) 
m <- lm(y ~ x)
m #결과로 y=25.59x + 110.97을 알수 있음

#모델m 그래프(7-4)
plot(x, y)
abline(m, col='red')

fitted(m) #예측값
residuals(m) #잔차

deviance(m) #잔차 제곱함
deviance(m)/length(x) #평균 제곱 오차  

newx <- data.frame((x=c(10.5, 25.0, 15.0)))
predict(m, newdata = newx)
