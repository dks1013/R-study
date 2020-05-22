##iris 4개의 설명 변수 중 Petal.Length를 제외한 3개만 사용하여 결정 트리 모델을 구하세요.
##-트리 시각화 하기
##-table함수를 사용하여 혼동 행렬을 구함

##iris의 사전 확률이 setosa:versicolor:virginica = 10:80:10이라 가정하고 결정 트리를 구하세요
##-사전확률이 10:10:80인 경우와 무엇이 달라졌는지와 이유를 설명하세요.
View(iris)
library(rpart)
r <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Width,  data = iris)
printcp(r)
#시각화 
par(mfrow = c(1,1), xpd = NA)
plot(r)
text(r, use.n = TRUE)
#혼동행렬 출력
p <- predict(r, iris, type = 'class')
table(p, iris$Species)
#예측
r_prior <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Width, data = iris,
                 parms = list(prior = c(0.1, 0.8, 0.1)))
plot(r_prior)
text(r_prior, use.n = TRUE)
