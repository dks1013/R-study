#영희는 데이터의 양이 늘면 더욱 정확하게 모델링할 수 있다는 말을 듣고 데이터를 추가로 수집하여 다음 훈련 집합을 구성하였다.
#이 훈련 집합으로 "다중 선형 회귀" 전체 과정을 재현하세요.
#X = {(3, 10), (6, 10), (3, 20), (6, 20), (7.5, 5), (7.5, 10), (15, 12)}
#Y = {4.65, 5.9, 6.7, 8.02, 7.7, 8.1, 6.1}
#-예측 데이터 
#X = {(7.5, 5), (15, 12)}에 대한 예측값

X = c(3, 6, 3, 6, 7.5, 7.5, 15) #설명 변수 
u = c(10, 10, 20, 20, 5, 10, 12) #설명 변수 
Y = c(4.65, 5.9, 6.7, 8.02, 7.7, 8.1, 6.1) #반응 변수

data1<- data.frame(X, u, Y)

model <- lm(Y ~ X + u, data = data1)
model #y = 0.05718x + 0.04418u + 5.79747

#예측
new_data <- data.frame(X = c(7.5, 15),
                       u = c(5, 12))
predict(model, new_data)
