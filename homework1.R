#women모델링
#women 데이터를 복사하여 women1을 만들어 사용함
#이상값으로 height=65.5, weight=121 샘플을 추가함
height <- (65.5)
weight <- (121)
df1 <- data.frame(height, weight)

women1 <- rbind(df1, women) #rbind함수 사용
women1 #이상값 확인

#새로운 데이터를 훈련 집합으로 lm으로 모델링함
women_model1 <- lm(weight ~ height, data = women1)
coef(women_model1) #y = 3.420797x + (-86.708726)

#모델을 시각함
plot(women1)
abline(women_model1, col='red')

#통계량을 분석하고 원래 women데이터와 비교하고 해석함
women_model <- lm(weight ~ height, data = women)
coef(women_model) #y = 3.45000x + (-87.51667)

#women모델을 시각화
plot(women)
abline(women_model, col = 'red')

summary(women_model)
 
summary(women_model1) #비교 분석 
