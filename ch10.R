library(dplyr)
library(ggplot2)
library(gapminder)

#시각화의 본질
#세계 여러나라의 경제 및 복지 수준과 이의 변화를 시각화

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp, col = continent)) +
  geom_point(alpha = 0.2) +
  facet_wrap(~year) + scale_x_log10()

#1952년 1인당 gdp가 10000이상인 아시아 국가
gapminder %>%  filter(year == 1952 &
                        gdpPercap > 10000 &
                        continent == "Asia")

#1952년 1인당 gdp가 매우 높은 아시아 국가 하나 발견 
#gdp는 인구의 영향을 받는 항목이므로 1952년 이후 gdp 및 pop변화를 시각화함 
#gdp변화 
gapminder %>%  filter(country == "Kuwait") %>%
  ggplot(aes(year, gdpPercap)) +
  geom_point() + geom_line()

#인구변화
gapminder %>% filter(country == "Kuwait") %>% 
  ggplot(aes(year, pop)) +
  geom_point() + geom_line()

#쿠웨이트와 한국의 양상 비교
#한국의 gdp변화 
gapminder %>%  filter(country == "Korea, Rep.") %>% 
  ggplot(aes(year, gdpPercap)) +
  geom_point() + geom_line()

#한국의 인구 변화 
gapminder %>%  filter(country == "Korea, Rep.") %>% 
  ggplot(aes(year, pop)) +
  geom_point() + geom_line()

#gdp와 pop을 효과적으로 관찰하기 위해 gdp(진짜) 추가
#nutate 사용
gapminder %>% filter(country == "Kuwait" |country == "Korea, Rep.") %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  ggplot(aes(year, gdp, col = country)) +
  geom_point() + geom_line()

#공업국가 원유국가 비교 
compare_country <- c("Kuwait", "Saudi Arabia", "Ireaq",
                     "Iran", "Korea, Rep", "China", 
                     "Japan")
#gdpPercap 변화
gapminder %>% 
  filter(country %in% compare_country) %>% 
  ggplot(aes(year, gdpPercap, col = country)) +
  geom_point() + geom_line()
#pop 변화
gapminder %>% 
  filter(country %in% compare_country) %>% 
  ggplot(aes(year, gdpPercap, col = country)) +
  geom_point() + geom_line() +
  scale_y_log10()
#gdp 변화
gapminder %>% 
  filter(country %in% compare_country) %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  ggplot(aes(year, gdp, col = country)) +
  geom_point() + geom_line() +
  scale_y_log10()

#모델링과 예측
#회귀함수 사용 

#물리실험 예제 데이터 사용
x <- c(3.0, 6.0, 9.0, 12.0) #독립변수 
y <- c(3.0, 4.0, 5.5, 6.5) #종속 변수
m <- lm(y ~ x)
m #결과로 y=0.4x + 1.75을 알수 있음 

#모델 m을 그림으로 그리기
plot(x, y)
abline(m, col="red")

#모델의 특성 살펴보기
coef(m) #회귀계수(절편, 기울기)
fitted(m) #샘플에 대한 예측값
residuals(m) #잔차를 알려줌 
deviance(m)/length(x) #deviance-잔차 제곱함

#summary함수로 모델 상세히 살피기
summary(m)

#예측
#predict함수
#1.2, 2.0, 20.65라는 3개의 새플이 새로 발생 했다고 가정
newx <- data.frame((x=c(1.2, 2.0, 20.65)))
predict(m, newdata = newx)

plot(x, predict(m, newdata = newx), xlim = c(1,21),
     pch = 16, col = 'red')
abline(m)