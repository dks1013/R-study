install.packages("ggplot2")
library(ggplot2)

#데이터 시각화 
gap1 <- gapminder::gapminder
gap1

plot(gap1$gdpPercap, gap1$lifeExp, col=gap1$continent)
legend("bottomright", legend = levels((gap1$continent)),
  pch = c(1:length(levels(gap1$continent))),
  col = c(1:length(levels(y$continent))))

#데이터의 직관적 이해(로그 스케일 사용)
plot(log10(gap1$gdpPercap), gap1$lifeExp, col=gap1$continent)
legend("bottomright", legend = levels((gap1$continent)),
       pch = c(1:length(levels(gap1$continent))),
       col = c(1:length(levels(y$continent))))

#ggplot 배경 설정
ggplot(data = mpg, aes(x = displ, y = hwy))

#그래프 추가하기
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

#히스토그램 작성 
ggplot(iris, aes(x = Sepal.Width, fill = Species, color = Species)) +
  geom_histogram(binwidth = 0.5, position = "dodge") +
  theme(legend.position = "top")

#산점도 작성
summary(iris)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3) +
  ggtitle("꽃잎의 길이")
theme(plot.title = element_text(size = 25, face = "bold",
                                colour = "steelblue"))

#그래프 범위 늘린것(summary확인후 설정)
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3) +
  ggtitle("꽃잎의 길이") +
  xlim(1, 8)+
  ylim(0, 3)+
theme(plot.title = element_text(size = 25, face = "bold",
                                colour = "steelblue"))

#월 별 강수량 막대그래프
library(ggplot)
library(dplyr)

df <- data.frame(month = c(1:6),
                 rain = c(55, 50, 45, 50, 60, 70))
df
ggplot(df, aes(x = month, y = rain)) + #막대 그래프
  geom_bar(stat = "identity",
           width = 0.7,
           fill = "steelblue")

#종에 따른 꽃잎 길이 평균
ggplot(iris, aes(x = Species, y = Petal.Length)) +
  geom_col()

#내림차순
ggplot(iris, aes(x = reorder(Species, -Petal.Length), y = Petal.Length)) +
  geom_col() +
  xlab("Species")

#빈도 막대 그래프
ggplot(iris, aes(x = Species)) +
  geom_bar(width = 0.5)

#cyl 종류별 gear빈도 누적 막대 그래프
mtcar <- mtcars
mtcar$cyl <- as.factor(mtcar$cyl)

ggplot(mtcar, aes(x = cyl)) +
  geom_bar(aes(fill=factor(gear)))

#그룹별 상자 그림
ggplot(iris, aes(y=Petal.Length, fill = Species)) +
  geom_boxplot()

#선 그래프
df2 <- data.frame(year = c(1937:1960),
                  cnt = as.vector(airmiles))
ggplot(df2, aes(x = year, y = cnt)) +
  geom_line(col = "red") +
  geom_point()

#대륙별로 기대 수명 70이상인 나라 그래프
gapminder %>% filter(lifeExp > 70) %>%  group_by(continent) %>% 
  summarise(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n)) + geom_bar(stat = "identity")

#2007년 나라별 평균 수명(대륙별로 색깔 구분)
gapminder %>%  filter(year == 2007) %>% 
  ggplot(aes(lifeExp, col = continent)) +
  geom_histogram()

#dodge속성 사용(옆으로 나란히)
gapminder %>%  filter(year == 2007) %>% 
  ggplot(aes(lifeExp, col = continent)) +
  geom_histogram(position = "dodge")

#빈도 막대그래프 사용한 것
gapminder %>%  filter(year == 2007) %>% 
  ggplot(aes(continent, lifeExp, col = continent)) +
  geom_boxplot()

#산점도 그래프
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = continent)) +
  geom_point(alpha = 0.2) + scale_x_log10() #x스케일 조정 alpha -> 불투명도 

#아프리카 국가별 기대수명 
gapminder %>% filter(continent == "Africa") %>% 
  ggplot(aes(country, lifeExp)) +
  geom_bar(stat = "identity")

#coord_filp()로 플롯의 방향을 전환
gapminder %>% filter(continent == "Africa") %>% 
  ggplot(aes(country, lifeExp)) +
  geom_bar(stat = "identity") +
  coord_flip()

#좀 더 다양한 색상 사용하기 
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()

#기대 수명 70초과 대륙별 막대그래프 (기본 색상)
gapminder %>% filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n)) +
  geom_bar(stat = "identity", aes(fill = continent))

#색상 조정(Spectral색상 사용)
gapminder %>% filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n)) +
  geom_bar(stat = "identity", aes(fill = continent)) +
  scale_fill_brewer(palette = "Spectral")

#색상 조정(Blues색상 사용)
gapminder %>% filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n)) +
  geom_bar(stat = "identity", aes(fill = continent)) +
  scale_fill_brewer(palette = "Blues")

#색상 조정(Oranges색상 사용)
gapminder %>% filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n = n_distinct(country)) %>% 
  ggplot(aes(x = continent, y = n)) +
  geom_bar(stat = "identity", aes(fill = continent)) +
  scale_fill_brewer(palette = "Oranges")

#막대그래프를 내림차순으로 정렬
gapminder %>% filter(lifeExp > 70) %>% 
  group_by(continent) %>% 
  summarise(n = n_distinct(country)) %>% 
  ggplot(aes(x = reorder(continent, -n), y = n)) + #reorder함수 사용
  geom_bar(stat = "identity", aes(fill = continent)) +
  scale_fill_brewer(palette = "Oranges")

#많은 양의 데이터를 효과적으로 관찰 하는 방법 
ggplot(gap1, aes(x = gdpPercap, y = lifeExp, col = continent)) +
  geom_point() + scale_x_log10()
#인구에 따라서 원의 크기를 다르게 함
ggplot(gap1, aes(x = gdpPercap, y = lifeExp, col = continent, size=pop)) +
  geom_point() + scale_x_log10()
#원이 가려지는 것을 막기 위한 투명도 설정
ggplot(gap1, aes(x = gdpPercap, y = lifeExp, col = continent, size=pop)) +
  geom_point(alpha = 0.5) + scale_x_log10()
#관측 연도별로 구분 
ggplot(gap1, aes(x = gdpPercap, y = lifeExp, col = continent, size=pop)) +
  geom_point(alpha = 0.5) + scale_x_log10() + facet_wrap(~year)

#데이터를 여러 관점에서 보기
#1952년 아시아 대륙의 인구 분포에서 각 국가의 순위를 매기기
gapminder %>% filter(year == 1952 & continent == "Asia") %>% 
  ggplot(aes(reorder(country, pop), pop)) +
  geom_bar(stat = "identity") +
  coord_flip()
#동일 결과 코드
gapminder %>% filter(year == 1952 & continent == "Asia") %>% 
  ggplot(aes(x = reorder(country, pop), y = pop)) +
  geom_col() +
  coord_flip()

#로그 스케일사용
gapminder %>% filter(year == 1952 & continent == 'Asia') %>% 
  ggplot(aes(reorder(country, pop), pop)) +
  geom_bar(stat = "identity") +
  scale_y_log10() +
  coord_flip()
#동일 결과 코드
gapminder %>% filter(year == 1952 & continent == "Asia") %>% 
  ggplot(aes(x = reorder(country, pop), y = pop)) +
  geom_col() +
  scale_y_log10() +
  coord_flip()


#선그래프(한국의 기대수명 변화를 연도에 따라 시각화)
gapminder %>% filter(country=="Korea, Rep.") %>% 
  ggplot(aes(year, lifeExp, col = country)) +
  geom_line() +
  geom_point()

#산점도 그래프(추세선 추가)
#여러 데이터의 변화를 동시에 비교하기
gapminder %>%
  ggplot(aes(x = year, y = lifeExp, col = continent)) +
  geom_point(alpha = 0.2) +
  geom_smooth() #추세선 추가

#1952년 전 세계 기대수명 분포 시각 화 
#hist()함수 사용 
x <- filter(gapminder, year == 1952)
hist(x$lifeExp, main = "Histogram of lifeExp in 1952")

#ggplot 라이브러리 geom_histogram()사용 
x %>% ggplot(aes(lifeExp)) + geom_histogram()

x %>% ggplot(aes(lifeExp)) +
  geom_histogram(fill = "white", colour = "black")

x %>% ggplot(aes(lifeExp)) +
  geom_histogram(binwidth = 5, fill = "white", colour = "black")

#상관 관계
#1인당 국내 총생산과 기대수명 간의 상관 관계 시각화
plot(log10(gapminder$gdpPercap), gapminder$lifeExp)

gapminder %>% 
  ggplot(aes(gdpPercap, lifeExp)) +
  geom_point(alpha=0.2, size=2) +
  scale_x_log10()

#베이스R
vis_air <- airquality
plot(vis_air$Temp, vis_air$Ozone, xlab = "온도", ylab = "오존",
     main = "온도와 오존")

plot(vis_air$Temp, vis_air$Ozone, xlab = "온도", ylab = "오존",
     main = "온도와 오존", pch = 5, cex=1.2, col = "navy") #표식 5번, 크기 1.2, 색깔 navy

plot(vis_air$Temp, vis_air$Ozone, xlab = "온도", ylab = "오존",
     main = "온도와 오존", pch = 5, cex=1.2, col = "navy",
     xlim=c(50, 100), ylim=c(0,180)) #그래프 범위 조정

plot(cars, type = "l", main = "cars") #ㅣ타입
plot(cars, type = "b", main = "cars") # b타입 

#문자열 text함수
plot(4:6, 4:6, cex = 1.5, col = "red", pch = 0)

text(5, 5, "00", adj = c(0, 0))
text(5, 5, "01", adj = c(0, 1))
text(5, 5, "10", adj = c(1, 0))
text(5, 5, "11", adj = c(1, 1))

#1952년 아시아 국가들의 gdp 구성과 비율 상위 6개 
x <- gapminder %>% 
  filter(year == 1952 & continent == "Asia") %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  select(country, gdp) %>% 
  arrange(desc(gdp)) %>% 
  head()

pie(x$gdp, x$country) #원형 그래프
barplot(x$gdp, names.arg = x$country) #막대 그래프 

#matplot함수(다중 플롯을 빠르게 구현)
matplot(iris[, 1:4], type = "l")
legend("topleft", names(iris)[1:4], lty = c(1:4), col = c(1:4))

#hist함수
hist(iris$Sepal.Width)