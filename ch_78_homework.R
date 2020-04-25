# 8주차 강의에서 2017년 데이터 정리 방법과 동일한 방법으로 
#2016년 데이터를 정리하여 NHIS2016M 데이터로 지정하기

#건강 검진 데이터 불러오기 
NHIS2016 <-
  read.csv("C:/work/R/NHIS_OPEN_GJ_2016.csv",
           fileEncoding = "EUC-KR")
View(NHIS2016)
table(is.na(NHIS2016$흡연)) #흡연에 NA가 있는지 확인

#결측값을 제거
NHIS2016M <- NHIS2016[1:26] #기준년도 ~ 흡연상태 열까지만 저장
NHIS2016M <- na.omit(NHIS2016M) #결측값 제거
View(NHIS2016M)
table(is.na(NHIS2016M$흡연)) #NA값이 제거된것을 확인 


#2016년, 2107년 데이터에 대한 요약 통계값을 데이터 프레임 변수
#하나에 병합하여 NHIS2016_7M으로 저장하기 

#2017년 데이터도 2016년 데이터처럼 정리 해준다.
NHIS2017 <-
  read.csv("C:/work/R/NHIS_OPEN_GJ_2017.csv",
           fileEncoding = "EUC-KR")
View(NHIS2017)
table(is.na(NHIS2017$흡연))

NHIS2017M <- NHIS2017[1:26]
NHIS2017M <- na.omit(NHIS2017M)
View(NHIS2017M)
table(is.na(NHIS2017M$흡연))

#데이터 병합
NHIS2016_7M <-
  merge(NHIS2016M, NHIS2017M, all = T)
View(NHIS2016_7M)


#각 대륙의 연도별 gdpPercap의 평균값을 데이터로 추출한 후
#해당 데이터를 사용하여 그래프를 그리고 범례를 원하는 위치에 추가하기 
library(gapminder)
library(dplyr)

gdp <- gapminder %>% 
  group_by(year, continent) %>% 
  summarise(m_pop = mean(pop))
head(gdp, 20)

plot(gdp$year, gdp$m_pop, col = gdp$continent,
     pch = c(1:nlevels(gdp$continent)))

legend("topleft", legend = levels(gdp$continent),
       pch = c(1:nlevels(gdp$continent)),
       col = c(1:nlevels(gdp$continent)))


#1957년 gdpPercap과 lifeExp의 대륙별 평균을 데이터로 추출한 후,
#해당 데이터를 사용하여 가로축에서 continent, 세로축에서는 gdpPercap 
#평균값을 나타낸 그래프로 시각화 하기 

life <- gapminder %>% 
  filter(year == "1957") %>% 
  group_by(year, continent) %>%
  summarise(m_lifeExp = mean(lifeExp),
            m_gdp = mean(gdpPercap))
head(life, 20)

plot(life$continent, life$m_gdp)
