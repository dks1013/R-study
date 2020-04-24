library(gapminder)
library(dplyr)

#한국, 중국, 일본 세 나라의 1인당 국내총생산과 기대 수명을
#전체 관측 기간에 걸쳐 나란히 출력
#(열(변수)들을 나라별로 연도가 가장 최근 
#데이터부터 나타나게 출력하세요.)

gapminder %>%
  filter(country %in% c("Korea, Rep.", "China", "Japan")) %>%
  select(country, gdpPercap, lifeExp, year) %>%
  arrange(country, desc(year))

#아프리카 대륙의 총인구가 유럽의 총인구 보다 
#많았던 해를 모두 출력

Africa_pop <-
  gapminder %>% group_by(year) %>%
   filter(continent == "Africa") %>% 
   summarise(sum_pop = sum(pop))
 
Europe_pop <- 
  gapminder %>% group_by(year) %>%
    filter(continent == "Europe") %>% 
    summarise(sum_pop = sum(pop))

Africa_pop %>% filter(Africa_pop$sum_pop > Europe_pop$sum_pop)
