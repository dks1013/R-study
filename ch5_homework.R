install.packages("gapminder")
install.packages("dplyr")
library(gapminder)
library(dplyr)

#(1952 ~ 2007)대한민국 인구의 최댓값과 해당 년도를 출력
apply(gapminder[gapminder$country == "Korea, Rep." & gapminder$year > 1951 & gapminder$year < 2008,
                c("pop", "year")], 2, max)

#2007년도 아시아 대륙의 인구 총합을 출력 
apply(gapminder[gapminder$continent == "Asia" & gapminder$year == 2007, 
                "pop" ], 2, sum)

