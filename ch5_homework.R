install.packages("gapminder")
install.packages("dplyr")
library(gapminder)
library(dplyr)

#(1952 ~ 2007)���ѹα� �α��� �ִ񰪰� �ش� �⵵�� ���
apply(gapminder[gapminder$country == "Korea, Rep." & gapminder$year > 1951 & gapminder$year < 2008,
                c("pop", "year")], 2, max)

#2007�⵵ �ƽþ� ����� �α� ������ ��� 
apply(gapminder[gapminder$continent == "Asia" & gapminder$year == 2007, 
                "pop" ], 2, sum)
