install.packages("dplyr")
install.packages("ggplot2")
install.packages("dslabs")
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

gapminder %>% filter(continent=="Africa" & year=="2012") %>%
  ggplot(aes(fertility,life_expectancy)) +
  geom_point()

gapminder %>% 
  filter(continent=="Africa" & year=="2012") %>%
  ggplot() +
  geom_point(aes(fertility,life_expectancy,color=region))

df <- gapminder %>%
  filter(fertility <= 3 & life_expectancy >= 70 & continent == "Africa" &         year == "2012") %>%
  select(country, region)

tab <- gapminder %>% filter(country %in% c("United States","Vietnam") & year >= 1960 & year <= 2010)

p <- tab %>% ggplot() +
  geom_line(aes(year,life_expectancy,color=country))

gapminder %>% filter(year >= 1960 & year <= 2010 & country == "Cambodia") %>% ggplot() + geom_line(aes(year,life_expectancy))

daydollars <- gapminder %>% filter(continent=="Africa" & year==2010) %>% mutate(dollars_per_day = gdp/population/365) %>% na.omit() 

daydollars %>% ggplot() + geom_density(aes(dollars_per_day))+scale_x_continuous(trans = "log2")

gapminder %>% filter(continent=="Africa" & year %in% c("1970","2010") & !is.na(gdp)) %>%
  mutate(dollars_per_day=gdp/population/365) %>% ggplot() + geom_density(aes(dollars_per_day)) + scale_x_continuous(trans="log2") + facet_grid(vars(year))

gapminder %>% filter(continent=="Africa" & year %in% c("1970", "2010") & !is.na(gdp)) %>% mutate(dollars_per_day=gdp/population/365) %>% ggplot() + geom_density(aes(dollars_per_day, fill=region), bw=0.5, position="stack") + facet_grid(vars(year)) + scale_x_continuous(trans="log2")