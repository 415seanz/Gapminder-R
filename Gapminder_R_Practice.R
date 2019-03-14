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


#Create stacked density plot showing the density of African countries by the number 
#of GDP dollars per resident per day in 1970 vs 2010. Shows conditions have improved
#for their residents over this time period, shown by the 1st bump decreasing and more
#countries moving right on this chart between 1970 and 2010
gapminder %>% filter(continent=="Africa" & year %in% c("1970","2010") & !is.na(gdp)) %>%
  mutate(dollars_per_day=gdp/population/365) %>% 
  ggplot() + 
  geom_density(aes(dollars_per_day)) + 
  scale_x_continuous(trans="log2") + 
  facet_grid(vars(year))

#Create stacked density plot showing the density by African region of countries by the number 
#of GDP dollars per resident per day in 1970 vs 2010. Shows most regions improving conditions 
#for their residents over this time period, creating the 2nd bump in the chart in 2010
gapminder %>% filter(continent=="Africa" & year %in% c("1970", "2010") & !is.na(gdp)) %>% 
  mutate(dollars_per_day=gdp/population/365) %>% 
  ggplot() + 
  geom_density(aes(dollars_per_day, fill=region), bw=0.5, position="stack") + 
  facet_grid(vars(year)) + 
  scale_x_continuous(trans="log2")