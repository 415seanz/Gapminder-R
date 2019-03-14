install.packages("dplyr")
install.packages("ggplot2")
install.packages("dslabs")
install.packages("ggrepel")
library(dplyr)
library(ggplot2)
library(dslabs)
library(ggrepel)
data(gapminder)

# Create stacked density plot showing the density by African region of countries by the number
# of GDP dollars per resident per day in 1970 vs 2010. Shows most regions improving conditions
# for their residents over this time period, creating the 2nd bump in the chart in 2010
gapminder %>% filter(continent=="Africa" & year %in% c("1970", "2010") & !is.na(gdp)) %>% 
  mutate(dollars_per_day=gdp/population/365) %>% 
  ggplot() + 
  geom_density(aes(dollars_per_day, fill=region), bw=0.5, position="stack") + 
  facet_grid(vars(year)) + 
  scale_x_continuous(trans="log2")

#Demonstrate how infant mortality has decreased in African countries between
#1970-2010 as GDP $/population/day has increased over that time period
gapminder_Africa_1970_2010 <- gapminder %>%
  filter(continent=="Africa" & year %in% c("1970","2010")) %>%
  mutate(dollars_per_day=gdp/population/365) %>% na.omit()
gapminder_Africa_1970_2010 %>% ggplot(aes(dollars_per_day,infant_mortality, color=region, label=country)) +
  geom_point() +
  scale_x_continuous(trans="log2") +
  geom_text_repel() + 
  facet_grid(year~.)

#Show life expectancy in Vietnam & US between 1960-2010 to demonstrate the impact of the Vietnam War
tab <- gapminder %>% filter(country %in% c("United States","Vietnam") & year >= 1960 & year <= 2010)
tab %>% ggplot() +
  geom_line(aes(year,life_expectancy,color=country))

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




