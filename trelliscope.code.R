install.packages("gapminder")
install.packages("ggplot2")
install.packages("trelliscopejs")
library(ggplot2)
library(gapminder)
library(trelliscopejs)

gapminder <- gapminder
ggplot(gapminder, aes(year,lifeExp)) +
  geom_line() +
  facet_trelliscope(~ country + continent,
                    name = "lifeExp_by_country",
                    desc = "Life expectancy vs. year for 142 countries.",
                    nrow = 2,
                    ncol = 3)
  