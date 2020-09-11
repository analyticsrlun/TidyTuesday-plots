install.packages("ggthemes")
library(ggthemes)
library(ggplot2)
library(friends)

ggplot(friends_info, aes(x=episode, y=us_views_millions, color=season)) +
  geom_point(aes(size=us_views_millions)) +
  theme_wsj() +
  labs(title="Most viewed episodes per season (FRIENDS)", x="Episode", y="Views in united states (millions)") + 
  facet_grid(.~season)