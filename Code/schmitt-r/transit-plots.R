setwd("C:/Users/carys/gatech/mgt6203/Team-104/")
library(tidyverse)

# plots of annual use

upt <- read.csv('Data/upt_city_sums_top_100.csv')
figure_filepath <- 'Visualizations/upt_sums/'

upt$date <- as.Date(upt$date)

colNames <- names(upt)[2:102]

# hat tip: https://appsilon.com/ggplot2-line-charts/
for(i in colNames){
  plt <- ggplot(upt, aes(x=date, y=.data[[i]])) +
    geom_line(linewidth=1, color='blue') +
    geom_point(size=2, color='blue') +
    labs(
      title = paste("Yearly UPT, 2002-2022 in", i),
      caption = "UPT: The number of passengers who board public transportation vehicles.",
      x = 'Time',
      y = 'Count of unlinked trips\nacross all modalities'
    )
  filepath <- stringr::str_c(figure_filepath, as.character(i), '.png')
  ggsave(filepath, plt, width=8, height=4, units="in")
  # print(plt)
}

#paths <- stringr::str_c(figure_filepath, names(plots), '.pdf')



ggplot(upt, aes(x=date, y=New.York.Newark..NY.NJ.CT)) +
  geom_line()
