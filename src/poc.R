library(tidyverse)
pol <- read_csv("data/pollution_2000_2023.csv")

get_year <- function(df, years) {
  xxx <- data.frame()
  for (year in years) {
    spec_year <- df %>% filter(str_detect(Date, year))
    x1 <- mean(spec_year[["O3 Mean"]])
    x2 <- mean(spec_year[["CO Mean"]])
    x3 <- mean(spec_year[["SO2 Mean"]])
    x4 <- mean(spec_year[["NO2 Mean"]])
    xxx <- rbind(xxx, data.frame(x1,x2,x3,x4))
  }
  return(xxx)
}

arz <- pol %>% filter(City == "Phoenix")
years <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020")
mod <- get_year(arz, years)
y <- c(484115, 479791, 491794, 529155, 545104, 544394, 550729)

clean <- data.frame(x2=mod$x2, x3=mod$x3, y)
cor(clean)

lm0 <- lm(y ~ x2+x3, data=clean)
summary(lm0)
