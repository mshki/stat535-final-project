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
y <- c(9.6, 9.3, 9.4, 	9.8, 	10.0, 9.7, 9.6)
y <- y * 0.01
# make the above percentages instead.
# this works with NaNs as well 

pop_dens <- 

clean <- data.frame(y, x1=mod$x1, x2=mod$x2, x3=mod$x3, x4=mod$x4)
cor(clean)

lm0 <- lm(y ~ ., data=clean)
summary(lm0)
