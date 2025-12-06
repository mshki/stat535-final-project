library(httr)
library(rvest)
library(dplyr)
library(janitor)
library(purrr)
library(stringr)

asthma_urls <- list(
  "2012" = "https://www.cdc.gov/asthma/brfss/2012/tableC1.htm",
  "2013" = "https://www.cdc.gov/asthma/brfss/2013/tableL21.htm",
  # "2014" = "https://www.cdc.gov/asthma/archivedata/2014/2014_data_states.html",
  # "2015" = "https://www.cdc.gov/asthma/archivedata/2015/2015_data_states.html",
  # "2016" = "https://www.cdc.gov/asthma/archivedata/2016/2016-archived-data-states.html",
  # "2017" = "https://www.cdc.gov/asthma/archivedata/2017/2017_archived_states_territory.html",
  # "2018" = "https://www.cdc.gov/asthma/archivedata/2018/2018_archived_states_territory.html",
  # "2019" = "https://www.cdc.gov/asthma/archivedata/2019/2019_archived_states_territory.html",
  "2020" = "https://www.cdc.gov/asthma/brfss/2020/tableL1.html"
)


tbl20 <- read_html("https://www.cdc.gov/asthma/brfss/2020/tableL1.html") %>% 
  html_table(fill = TRUE) 
tbl20 <- tbl20[[1]] %>% select(contains("State"), contains("Prevalence"))

convert_state_code <- function(df, col = "State") {
  state_lookup <- setNames(state.name, toupper(state.abb))
  state_codes <- df[[col]]
  input_codes_upper <- toupper(as.character(state_codes))
  converted_names <- state_lookup[input_codes_upper]
  df[[col]] <- unname(converted_names)
  return(df)
}

asthma_20 <- convert_state_code(tbl20) %>% drop_na("State")


get_year_pollutant <- function(df, year, State="State") {
  spec_year <- df %>% filter(str_detect(Date, year))
  for (state in spec_year[[State]]) {
    spec_state <- df[df$State == state,]
    x1 <- mean(df[["O3 Mean"]])  
    x2 <- mean(df[["CO Mean"]])
    x3 <- mean(df[["SO2 Mean"]])
    x4 <- mean(df[["NO2 Mean"]])
  }
}

pol <- read_csv("data/pollution_2000_2023.csv") %>% 
  clean_names()

pol <- pol %>%
  mutate(date = suppressWarnings(as.Date(date, format = "%d/%m/%y")),
         year = year(date))

pol_cols <- c("o3_mean", "co_mean", "so2_mean", "no2_mean")
pol_cols <- pol_cols[pol_cols %in% names(pol)]

pol_state_year <- pol %>%
  group_by(state, year) %>%
  summarise(
    across(
      all_of(pol_cols), 
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  ungroup()

pol_20 <- pol_state_year[pol_state_year$year == 2020, ]
colnames(pol_20)[colnames(pol_20) == "state"] <- "State"
joint <- inner_join(asthma_20, pol_20, by = "State")
colnames(joint)[2] <- "Rate"
joint$Rate <- as.numeric(joint$Rate)

lm0 <- lm(Rate ~ year+o3_mean+co_mean+so2_mean, no2_mean, data=joint)
summary(lm0)

cor(cbind(joint$Rate, joint$o3_mean, joint$co_mean, joint$so2_mean, joint$no2_mean))
