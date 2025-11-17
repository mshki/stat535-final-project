library(tidyverse)
library(rvest)

mls <- read_html("https://mls.foreclosure.com/listing/search.html?ci=albany&st=ny&utm_source=internal&utm_medium=link&utm_campaign=MLS_top_links")
listing <- mls %>% html_elements("div.container.mt-4") %>% 
  html_elements("#searchResultsMainContainer") %>% 
  html_elements("#fragmentsSearchResults") %>%
  html_elements("div.col-md-12.listingRow") 


listing[1] %>% html_elements("div.address") %>% html_text2()
listing[1] %>% html_elements("div.fl.bedroomsbox") %>% html_text2()
listing[1] %>% html_elements("div.fl.barhroomsbox") %>% html_text2()
listing[1] %>% html_elements("div.fl.sizebox") %>% html_text2()
listing[1] %>% html_elements("div.fl.ptypebox") %>% html_text2()
listing[1] %>% html_elements("div.savePrice") %>% html_text2()
listing[2] %>% html_elements("div.mb-2.py-0.ps-3") %>% html_text2()

# For some reason, this is excluding the first row of information. 
parser <- function(list) {
  (df <- tibble())
  for (i in list) {
    (temp <- (
      tibble(
        Address = i %>% html_elements("div.address") %>% html_text2(),
          # Can't get the full address
        Beds = i %>% html_elements("div.fl.bedroomsbox") %>% html_text2(),
        Baths = i %>% html_elements("div.fl.barhroomsbox") %>% html_text2(),
        Size = i %>% html_elements("div.fl.sizebox") %>% html_text2(),
          # Note, size has a "." at the end, kinda wanna remove that..
        Type = i %>% html_elements("div.fl.ptypebox") %>% html_text2(),
        Price = i %>% html_elements("div.savePrice") %>% html_text2(),
          # Sometimes might not be available, price is in EMV
        Rent = i %>% html_elements("div.mb-2.py-0.ps-3") %>% html_text2()
      )
    ))
    df <- bind_rows(df, temp)
  }
  
  return(df)
}

info <- parser(listing)

