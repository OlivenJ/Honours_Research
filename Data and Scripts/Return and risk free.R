library(tidyverse)
library(alphavantager)

risk_free <- read.table("~/Documents/Honours/Data and Scripts/Data/F-F_Research_Data_Factors.txt", header=TRUE, quote="\"") %>% 
  as_tibble() %>% 
  select(Rf = Mkt.RF)

names <- read.csv("~/Downloads/component.csv") %>% as_tibble()



av_api_key("MLFVJHVTM8RXPQ5U")
print(av_api_key())

alpha_return <- tibble(ticker, timestamp, close)

for (i in i:nrow(names)){

  temp <- av_get(av_fun = "TIME_SERIES_MONTHLY_ADJUSTED", symbol = names$Symbol[i] ) %>% 
    select(timestamp, close) %>% 
    mutate(ticker = names$Symbol[i]) %>% 
    select(ticker, timestamp, close)
  print(i)
  return <- return %>%  bind_rows(temp)
}


MMM <- av_get(av_fun = "TIME_SERIES_MONTHLY_ADJUSTED", symbol = names$Symbol[250],outputsize = "full" )

sum(duplicated(return))
length(unique(return$ticker))

