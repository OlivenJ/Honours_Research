library(tidyverse)
library(lubridate)
library(readxl)
#=============================================================================================#
#=============================================================================================#
#Risk Free rate and Factors#


risk_free <- read.table("~/Documents/Honours/Data and Scripts/Data/F-F_Research_Data_Factors.txt", header=TRUE, quote="\"") %>% 
  as_tibble() %>% 
  select(Rf = Mkt.RF) 
risk_free$Date <- seq.Date(ymd("1926-07-01"), ymd("2020-5-01"), "month")
risk_free <- risk_free %>% select(Date, everything())

risk_factors <- read_excel("Documents/Honours/Data and Scripts/Data/Risk_factors.xlsx")%>% 
  rename(Date = `in per cent!`)
risk_factors$Date <- (seq.Date(ymd("1976-07-01"), ymd("2017-12-01"), "month"))
risk_factors <- risk_factors %>% 
  pivot_longer(c(variable.names(risk_factors)[-1]), names_to = "Factor", values_to = "Value" )


#=============================================================================================#
#=============================================================================================#
#Return#
price <- read_excel("Documents/Honours/Data and Scripts/Data/price.xlsx", 
                    col_types = c("text", "date", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric")) %>% 
  select(Ticker, Date, Adj.Close)

group_price <- price %>% 
  select(Ticker, Date, Adj.Close) %>% 
  arrange(Ticker, Date) %>% 
  group_by(Ticker) %>% 
  nest()

return <- tibble(Ticker = character(), Date =ymd(), Adj.Close = double(), Return = double())

for (i in 1:(nrow(group_price)-1)) {
  temp <-group_price[i,] %>% 
    unnest(col = c("data"))
  temp$Return <- c(NA,diff(temp$Adj.Close)/temp$Adj.Close[1])
  return <-return %>%  bind_rows(temp)
  print(i)
}
ymd(return$Date)

return %>% filter(Date >= ymd("1976-07-01") & Date <= ymd("2017-12-01"))
  
return <- inner_join(return, risk_free, by = "Date") %>% mutate(Excess = Return - Rf)

ten_name <- price %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("2007-11-01")) %>% 
  group_by(Ticker) %>% 
  summarise(length = n()) %>% 
  filter(length == 121)

twenty_name <- price %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("1997-11-01")) %>% 
  group_by(Ticker) %>% 
  summarise(length = n()) %>% 
  filter(length == 241)

thirty_name <- price %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("1987-11-01")) %>% 
  group_by(Ticker) %>% 
  summarise(length = n()) %>% 
  filter(length == 361)

ten_return <- semi_join(return, ten_name, by = "Ticker") %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("2007-12-01")) %>% 
  select(-Rf, -Adj.Close)

twenty_return <- semi_join(return, twenty_name, by = "Ticker") %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("1997-12-01")) %>% 
  select(-Rf, -Adj.Close)

thirty_return <- semi_join(return, thirty_name, by = "Ticker") %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("1987-12-01")) %>% 
  select(-Rf, -Adj.Close)

ten_return$Date <- as.Date(ten_return$Date)
twenty_return$Date <- as.Date(twenty_return$Date)
thirty_return$Date <- as.Date(thirty_return$Date)
