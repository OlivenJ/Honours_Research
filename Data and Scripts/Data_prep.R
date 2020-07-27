library(tidyverse)
library(lubridate)
library(readxl)
#=============================================================================================#
#=============================================================================================#
#Risk Free rate and Factors#


risk_free <- read.table("~/Documents/Honours/Data and Scripts/Data/F-F_Research_Data_Factors.txt", header=TRUE, quote="\"") %>% 
  as_tibble() %>% 
  select(Rf = RF)
risk_free$Date <- seq.Date(ymd("1926-07-01"), ymd("2020-5-01"), "month")
risk_free <- risk_free %>% select(Date, everything())

risk_factors <- read_excel("/Users/olivenjiang/Documents/Honours/Data and Scripts/Data/Risk_factors.xlsx")%>% 
  rename(Date = `in per cent!`)
risk_factors$Date <- (seq.Date(ymd("1976-07-01"), ymd("2017-12-01"), "month"))
risk_factors <- risk_factors %>% 
  pivot_longer(c(variable.names(risk_factors)[-1]), names_to = "Factor", values_to = "Value" )

factor_ancillary <- read_excel("/Users/olivenjiang/Desktop/Ancillary Materials for Honours/Book1.xlsx", 
                            col_types = c("text", "text", "skip", 
                                          "numeric"))

Market_factor <- risk_factors %>% filter(Factor == "MktRf")
risk_factor<- risk_factors %>% filter(Factor !="MktRf") 
#=============================================================================================#
#=============================================================================================#
#Return#
price <- read_excel("/Users/olivenjiang/Documents/Honours/Data and Scripts/Data/price.xlsx", 
                    col_types = c("text", "date", "numeric", 
                                  "numeric", "numeric", "numeric", 
                                  "numeric", "numeric")) %>% 
  select(Ticker, Date, Adj.Close, Close)

group_price <- price %>% 
  select(Ticker, Date, Adj.Close, Close) %>% 
  filter(Date < as.Date("2020-07-09")) %>% 
  arrange(Ticker, Date) %>% 
  group_by(Ticker) %>% 
  nest()

raw_return <- tibble(Ticker = character(), Date =ymd(), Adj.Close = double(),Close = double(), Return = double(), Adj.Return = double())



for (i in 1:(nrow(group_price)-1)) {
  temp <-group_price[i,] %>% 
    unnest(col = c("data"))
  temp$Adj.Return <- c(NA, diff(temp$Adj.Close)/temp$Adj.Close[-length(temp$Adj.Close)]*100)
  temp$Return <- c(NA, diff(temp$Close)/temp$Close[-length(temp$Close)]*100)
  raw_return <-raw_return %>%  bind_rows(temp)
  print(i)
}
#notice the changes 
raw_return

#return %>% mutate(diff = Return-Adj.Return) %>% view()

return <- raw_return %>% filter(Date >= ymd("1976-07-01") & Date <= ymd("2017-12-01")) %>% 
  inner_join( risk_free, by = "Date") %>% mutate(Excess = Return - Rf) %>% 
  select(-Adj.Return) %>% 
  filter(Excess < 300) 

  
ten_name <- price %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("2007-11-01")) %>% 
  group_by(Ticker) %>% 
  summarise(length = n()) %>% 
  filter(length == 121)

two_ten_name <- price %>% 
  filter(Date < ymd("2008-01-01") & Date > ymd("1997-11-01")) %>% 
  group_by(Ticker) %>% 
  summarise(length =n()) %>% 
  filter(length == 121)

three_ten_name <- price %>% 
  filter(Date < ymd("1998-01-01") & Date > ymd("1987-11-01")) %>% 
  group_by(Ticker) %>% 
  summarise(length =n()) %>% 
  filter(length == 121)

#fifty_name <-price %>% 
# filter(Date < ymd("2018-01-01") & Date > ymd("2002-11-01")) %>% 
#  group_by(Ticker) %>% 
#  summarise(length = n()) %>% 
#  filter(length == 181)

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

#post_GFC_name <- price %>% 
#  filter(Date < ymd("2018-01-01") & Date > ymd("2009-03-01")) %>% 
#  group_by(Ticker) %>% 
#  summarise(length = n()) %>% 
#  filter(length == 105)

#pre_GFC_thirty_name <- price %>% 
#  filter(Date < ymd("2016-01-01") & Date > ymd("1985-12-01")) %>% 
#  group_by(Ticker) %>% 
#  summarise(length = n()) %>% 
#  filter(length == 360)

#first_boom_name <- price %>% 
#  filter(Date < ymd("2000-07-01") & Date > ymd("1990-05-01")) %>% 
#  group_by(Ticker) %>% 
#  summarise(length = n()) %>% 
#  filter(length == 121)

ten_return <- semi_join(return, ten_name, by = "Ticker") %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("2007-12-01")) %>% 
  select(-Rf, -Adj.Close)


two_ten_return <- semi_join(return, two_ten_name, by = "Ticker") %>% 
  filter(Date < ymd("2008-01-01") & Date > ymd("1997-12-01")) %>% 
  select(-Rf, -Adj.Close)

three_ten_return<- semi_join(return, three_ten_name, by = "Ticker") %>% 
  filter(Date < ymd("1998-01-01") & Date > ymd("1987-12-01")) %>% 
  select(-Rf, -Adj.Close)

#fifty_return <- semi_join(return, fifty_name, by = "Ticker") %>% 
#  filter(Date < ymd("2018-01-01") & Date > ymd("2002-12-01")) %>% 
#  select(-Rf, -Adj.Close)

twenty_return <- semi_join(return, twenty_name, by = "Ticker") %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("1997-12-01")) %>% 
  select(-Rf, -Adj.Close)

thirty_return <- semi_join(return, thirty_name, by = "Ticker") %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("1987-12-01")) %>% 
  select(-Rf, -Adj.Close)

#post_GFC_return <- semi_join(return, post_GFC_name, by = "Ticker") %>% 
#  filter(Date < ymd("2018-01-01") & Date > ymd("2009-04-01")) %>% 
#  select(-Rf, -Adj.Close)

#pre_GFC_thirty_return <- semi_join(return, pre_GFC_thirty_name, by = "Ticker") %>% 
#  filter(Date < ymd("2016-01-01") & Date > ymd("1985-12-01")) %>% 
#  select(-Rf, -Adj.Close)

#first_boom_return <- semi_join(return, first_boom_name, by = "Ticker") %>% 
#  filter(Date < ymd("2000-07-01") & Date > ymd("1990-06-01")) %>% 
#  select(-Rf, -Adj.Close)

ten_return$Date <- as.Date(ten_return$Date)
two_ten_return$Date <- as.Date(two_ten_return$Date)
three_ten_return$Date <- as.Date(three_ten_return$Date)
#fifty_return$Date <- as.Date(fifty_return$Date)
twenty_return$Date <- as.Date(twenty_return$Date)
thirty_return$Date <- as.Date(thirty_return$Date)
#post_GFC_return$Date<- as.Date(post_GFC_return$Date)
#first_boom_return$Date <- as.Date(first_boom_return$Date)
#pre_GFC_thirty_return$Date<- as.Date(pre_GFC_thirty_return$Date)

ten_return <- inner_join(ten_return, risk_factor, by = "Date") %>% inner_join(Market_factor, by = "Date")%>% 
  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
  select(-Factor.y) 

two_ten_return <- inner_join(two_ten_return, risk_factor, by = "Date") %>% inner_join(Market_factor, by = "Date")%>% 
  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
  select(-Factor.y) 


three_ten_return <- inner_join(three_ten_return, risk_factor, by = "Date") %>% inner_join(Market_factor, by = "Date")%>% 
  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
  select(-Factor.y) 

#fifty_return <- inner_join(fifty_return, risk_factor, by = "Date") %>% inner_join(Market_factor, by = "Date")%>% 
#  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
#  select(-Factor.y) 

twenty_return<- inner_join(twenty_return, risk_factor, by = "Date") %>% 
  inner_join(Market_factor, by = "Date")%>% 
  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
  select(-Factor.y) 

thirty_return<- inner_join(thirty_return, risk_factor, by = "Date") %>%
  inner_join(Market_factor, by = "Date")%>% 
  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
  select(-Factor.y) 

#post_GFC_return<- inner_join(post_GFC_return, risk_factor, by = "Date") %>%
#  inner_join(Market_factor, by = "Date")%>% 
#  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
#  select(-Factor.y) 

#first_boom_return <- inner_join(first_boom_return, risk_factor, by = "Date") %>%
#  inner_join(Market_factor, by = "Date")%>% 
#  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
#  select(-Factor.y) 

#pre_GFC_thirty_return<- inner_join(pre_GFC_thirty_return, risk_factor, by = "Date") %>%
#  inner_join(Market_factor, by = "Date")%>% 
#  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
#  select(-Factor.y) 
