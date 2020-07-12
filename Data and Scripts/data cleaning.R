library(tidyverse)
library(alphavantager)
library(readxl)
library(lubridate)
library(zoo)

risk_free <- read.table("~/Documents/Honours/Data and Scripts/Data/F-F_Research_Data_Factors.txt", header=TRUE, quote="\"") %>% 
  as_tibble() %>% 
  select(Rf = Mkt.RF)

test_return <-  read_excel("~/Documents/Honours/Data and Scripts/Data/Osiris_Export_4.xlsx", col_types = c("numeric", "text", "text", rep("numeric", 179))) %>% 
  select(-"...1")

var_name <- test_return[3:181] %>% variable.names()

 test_return <- test_return %>% 
  pivot_longer(var_name, names_to = "time", values_to = "price") %>% 
  separate(time, into = c("fir", "sec", "thir"), sep = "- ") %>% 
  select(-c(fir, sec)) %>% 
  separate(thir, into = c("Month", "a", "year"), sep = "\n" ) %>% 
  select(-a) %>% 
  separate(Month, into = c("Month", "a"), sep = "\r" ) %>% 
  select(-a) %>% 
  unite(time, year,Month, sep = "/") %>% 
   select("name"  = `Company name`,"ticker" = `Ticker symbol`, time, price)

 test_return$time <- as.yearmon(test_return$time, "%Y/%B")
 
na_list<- test_return %>% filter(time <"Jan 2018") %>% 
  filter(is.na(price)) %>% select(ticker) %>% unique() 
 
anti_join(test_return, na_list) %>% filter(time < "Jan 2018") %>%  select(ticker) %>% unique()
 
 test_return %>% group_by(ticker) %>% 
   arrange(time)
 
ten_return <-  test_return %>% drop_na() %>% 
   filter(time < "Jan 2018") %>% arrange(ticker ,time)

avilable_list <- ten_return %>% group_by(ticker) %>% 
  summarize(count=n()) %>% filter(count >=120) %>% 
  select(ticker)

 semi_join(ten_return, avilable_list) %>% group_by(ticker) %>% nest()
 
 
