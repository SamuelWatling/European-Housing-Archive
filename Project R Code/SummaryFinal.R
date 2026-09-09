setwd("C:/Users/S.Watling/Documents/Processed Modern") 

options(scipen = 999)  

library(tidyverse)
library(lubridate)
library(rlang)
library(zoo)
library(xts)
library(ggpattern) 

Main <- read_csv("Combined.csv") %>% 
  select(-c("...1")) %>% 
  mutate(Private = `4` * Y7, Public = `1` * Y7) %>% 
  mutate(PrivRate = 1 + (Private / `X5 (Estimate)`), PubRate = 1 + (Public / `X5 (Estimate)`), TotRate = 1 + (Y7 /`X5 (Estimate)`)) %>% 
  mutate(Priv = Private * 100 / `X5 (Estimate)`, Pub = Public *100 / `X5 (Estimate)`, Total = Y7 * 100 / `X5 (Estimate)`) %>% 
  mutate(Date = dmy(Date))  
  
view(Main)

Population <- Main %>%
  select(c("Country", "Date", "X50")) %>%
  filter(Date == as.Date("1955-01-01") | Date == as.Date("1979-01-01") | Date == as.Date("2015-01-01")) %>% 
  pivot_wider(names_from = Date, values_from = X50) %>% 
  mutate(`Change PostWar` = `1979-01-01` - `1955-01-01`, PctChangePostWar = `Change PostWar`*100 / `1955-01-01`) %>% 
  mutate(`Change Modern`  = `2015-01-01` - `1979-01-01`, PctChangeModern = `Change Modern`*100 / `1979-01-01`) %>% 
  mutate(`Change Total` = `2015-01-01` - `1955-01-01`, PctChangeTotal = `Change Total` *100 / `1955-01-01`) %>%
  arrange(PctChangeTotal) 

view(Population) 


Population2 <- Main %>%
  filter(Date == as.Date("1948-01-01") | Date == as.Date("1979-01-01")) %>% 
  pivot_wider(names_from = Date, values_from = X50) %>% 
  mutate(Change = `1979-01-01` - `1948-01-01`, PctChange = Change *100 / `1948-01-01`) %>% 
  arrange(PctChange) 

view(Population2) 

SwiComp <- read_csv("Combined.csv") %>% 
  select(-c("...1")) %>% 
  filter(Date < as.Date("1980-01-01")) %>% 
  filter(Country == "Unitedkingdom" | Country == "Switzerland") %>%
  group_by(Country) %>% 
  summarise(MeanPriv = mean(Priv), MeanPub = mean(Pub)) %>% 
  mutate(MeanTot = MeanPriv + MeanPub)

view(SwiComp) 

UKComp <- Main %>% 
          select(c("Country", "Date", "Priv", "Pub", "Total")) %>%
          filter(Country == "Unitedkingdom") %>% 
          mutate(Period = ifelse(Date > as.Date("1949-01-01") & Date < as.Date("1960-01-01"), "1950s",  
                          ifelse(Date > as.Date("1959-01-01") & Date < as.Date("1970-01-01"), "1960s", 
                          ifelse(Date > as.Date("1969-01-01") & Date < as.Date("1980-01-01"), "1970s", 
                          ifelse(Date > as.Date("1979-01-01") & Date < as.Date("1990-01-01"), "1980s", 
                          ifelse(Date > as.Date("1989-01-01") & Date < as.Date("2000-01-01"), "1990s", 
                          ifelse(Date > as.Date("1990-01-01") & Date < as.Date("2010-01-01"), "2000s", 0 ))))))) %>%
         filter(Period != 0) %>% 
         group_by(Period) %>% 
         summarise(`Mean Private Rate` = mean(Priv), `Mean Public Rate` = mean(Pub), `Mean Total Rate` = mean(Total))

view(UKComp) 

UKPost <- Main %>% select(c("Country", "Date", "Priv", "Pub", "Total")) %>%
  filter(Country == "Unitedkingdom") %>% 
  mutate(Period = ifelse(Date > as.Date("1947-01-01") & Date < as.Date("1980-01-01"), "1948-1979",  
                  ifelse(Date > as.Date("1979-01-01") & Date < as.Date("2020-01-01"), "1980-2020", 0))) %>%
                           filter(Period != 0) %>% 
                           group_by(Period) %>% 
                           summarise(`Mean Private Rate` = mean(Priv), `Mean Public Rate` = mean(Pub), `Mean Total Rate` = mean(Total)) %>% 
         rbind(UKComp) 

view(UKPost) 

UK <- Main %>% select(c("Country", "Date", "Priv", "Pub", "Total")) %>%
  filter(Country == "Unitedkingdom") %>% 
  filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>%
summarise(`Mean Private Rate` = mean(Priv), `Mean Public Rate` = mean(Pub), `Mean Total Rate` = mean(Total)) %>% 
  mutate(Period = "1955-1979") %>% 
  rbind(UKPost) 

view(UK) 

UK 

view

UKTotal <- Main %>% select(c("Country", "Date", "Priv", "Pub", "Total")) %>% 
  filter(Date < as.Date("2020-01-01")) %>%
  filter(Country == "Unitedkingdom") %>% 
  summarise(`Mean Private Rate` = mean(Priv), `Mean Public Rate` = mean(Pub), `Mean Total Rate` = mean(Total)) %>% 
  mutate(Period = "1948-2019") %>% 
  rbind(UK) %>% 
  slice(1:4) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 2))) %>% 
  relocate(Period, .before = `Mean Private Rate`)
  
view(UKTotal)

setwd("C:/Users/S.Watling/Documents/Counterfactuals") 
write.csv(UKTotal, "UKSummary.csv")
