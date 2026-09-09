setwd("C:/Users/S.Watling/Documents/Newscans")

library(tidyverse)
library(tidyr)
library(lubridate)
library(haven)
library(zoo)
library(xts)

Housingstock1 <- read_csv("1989 Stock.csv") %>% 
                 pivot_longer(-c("Description", "Item"), names_to = "Period", values_to = "Value") %>%
                 separate(Period, into = c("Country", "Year"), sep = "\\,") %>%
  mutate(Value = gsub("S", "8", Value)) %>%
  mutate(Value = gsub("O", "0", Value)) %>%  
  mutate(Value = gsub("s", "8", Value)) %>%
  mutate(Value = gsub("o", "0", Value)) %>%
  mutate(Value = gsub("I", "1", Value)) %>%
  mutate(Value = gsub("l", "1", Value)) %>%
  mutate(Value = gsub("\\'", "1", Value)) %>%
  mutate(Value = gsub("/)'", "7", Value)) %>%
  mutate(Value = gsub("\\/", "\\.", Value)) %>%
  mutate(Value = gsub("\\,", "\\.", Value)) %>%
  mutate(Value = gsub(" ", "", Value)) %>% 
  mutate(Value = gsub("[^0-9, .]", "", Value)) %>%
  mutate(Value = as.numeric(Value)) %>%
  mutate(Date = parse_date_time(Year, orders = c("Y", "dmy"))) 

view(Housingstock1)

Housingstock2 <- read_csv("1990 Stock.csv") %>%
                 pivot_longer(-c("Description", "Item") , names_to = "Period", values_to = "Value") %>%
                 separate(Period, into = c("Country", "Year"), sep = " ") %>% 
  mutate(Value = gsub("S", "8", Value)) %>%
  mutate(Value = gsub("O", "0", Value)) %>%  
  mutate(Value = gsub("s", "8", Value)) %>%
  mutate(Value = gsub("o", "0", Value)) %>%
  mutate(Value = gsub("I", "1", Value)) %>%
  mutate(Value = gsub("l", "1", Value)) %>%
  mutate(Value = gsub("\\'", "1", Value)) %>%
  mutate(Value = gsub("/)'", "7", Value)) %>%
  mutate(Value = gsub("\\/", "\\.", Value)) %>%
  mutate(Value = gsub("\\,", "\\.", Value)) %>%
  mutate(Value = gsub(" ", "", Value)) %>% 
  mutate(Value = gsub("[^0-9, .]", "", Value)) %>%
  mutate(Value = as.numeric(Value)) %>%
  mutate(Date = parse_date_time(Year, orders = c("Y", "dmy"))) 

view(Housingstock2)
  
Housingstock3 <- read_csv("1991 Stock.csv") %>% 
                 pivot_longer(-c("Description", "Item") , names_to = "Period", values_to = "Value") %>%
                 separate(Period, into = c("Country", "Year"), sep = " ") %>%
  mutate(Value = gsub("S", "8", Value)) %>%
  mutate(Value = gsub("O", "0", Value)) %>%  
  mutate(Value = gsub("s", "8", Value)) %>%
  mutate(Value = gsub("o", "0", Value)) %>%
  mutate(Value = gsub("I", "1", Value)) %>%
  mutate(Value = gsub("l", "1", Value)) %>%
  mutate(Value = gsub("\\'", "1", Value)) %>%
  mutate(Value = gsub("/)'", "7", Value)) %>%
  mutate(Value = gsub("\\/", "\\.", Value)) %>%
  mutate(Value = gsub("\\,", "\\.", Value)) %>%
  mutate(Value = gsub(" ", "", Value)) %>% 
  mutate(Value = gsub("[^0-9, .]", "", Value)) %>%
  mutate(Value = as.numeric(Value)) %>%
  mutate(Date = parse_date_time(Year, orders = c("Y", "dmy"))) 

view(Housingstock3)

Housingstockfull <- rbind(Housingstock1, Housingstock2, Housingstock3) %>%
                    arrange(Country) %>%
                    mutate(Country = str_to_title(Country)) %>%
                    select(-c("Year")) %>% 
                    group_by(Country) %>% 
                    distinct(Date, Item, .keep_all = TRUE) %>% 
                    pivot_wider(names_from = Date, values_from = Value) %>%
                    ungroup() %>% 
                    arrange(Item) %>%
                    arrange(Country) 

view(Housingstockfull)

write.csv(Housingstockfull, "FullHousingStock.csv") 

Data1990 <- read_csv("1990 housing statistics.csv") %>% 
            mutate_all(~ as.character(.)) %>%
            pivot_longer(-c("Description", "Item"), names_to = c("Country", "Year"), names_sep = " ",
                          values_to = "Value", values_transform = as.character) 
            
view(Data1990)
