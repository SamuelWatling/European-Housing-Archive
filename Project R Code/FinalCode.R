setwd("C:/Users/S.Watling/Documents/Final European Data") 

library(tidyverse)
library(lubridate)
library(zoo) 
library(xts)

Data1990 <- read_csv("1990 housing statistics.csv") %>% 
  mutate_all(~ as.character(.)) %>%
  pivot_longer(-c("Description", "Item"), names_to = c("Country", "Year"), names_sep = " ",
               values_to = "Value", values_transform = as.character) %>%
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
  mutate(Date = parse_date_time(Year, orders = c("Y", "dmy"))) %>%
  distinct(Country, Date, Item, .keep_all = TRUE) %>%
  select(-c("Year")) 

view(Data1990)

Stock1990 <- read_csv("FullHousingStock.csv") %>%
             select(-"...1") %>% 
             filter(Item %in% (1:2) | Item %in% (5:6)) %>%
             pivot_longer(-c("Description", "Item", "Country"), names_to = "Date", values_to = "Value")
             
view(Stock1990)

Full1990 <- rbind(Data1990, Stock1990) %>%
            mutate(Country = str_to_title(Country)) %>% 
            distinct(Country, Date, Item, .keep_all = TRUE) %>%
            pivot_wider(names_from = Date, values_from = Value) %>%
            mutate(Item = as.numeric(Item)) %>%
            mutate(Country = gsub("ssr","SSR", Country))
            

view(Full1990)

Baseline <- read_csv("SecondIIJoin1960s.csv") %>%
            select(-"...1") %>% 
            left_join(Full1990, by = c("Country" = "Country", "Item" = "Item")) %>%
            mutate(X1980 = coalesce(X1980, `1980-01-01`), X1981 = coalesce(X1981, `1981-01-01`), 
                   X1985 = coalesce(X1985, `1985-01-01`), X1987 = coalesce(X1987, `1987-01-01`), 
                   X1988 = coalesce(X1988, `1988-01-01`)) %>%
            select(-c("Description", "1980-01-01", "1981-01-01", "1985-01-01", "1987-01-01", "1988-01-01")) %>%
            rename("X1989" = "1989-01-01", "X1990" = "1990-01-01", "X1991" = "1991-01-01")

view(Baseline)

