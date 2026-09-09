setwd("C:/Users/S.Watling/Documents/Processed Modern")

options(scipen = 999)

library(tidyverse)
library(lubridate)
library(rlang) 
library(janitor)
library(zoo)
library(xts)

Household <- read_csv("FullHousehold.csv")  

Tenure <- read_csv("ModernTenure.csv") %>%
  pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
  pivot_wider(names_from = Item, values_from = Value) %>%
  mutate(Country = gsub("Westgermany", "Germany", Country)) 

Total <- Household %>% 
         full_join(Tenure, by = c("Country" = "Country", "Date" = "Date")) %>% 
         mutate(Date = dmy(Date)) %>%
         mutate(Country = ifelse(Date < as.Date("1991-01-01"), 
                       gsub("Germany", "West Germany", Country), Country))
         
view(Household)
view(Tenure)
view(Total)

setwd("C:/Users/S.Watling/Documents/Final European Data") 

EuroCap <- read.csv("Complete West Capital Formation.csv") %>%
  select(-c("X", "Out.of", "Quantity" )) %>% 
  mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Westgermany", "Unitedkingdom", "Unitedstates") , 1, 0)) %>%
  filter(WestEurope == 1) %>%
  select(-"WestEurope") %>%
  pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
  mutate(Date = gsub("[^0-9.-]", "", Date)) %>%
  mutate(Date = parse_date_time(Date, orders = c("Y", "ymd"))) %>% 
  pivot_wider(names_from = Item, values_from = Value) %>% 
  mutate(Country = gsub("Westgermany", "West Germany", Country)) 

Totinv <- Total %>% 
          full_join(EuroCap, by = c("Country" = "Country", "Date" = "Date")) 
  
view(Totinv)
view(EuroCap) 

setwd("C:/Users/S.Watling/Documents/Historical Housing Statistics") 

Households <- read_csv("Households.csv") %>% 
  row_to_names(row_number = 3) %>% 
  dplyr::select(Year, Houses) %>%
  slice(c(2:87)) %>% 
  separate("Year", c("Year", "Month"), sep = " ") %>% 
  mutate(Month = gsub("[^[:alpha:]]+", "", Month)) %>% 
  mutate_all(na_if, "") %>%
  fill(Month) %>% 
  mutate(Year = as.numeric(Year)) %>%
  filter(Year > 1800) %>% 
  mutate(Month = gsub("April", "1 April", Month), Month = gsub("March", "31 March", Month), Month = gsub("December", "31 December", Month)) %>% 
  mutate(Houses = gsub(",", "", Houses)) %>%
  mutate(Houses = as.numeric(Houses)) %>% 
  filter(Houses > 0) %>% 
  filter(Year < 1970 | Year >= 1970 & Month != "1 April" ) 

view(Households) 

PreWar <- read_csv("Pre-War-Completions.csv") %>%
  row_to_names(row_number = 2) %>%
  rename("Private unassisted" = "Quantity") %>% 
  mutate(Total = `Private unassisted`, `Private assisted` = 0, `Public Housing` = 0) %>%
  mutate(Year = gsub("[^0-9]+", "", Year)) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year < 1918) 


view(PreWar)

Ten1 <- read_csv("Pre-War Tenure.csv") %>% 
  row_to_names(row_number = 3) %>% 
  slice(3:28) %>%
  mutate(Year = gsub("\\/..", "", Year)) %>%
  rename("Public Housing" = "Local authorities") %>% 
  rbind(PreWar) %>%
  mutate(Prefabs = 0, `Government departments` = 0, `Housing associations` = 0)

view(Ten1)

Ten2 <- read_csv("Post-War Tenure.csv") %>%
  row_to_names(row_number = 3) %>%
  slice(2:41) %>% 
  separate("Local authorities and New Towns", c("Public Housing", "Prefabs"), sep = "\\+") %>%
  rename("Private unassisted" = "Private owners") %>%
  mutate(`Private assisted` = 0)

view(Ten2)

Ten3 <- read_csv("Post-War Tenure2.csv") %>% 
  row_to_names(row_number = 2) %>%
  rename("Public Housing" = "Local authorities and New Towns") %>%
  rename("Private unassisted" = "Private owners") %>%
  mutate(`Private assisted` = 0, Prefabs = 0)  

view(Ten3)

Ten4 <- read_csv("Currenthousebuilding.csv") %>% 
  row_to_names(row_number = 5) %>%
  slice(54:77) %>%
  mutate_all(~ gsub("[^0-9]+", "", .)) %>%
  rename("Total" = "All Dwellings", "Private unassisted" = "Private Enterprise", 
         "Housing associations" = "Housing Associations", "Public Housing" = "Local Authorities") %>%
  mutate(`Private unassisted` = as.numeric(`Private unassisted`), `Private unassisted` = `Private unassisted` / 1000, `Private unassisted` = round(`Private unassisted`, digits = 1)) %>%
  mutate(Total = as.numeric(Total), Total = Total / 1000, Total = round(Total, digits = 1)) %>% 
  mutate(`Housing associations` = as.numeric(`Housing associations`), `Housing associations` = `Housing associations` / 1000, `Housing associations` = round(`Housing associations`, digits = 2)) %>%
  mutate(`Public Housing` = as.numeric(`Public Housing`), `Public Housing` =  `Public Housing` / 1000 , `Public Housing` = round(`Public Housing`, digits = 2)) %>% 
  mutate(`Private unassisted` = as.character(`Private unassisted`), Total = as.character(Total), `Housing associations` = as.character(`Housing associations`), `Public Housing` = as.character(`Public Housing`)) %>%
  mutate(`Private assisted` = 0, Prefabs = 0, `Government departments` = 0)  

Tenure <- rbind(Ten1, Ten2, Ten3, Ten4) %>%
  slice(-(147:153)) %>%
  mutate_all(~ gsub("[^0-9, \\.]+", "", .)) %>%
  mutate_all(~as.numeric(.)) %>%
  mutate(`Public Housing` = `Public Housing` + `Government departments`) %>%
  dplyr::select(-c("Government departments")) %>% 
  mutate(`Private assisted` = na.approx(`Private assisted`, maxgap = 2, na.rm  = FALSE)) %>% 
  mutate(`Private unassisted` = na.approx(`Private unassisted`, maxgap = 2, na.rm  = FALSE)) %>% 
  mutate_all(~ ifelse(is.na(.), 0, .)) %>%
  mutate(Total2 = `Public Housing` + `Private assisted` + `Private unassisted` + Prefabs + `Housing associations`) %>% 
  mutate_all(~ ifelse(. == 0, NA, .)) %>%
  mutate(Total = coalesce(Total, Total2)) %>%
  mutate_all(~ ifelse(is.na(.), 0, .)) 

view(Tenure)

Total <- read_csv("Demolitions.csv") %>%
  row_to_names(row_number = 2) %>%
  slice(1:51) %>%
  mutate(Year = gsub("\\-..", "", Year), Year = gsub("\\/..", "", Year)) %>% 
  mutate(Year = as.numeric(Year)) %>%
  full_join(Tenure, by = c("Year" = "Year")) %>%
  full_join(Households, by = c("Year" = "Year")) %>% 
  arrange(Year) %>%
  fill(Month) %>%
  unite("Date", c("Year","Month"), sep = " ") %>% 
  mutate(Date = gsub(" ", "", Date)) %>%
  mutate(Date = ydm(Date)) %>%
  slice(9:170) %>% 
  mutate(across(!Date, as.numeric)) %>% 
  mutate(Netinc = Houses - lag(Houses)) %>%
  mutate(Nethouse = Total - Demolitions) %>% 
  mutate(Nethouse = ifelse(Date < as.Date("1955-01-01"), NA, Nethouse)) %>% 
  mutate(CumulNet = cumsum(replace_na(Nethouse, 0))) %>% 
  mutate(CumulNet = ifelse(CumulNet == 0, NA, CumulNet )) %>%
  mutate(Cumuhouse = cumsum(Total)) %>% 
  mutate(Cumuhouse2 = Cumuhouse) %>%
  mutate(Cumuhouse2 = ifelse(Date >  as.Date("1955-01-01"), NA, Cumuhouse2)) %>%
  fill(Cumuhouse2) %>% 
  mutate(Cumubase = Cumuhouse2 + CumulNet) %>%
  mutate(Cumuhouse = coalesce(Cumubase, Cumuhouse2)) %>%
  mutate(BaseHouse = Houses) %>%
  fill(BaseHouse, .direction = "downup") %>%
  mutate(FirstHouse = first(BaseHouse)) %>%
  mutate(AdjHouse = Cumuhouse - 280 ) %>%
  mutate(ImpHouse = FirstHouse + AdjHouse) %>%
  mutate(Housediff = Houses - ImpHouse) %>% 
  mutate(Census1 = ifelse(Date > as.Date("1861-01-01") & Date < as.Date("1871-01-01"), 1, 
                          ifelse(Date > as.Date("1871-01-01") & Date < as.Date("1881-01-01"), 2, 
                                 ifelse(Date > as.Date("1881-01-01") & Date < as.Date("1891-01-01"), 3 , 
                                        ifelse(Date > as.Date("1891-01-01") & Date < as.Date("1901-01-01"), 4 , 
                                               ifelse(Date > as.Date("1901-01-01") & Date < as.Date("1911-01-01"), 5 , 
                                                      ifelse(Date > as.Date("1911-01-01") & Date < as.Date("1921-01-01"), 6 , 
                                                             ifelse(Date > as.Date("1921-01-01") & Date < as.Date("1931-01-01"), 7,  
                                                                    ifelse(Date > as.Date("1931-01-01") & Date < as.Date("1940-01-01"), 8, 
                                                                           ifelse(Date > as.Date("1940-01-01") & Date < as.Date("1945-01-01"), 9, 
                                                                                  ifelse(Date > as.Date("1945-01-01") & Date < as.Date("1951-01-01"), 10, 
                                                                                         ifelse(Date > as.Date("1951-01-01") & Date < as.Date("1961-01-01"), 11,  
                                                                                                ifelse(Date > as.Date("1961-01-01") & Date < as.Date("1970-01-01"), 12, NA))))))))))))) %>%
  mutate(Demo2 = Demolitions) %>%
  mutate(Demo2 = ifelse(Date < as.Date("1951-01-01"), NA, Demo2)) %>%
  mutate(Housediff2 = Housediff) %>% 
  fill(Housediff2) %>%
  mutate(ChangeHousediff = Housediff2 - lag(Housediff2)) %>%
  mutate(ChangeHousediff = ifelse(ChangeHousediff == 0, NA, ChangeHousediff)) %>%
  fill(ChangeHousediff, .direction = "up") %>%
  mutate(ChangeHousediff = ifelse(Date < as.Date("1862-01-01"), NA, ChangeHousediff )) %>%
  mutate(ChangeHousediff = lead(ChangeHousediff)) %>% 
  mutate(Basebuild = coalesce(Total, Nethouse)) %>%
  group_by(Census1) %>% 
  mutate(Dem = sum(Demolitions, na.rm = TRUE)) %>% 
  mutate(Built = sum(Basebuild)) %>%
  mutate(Propbuild = Basebuild / Built) %>%
  mutate(PropDem =  Demo2 / Dem) %>%
  mutate(ImpDem = PropDem * ChangeHousediff) %>%
  mutate(ImpDem2 = Propbuild * ChangeHousediff) %>% 
  ungroup() %>%
  mutate(ImpDemtot = coalesce(ImpDem, ImpDem2)) %>%
  mutate(CumImpDem = cumsum(replace_na(ImpDemtot, 0))) %>% 
  mutate(Housingstock = ImpHouse + CumImpDem) %>%
  mutate(Houseestimate  = coalesce(Houses, Housingstock))

view(Total) 

Merge <- Total %>% 
         select(c("Date":"Housing associations", "Houseestimate")) %>% 
         mutate()

view(Merge)
