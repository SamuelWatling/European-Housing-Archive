setwd("C:/Users/S.Watling/Documents/1990s Tenure csv files")

library(tidyverse)
library(lubridate) 
library(janitor)
library(zoo)
library(xts) 

Construction <- read_csv("Constructed2.csv") 
names(Construction) <- paste(names(Construction), Construction[1, ], sep = "_")
Construction <- Construction[-1, ]

Constructionalt <- Construction %>% 
  rename_with(~ gsub("_NA", "", .x)) %>%
  rename_with(~ gsub("\\.\\.\\.", "", .x)) %>% 
  mutate_all(~ as.character(.)) %>% 
  select(-c("Value")) %>%
  pivot_longer(-c("Item"), names_to = "Index", values_to = "Value") %>%
  separate(Index, c("Country", "Date"), sep = "_") %>%
  mutate(Country = gsub("[0-9]", "", Country)) %>%
  mutate(Value = gsub("(X)", "0", Value)) %>%
  mutate(Value = gsub("S", "8", Value)) %>%
  mutate(Value = gsub("[^0-9, .]+", "", Value)) %>% 
  mutate(Country = gsub(" ", "", Country)) %>% 
  mutate(Country = str_to_title(Country)) %>%
  filter(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Germany", "Unitedkingdom")) 

TenureI <- read_csv("Tenure1.csv") 
names(TenureI) <- paste(names(TenureI), TenureI[1, ], sep = "_")
TenureI <- TenureI[-1, ] 

Tenurealt <- TenureI %>% 
  rename_with(~ gsub("_NA", "", .x)) %>%
  rename_with(~ gsub("\\.\\.\\.", "", .x)) %>%
  mutate_all(~ as.character(.)) %>%
  pivot_longer(-c("Country"), names_to = "Index", values_to = "Value") %>%
  separate(Index, c("Tenure", "Date"), sep = "_") %>%
  mutate(Tenure = gsub("[0-9]", "", Tenure)) %>%
  mutate(Tenure = gsub("Public", "X1", Tenure), Tenure = gsub("Private", "X4", Tenure)) %>% 
  mutate(Value = gsub("(X)", "0", Value)) %>%
  mutate(Value = gsub("S", "8", Value)) %>%
  mutate(Value = gsub("[^0-9, .]+", "", Value)) %>% 
  mutate(Country = gsub(" ", "", Country)) %>% 
  mutate(Country = str_to_title(Country)) %>%
  filter(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Germany", "Unitedkingdom")) %>%
  rename("Item" = "Tenure") %>%
  mutate(Date = gsub("^", "Y", Date))

TenureII <- Constructionalt %>%
  filter(Item %in% c("X1", "X2", "X3", "X4", "X5", "X6", "X7")) %>%
  rbind(Tenurealt) %>%
  mutate(Value = as.numeric(Value)) %>%
  pivot_wider(names_from = Date, values_from = Value) %>%
  select(-c("NA")) %>%
  mutate(`1990` = coalesce(`1990`, Y1990), `1993` = coalesce(`1993`, Y1993)) %>%
  select(-c("Y1990", "Y1993")) %>% 
  rename_with(~ gsub("Y", "", .x)) 

view(TenureII) 

TenureIII <- TenureII %>% 
  mutate(Country = gsub("Germany", "Westgermany", Country)) %>%
  select(-c("1980")) %>%
  pivot_longer(-c("Item", "Country"), names_to = "Date", values_to = "Value") %>%
  pivot_wider(names_from = Item, values_from = Value) %>%
  mutate(Tot = X1 + X4) %>%
  mutate(across("X1":"X7", ~ .x / Tot)) %>% 
  pivot_longer(-c("Country", "Date"), names_to = "Item", values_to = "Value") %>%
  pivot_wider(names_from = Date, values_from = Value) %>%
  mutate(Item = gsub("X", "", Item)) %>%
  filter(Item != "Tot") %>%
  mutate(Item = as.numeric(Item)) 

view(TenureIII) 

Tenure <- read_csv("1950s Tenure Data.csv") %>% 
  mutate(across("X1948":"01/01/1988", ~ .x / 100)) %>% 
  mutate(`1989` = NA) %>%
  right_join(TenureIII, by = c("Country" = "Country", "Item" = "Item")) %>% 
  select(-c("Tenure")) %>%
  pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
  mutate(Date = gsub("X", "", Date), Date = gsub("01/01/", "", Date)) %>%
  mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
  pivot_wider(names_from = Date, values_from = Value) %>%
  mutate(across("1948-01-01":"1998-01-01", ~ ifelse(.x == 0, NA, .x))) %>% 
  pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
  pivot_wider(names_from = Item, values_from = Value) %>%
  mutate(Date = ymd(Date)) 
  
view(Tenure)

Denmark <- read_csv("Denmark Tenure.csv") %>%
           fill(Type) %>%
           filter(Type == "Completed") %>%
           select(-c("Type", "Tenure")) %>%
           mutate(Country = "Denmark") %>%
           pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
           pivot_wider(names_from = Item, values_from = Value) %>%
           select(-c("NA")) %>%
           mutate(`1` = `2` + `3`, `4` = `5` + `6`, `7` = NA) %>%
           mutate(Tot = `1` + `4`) %>%
           mutate(across(`5`:`4`, ~ .x / Tot)) %>%
           select(-c("Tot"))

view(Denmark)

Switzerland1 <- read_csv("SwitzerlandOld.csv") %>%
                slice((10:42))
view(Switzerland1)

Switzerland2 <- read_csv("SwitzerlandNew.csv") %>%
                slice((10:18)) %>% 
                rbind(Switzerland1) %>%
                pivot_longer(-c("Year"), names_to = "Item", values_to = "Value") %>%
                mutate(Item = gsub("\\..*", "", Item)) %>% 
                mutate(Value = gsub("[^0-9]+", "", Value)) %>%
                mutate(Value = na_if(Value, "")) %>% 
                mutate(Value = as.numeric(Value)) %>%
                group_by(Year, Item) %>% 
                summarise(Value = mean(Value, na.rm = TRUE)) %>% 
                ungroup() %>%
                pivot_wider(names_from = Item, values_from = Value) %>% 
                mutate(Tot = `1` + `4`) %>% 
                mutate_at(vars(`1`:`8`), ~ .x / `Tot`) %>%
                mutate(Country = "Switzerland") %>%
                select(c("Year":"7", "Country")) %>% 
                mutate(`3` = NA) %>%
                rename("Date" = "Year")

view(Switzerland2)

Germany <- read_csv("Germany Public Housing.csv") %>%
           slice(- 1) %>%
           fill(Country) %>% 
           select(c("Country", "Item", "Year", "Dwellings")) %>%
           pivot_wider(names_from = Item, values_from = Dwellings) %>% 
           mutate(across(`7`:`3`, ~ as.numeric(.))) %>%
           mutate(`1` = `2` + `3`, `4` = `7` - `1`) %>%
           mutate(across(`2`:`4`, ~ .x / `7`)) %>%
           rename("Date" = "Year") %>%
           mutate(`5` = NA, `6` = NA, `7` = NA) 
          

view(Germany)

Unitedkingdom <- read_csv("BritainTenure.csv") %>%
                 filter(Date > 1988) %>% 
                 mutate(across(X7:`2`, ~ gsub("[^0-9]+", "", .x))) %>%
                 mutate(across(X7:`2`, ~ as.numeric(.x))) %>%
                 mutate(`1` = `2` + `3`) %>%
                 mutate(across(`4`:`1`, ~ .x / X7)) %>%
                 mutate(Country = "Unitedkingdom") %>%
                 mutate(`5` = NA, `6` = NA, `7` = NA) %>%
                 select(-c("X7"))

view(Unitedkingdom) 

Sweden <- read_csv("Sweden Tenure.csv") %>% 
          select(-c("type of building", "type of ownership")) %>%
          pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>% 
          group_by(Date, Item, Country) %>%
          summarise_all(sum, na.rm = TRUE) %>% 
          pivot_wider(names_from = Item, values_from = Value) %>%
          mutate(`1` = `2` + `3`, `4` = `6` + `7`, `5` = NA, Tot = `1` + `4`) %>%
          mutate(across(`2`:`5`, ~ .x / Tot)) %>%
          select(-c("Tot"))

view(Sweden) 

Austria <- read_csv("AustrianApartments.csv") %>%
            select(- c("...2", "Not classifiable", "Not classifiable <0>")) %>%
            fill(Date) %>% 
            group_by(Date) %>%
            summarise_all(sum) %>%
            select(c("Date", "Private owner Constructed":"Other legal entity Constructed")) %>% 
            rename("5" = "Private owner Constructed", "3" = "Non-profit building association Constructed", 
                   "2" = "Regional authority Constructed", "7" = "Other legal entity Constructed") %>% 
            mutate(`1` = `2` + `3`, `4` = `5` + `7`, Total = `1` + `4`) %>%
            mutate(across(`5`:`4`, ~ .x / Total)) %>%
            select(-c("Total")) %>%
            mutate(Country = "Austria", `6` = NA)

view(Austria) 

Austria2 <- read_csv("AustriaModern.csv") %>%
            select(-c("...2")) %>%
            fill(Date) %>%
            group_by(Date) %>% 
            summarise_all(sum) %>%
            mutate(Date = gsub("[^0-9]+", "", Date)) %>%
            rename("5" = "Private Person", "7" = "Other entities", "2" = "Local Government", "3" = "Non Profit") %>%
            mutate(`1` = `2` + `3`, `4` = `5` + `7`, `6` = NA, Country = "Austria", Tot = `1` + `4`) %>% 
            mutate(across(`5`:`4`, ~ .x / Tot)) %>%
            select(-c("Tot")) %>% 
            rbind(Austria)
            
view(Austria2) 

Netherlands <- read_csv("NetherlandsX2.csv") %>%
               mutate(`1` = `1` / `7`,`4` = `4` / `7`) %>%
               mutate(`2` = NA, `3` = NA, `5` = NA, `6` = NA, `7` = NA, Country = "Netherlands")

view(Netherlands) 

Ireland1 <- read_csv("IrelandSocHouse1.csv") %>%
            slice(1:2) %>% 
            pivot_longer(-c("Tenure"), names_to = "Date", values_to = "Value") %>%
            mutate(Tenure = gsub("Private Housing", "4", Tenure), Tenure = gsub("Social Housing", "1", Tenure)) %>% 
            pivot_wider(names_from = Tenure, values_from = Value) %>% 
            mutate(Tot = `1` + `4` ) %>% 
            mutate(across(`1`:`4`, ~ .x / Tot)) %>%
            select(-c("Tot")) %>%
            mutate(`2` = NA, `3` = NA) 
  
view(Ireland1) 

Ireland2 <- read_csv("Ireland2.csv") %>% 
            slice(8:13) %>%
            select(c("Date", "2", "3", "TOTAL OUTPUT")) %>%
            mutate_all(~ gsub("\\,", "", .)) %>%
            mutate_all(~ as.numeric(.)) %>%
            mutate(`1` = `2` + `3`, `4` = `TOTAL OUTPUT` - `1`) %>%
            relocate(`TOTAL OUTPUT`, .after = `4`) %>%
            mutate(across(`2`:`4`, ~ .x / `TOTAL OUTPUT`)) %>%
            select(-c("TOTAL OUTPUT"))

view(Ireland2) 

Ireland3 <- read_csv("Ireland3.csv") %>%
            row_to_names(row_number = 2) %>%
            select(c("Date", "Build Total", "Total Construction")) %>%
            slice(1:5) %>%
            mutate_all(~ gsub("\\,", "", .)) %>% 
            mutate_all(~ as.numeric(.)) %>%
            rename("1" = "Build Total") %>% 
            mutate(`4` = `Total Construction` - `1`, `4` = `4` / `Total Construction`,`1` = `1` / `Total Construction`) %>%
            select(-c("Total Construction")) %>%
            mutate(`2` = NA, `3` = NA)
            
view(Ireland3)

Ireland <- rbind(Ireland1, Ireland2, Ireland3) %>% 
           mutate(`5` = NA, `6` = NA, `7` = NA, Country = "Ireland")

view(Ireland)

Compiled <- rbind(Germany, Unitedkingdom, Denmark, Switzerland2, Sweden, Austria2, Netherlands, Ireland) %>% 
            mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
            full_join(Tenure, by = c("Country" = "Country", "Date" = "Date")) %>%
            mutate(`1` = coalesce(`1.x`, `1.y`), `2` = coalesce(`2.x`, `2.y`), 
                   `3` = coalesce(`3.x`, `3.y`), `4` = coalesce(`4.x`, `4.y`), 
                   `5` = coalesce(`5.x`, `5.y`), `6` = coalesce(`6.x`, `6.y`), 
                   `7` = coalesce(`7.x`, `7.y`)) %>%
            select(c("Country", "Date", "1":"7")) %>% 
            arrange(Country) %>%
            arrange(Date) %>%
            arrange(Country) %>% 
            pivot_longer(-c("Country", "Date"), names_to = "Item", values_to = "Value") %>%
            pivot_wider(names_from = Date, values_from = Value) %>% 
            select(-c("2021-01-01")) 

  
view(Compiled) 

Ireland <- Compiled %>%
           filter(Country == "Ireland") 

Sweden <- Compiled %>% 
          filter(Country == "Sweden")

view(Sweden)

write.csv(Compiled, "ModernTenure.csv")

