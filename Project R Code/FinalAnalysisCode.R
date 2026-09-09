setwd("C:/Users/S.Watling/Documents/Final European Data 2") 

library(tidyverse)
library(lubridate)
library(zoo) 
library(xts)
library(Hmisc)
library(patchwork)
library(stargazer)

Survey  <- read_csv("1960 Survey Data.csv") %>%
  fill(Country) %>%
  filter(Country != "Romania") %>%
  filter(Item %in% c("TotA", "TotPer", "TotB", "TotC")) 

nm1 <- Survey %>% select(c("Total":"7 or more"))
nm2 <- colnames(nm1)
nm2

Survey2 <- Survey %>%
  mutate_at(vars(nm2), ~ gsub("S", "8", .)) %>%
  mutate_at(vars(nm2), ~ gsub("[^0-9, .]", "", .)) %>% 
  mutate_at(vars(nm2), ~ gsub(" ", "", .)) %>%
  mutate_at(vars(nm2), ~ as.numeric(.)) %>%
  mutate_at(vars(nm2), ~ round(., digits = 1)) %>%
  arrange(Country) %>%
  pivot_longer(-c("Country", "Item", "Value", "Quantity")) %>%
  mutate(name = gsub("Total", "0", name), name = gsub("7 or more", "7", name)) %>%
  filter(Item == "TotA") %>%
  mutate(name = as.numeric(name)) %>%
  mutate(Rooms = name*value) %>%
  group_by(Country) %>%
  summarise(Rooms = sum(Rooms, na.rm = TRUE)) %>%
  mutate(Date = ifelse(Country == "Unitedkingdom", format(as.Date("1961-01-01"), format = "%Y-%m-%d"), format(as.Date("1960-01-01"),"%Y-%m-%d"))) %>%
  mutate(Date = ymd(Date))

Survey90s <- read_csv("1990 Survey Data 2.csv") %>%
  fill(Country) %>%
  fill(Date)

nm1 <- Survey90s %>% select(c("Value":"Persons"))
nm2 <- colnames(nm1)
nm2

Clean90s <- Survey90s %>%
  mutate_at(vars(nm2), ~ gsub("S", "8", .)) %>%
  mutate_at(vars(nm2), ~ gsub("O", "0", .)) %>%  
  mutate_at(vars(nm2), ~ gsub("s", "8", .)) %>%
  mutate_at(vars(nm2), ~ gsub("o", "0", .)) %>%
  mutate_at(vars(nm2), ~ gsub("I", "1", .)) %>%
  mutate_at(vars(nm2), ~ gsub("l", "1", .)) %>%
  mutate_at(vars(nm2), ~ gsub("\\'", "1", .)) %>%
  mutate_at(vars(nm2), ~ gsub("/)'", "7", .)) %>%
  mutate_at(vars(nm2), ~ gsub("\\/", "\\.", .)) %>%
  mutate_at(vars(nm2), ~ gsub("\\,", "\\.", .)) %>%
  mutate_at(vars(nm2), ~ gsub(" ", "", .)) %>% 
  mutate_at(vars(nm2), ~ gsub("[^0-9, .]", "", .)) %>%
  mutate_at(vars(nm2), ~ as.numeric(.)) %>%
  rename("Year" = "Date") %>%
  mutate(Date = parse_date_time(Year, orders = c("Y", "dmy"))) 

Processed90s <- Clean90s %>% 
  filter(Value %in% (8:10)) %>% 
  select(c("Country", "Rooms",  "Date")) %>%
  group_by(Country, Date) %>%
  mutate(Rooms = sum(Rooms, na.rm = TRUE)) %>%
  distinct() %>%
  rbind(Survey2) %>%
  mutate_if(.predicate =  is.numeric, 
            .fun = function(x){ifelse(x == 0,
                                      yes =  NA, no = x)}) %>%
  mutate(Country = gsub(" ", "", Country)) %>%
  mutate(Country = str_to_title(Country)) %>%
  mutate(Country = gsub("ssr", "SSR", Country)) %>%
  mutate(Item = 46) %>%
  rename("Value" = "Rooms")

Household60s <- Survey %>% 
  filter(Item == "TotA") %>% 
  select(c("Country", "Total")) %>% 
  mutate(Date = ifelse(Country == "Unitedkingdom" , format(as.Date("1961-01-01", format = "%Y-%m-%d")), format(as.Date("1960-01-01", format = "%Y-%m-%d")))) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Item = 55) %>%
  rename("Value" = "Total") %>%
  mutate(Value = gsub(" ", "", Value), Value = as.numeric(Value))
  
Household90s <- Clean90s %>% 
                select(c("Country", "Value", "Dwellings", "Date")) %>% 
                filter(Value == 8) %>%
                group_by(Country, Date) %>%
                mutate(Dwellings = sum(Dwellings, na.rm = TRUE)) %>%
                distinct() %>%
                mutate(Item = 55) %>%
                select(-c("Value")) %>%
                rename("Value" = "Dwellings") %>%
                rbind(Household60s)

Population <- read_csv("Population.csv") %>%
  select(-c("X", "Code", "Value")) %>%
  pivot_longer(-c("Country"), names_to = "Date", values_to = "Value") %>%
  mutate(Date = gsub("[^0-9]", "", Date)) %>%
  mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
  mutate(Item = 47)

Add50s <- read_csv("1950HouseDataV.csv") %>% 
          select(c("Country":"01/01/1957")) %>% 
          select(-c("Value")) %>%
          pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
          mutate(Value = as.numeric(Value), Date = dmy(Date)) %>%
          pivot_wider(names_from = Date, values_from = Value)

General <- read_csv("CompleteData1958-1991.csv") %>% 
           mutate_all(~ as.character(.)) %>% 
           select(-c("Value")) %>%
           pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
           mutate(Date = gsub("[^0-9]", "", Date)) %>%
           mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
           rbind(Processed90s) %>% 
           rbind(Household90s) %>% 
           mutate(Value = as.numeric(Value), Item = as.numeric(Item)) %>%
           pivot_wider(values_from = Value, names_from = Date) %>% 
           arrange(Item) %>% 
           group_by(Item) %>% 
           arrange(Country) %>%
           filter(Item %in% (1:19) | Item == 34 | Item == 39 | Item == 46 | Item == 55) %>% 
           left_join(Add50s, by = c("Country" = "Country", "Item" = "Item")) %>% 
           pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>% 
           mutate(Date = ymd(Date)) %>% 
           arrange(Date) %>% 
           arrange(Country) %>% 
           rbind(Population) %>% 
           mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Westgermany") , 1, 
                             ifelse(Country == "Unitedkingdom", 2, 
                              ifelse(Country %in% c("Czechoslovakia", "Hungary", "EastGermany", "Poland", "Yugoslavia"), 3, 0)))) %>% 
           filter(WestEurope != 0) %>%
           mutate(Item = gsub("^", "X", Item)) %>% 
           pivot_wider(names_from = "Item", values_from = "Value") %>%
           mutate(X5 = coalesce(X5, X55)) %>%
           select(-c("X55")) 

view(General)

Interpolated <- General %>%           
           mutate(X11b = X34 * X47, X34b = X11 / X47 ) %>%
           mutate(X11c = coalesce(X11, X11b), X34c = coalesce(X34, X34b)) %>% 
           group_by(Country) %>%
           mutate(impX11c = na.approx(X11c, maxgap = 4, na.rm = FALSE)) %>%
           mutate(impX34c = na.approx(X34c, maxgap = 4, na.rm = FALSE)) %>% 
           mutate(impX2b = na.approx(X2, maxgap = 4, na.rm = FALSE )) %>% 
           mutate(X1b = coalesce(X1, X7)) %>%
           mutate(impX1b = na.approx(X1b, maxgap = 4, na.rm = FALSE )) %>% 
           mutate(impX2b = ifelse(is.na(impX2b) , yes =  0, no = impX2b)) %>%
           mutate(impX50 = impX1b - impX2b) %>% 
           mutate(X50b = X5 - lag(X5)) %>% 
           mutate(X50b = ifelse(X50b < 0, yes =  NA, no = X50b)) %>% 
           mutate(impX50b = coalesce(impX50, X50b)) 
           
Calculation <- Interpolated %>%      
           mutate(Totrooms = cumsum(replace_na(impX11c, 0)), Totroomspp = cumsum(replace_na(impX34c, 0))) %>%
           mutate(Base1 = ifelse(Date <= as.Date("1961-01-01"), 1, NA)) %>%
           mutate(Base2 = ifelse(Date >= as.Date("1980-01-01"), 1, NA)) %>%
           mutate(Start = X46 * Base1) %>%
           mutate(Finish = X46 * Base2) %>% 
           mutate(Based1 = ifelse(Start > 0, 1, NA)) %>%
           mutate(Based2 = ifelse(Finish > 0, 1, NA)) %>%
           fill(Start, .direction = "downup") %>%
           fill(Finish, .direction = "downup") %>%
           mutate(Totimp = Totrooms * Based1) %>%
           mutate(Totfin = Totrooms * Based2) %>%
           fill(Totimp, .direction = "downup") %>%
           fill(Totfin, .direction = "downup") %>%
           mutate(Totroomsbase = Totrooms - Totimp) %>%
           mutate(Demolish = Totroomsbase * Based2) %>%
           mutate(Demolished = Demolish + Start - Finish ) %>%
           fill(Demolished, .direction = "downup") %>%
           mutate(Households = X5) %>% 
           fill(Households, .direction = "downup") %>%
           mutate(Cumadditions = cumsum(replace_na(impX50b, 0))) %>%
           mutate(Firststock = replace_na(X5, min(X5, na.rm = TRUE))) %>%
           mutate(Secondstock = first(Firststock)) %>%
           mutate(Basestock = Cumadditions + Secondstock) %>% 
  mutate(Basediff = X5 - Basestock) %>%
  mutate(Baseddiffimp = na.approx(Basediff, maxgap = Inf , na.rm = FALSE)) %>%
  mutate(ProxStop = Basestock + Baseddiffimp) %>%
  mutate(Percap = ProxStop/X47) %>%
  mutate(PerCapStock = coalesce(X6, Percap)) %>% 
  mutate(Newhouse = coalesce(X5, ProxStop)) %>%
           mutate(Ratio = impX2b / impX1b) %>% 
           mutate(Ratio = ifelse(Ratio == 0, NA, Ratio)) %>% 
  group_by(Country) %>%
  fill(Ratio, .direction = "up") %>% 
  mutate(Extradem = impX1b * Ratio) %>%
  mutate(ExtraX50b = impX1b - Extradem) %>% 
  mutate(ExtraSum = cumsum(ExtraX50b)) %>% 
  mutate(ExtraXF = Secondstock + ExtraSum) %>%
  mutate(Basediff2 = X5 - ExtraXF) %>% 
  mutate(Basediff2imp = na.approx(Basediff2, maxgap = 4, na.rm = FALSE)) %>%
  fill(Basediff2imp, .direction = "up") %>% 
  mutate(ExtraHouse = ExtraXF + Basediff2imp) %>%
  mutate(Housingstock = coalesce(Newhouse, ExtraHouse)) %>%
  mutate(NewPerCapStock = Housingstock / X47) %>% 
  mutate(PerCapStock = coalesce(PerCapStock, NewPerCapStock)) 

view(Calculation) 

Denmark <- Calculation %>%
           filter(Country == "Denmark")

view(Denmark)

Altspec <- Calculation %>% 
           group_by(Country) %>%
           mutate(Shift = X5 - lag(X5)) %>%
           mutate(Shift = ifelse(Shift < 0, NA, Shift)) %>% 
           mutate(Demimp = impX1b - Shift) %>%
           mutate(ProxX2 = coalesce(X2, Demimp)) %>% 
           mutate(ProxX2 = ifelse(ProxX2 < 0, NA, ProxX2)) %>%
           mutate(impaltX2 = na.approx(ProxX2, maxgap = Inf, rule = 2, na.rm = FALSE)) %>%
           mutate(CumaltX2 = cumsum(impaltX2))

view(Altspec)

Housestock <- Calculation %>% 
              group_by(WestEurope, Date) %>% 
              filter(Country != "Portugal" & Country != "Spain" & Country != "Greece" & Country != "Italy") %>% 
              filter(WestEurope != 3) %>%
              mutate(PerCapMean = weighted.mean(PerCapStock, X47, na.rm = TRUE)) 

view(Housestock)

Countryhouse <- Housestock %>%
                ungroup() %>%
                select(c("Country", "Date", "PerCapStock")) %>% 
                pivot_wider(names_from = Country, values_from = PerCapStock) %>%
                mutate(Britainbase = Unitedkingdom) %>%
                mutate_at(vars(-Date), ~ . *100 / Britainbase) %>%
                select(-c("Britainbase", "Unitedkingdom")) %>%
                pivot_longer(-c("Date"), names_to = "Country", values_to = "Value") %>%
                filter(Date > as.Date("1957-01-01"))

view(Countryhouse) 

CountryGraph <- Countryhouse %>%
                ggplot(aes(x = Date, y = Value, color = Country)) + geom_line(size = 1.5)

CountryGraph
CountryGraph + facet_wrap(~ Country, ncol = 4)

HouseGraph <- Housestock %>% 
              ungroup %>%
              select(c("Country", "Date", "PerCapStock")) %>% 
              filter(Country %in% c("Unitedkingdom", "Westgermany", "Netherlands", "Denmark", "Switzerland", "Finland", "Austria", "Sweden")) %>%
              filter(Date > as.Date("1954-01-01"), Date < as.Date("1980-01-01")) %>%
              filter()

view(HouseGraph)
              

IndexHouse <- Housestock %>%
  ungroup() %>%
  select(c("Country", "Date", "PerCapMean")) %>% 
  pivot_wider(names_from = Country, values_from = PerCapMean) %>% 
  select(c("Date","Unitedkingdom", "Westgermany")) %>%
  rename("Westeurope" = "Westgermany") %>%
  mutate(Britainbase = Unitedkingdom) %>%
  mutate(Unitedkingdom = Unitedkingdom*100/Britainbase, 
         Westeurope = Westeurope*100/Britainbase) %>% 
  select(-c("Britainbase")) %>% 
  pivot_longer(-c("Date"), names_to = "Region", values_to = "Value") %>%
  group_by(Region) %>%
  mutate(Avg = rollmean(Value, k = 2, fill = NA))

view(Housestock) 

HouseCalc <- IndexHouse %>% 
             ggplot(aes(x = Date, y = Value, color = Region)) + geom_line(size = 1.5)
HouseCalc 

HouseCalc <- IndexHouse %>% 
  filter(Date > as.Date("1950-01-01")) %>%
  ggplot(aes(x = Date, y = Avg, color = Region)) + geom_line(size = 1.5) + 
  labs(title = "Housing Stock Per Capita Average", y = "Houses Per capita")
HouseCalc 

HouseCalc <- Index

RoomCalc <- Calculation %>% 
            group_by(Country) %>%
            mutate(Totdem = sum(impX2b, na.rm = TRUE)) %>% 
            mutate(Fracdem = impX2b / Totdem) %>%
            mutate(Roomdem = Fracdem * Demolished) %>%
            mutate(CumRoomdem = cumsum(replace_na(Roomdem, 0))) %>%
            mutate(Netroom = Totrooms- CumRoomdem) %>%
            mutate(Roomstock= Start + Netroom) %>% 
            mutate(RoomstockPerCap = Roomstock / X47) %>% 
  group_by(WestEurope, Date) %>% 
  mutate(RoomCapMean = weighted.mean(RoomstockPerCap, X47, na.rm = TRUE)) %>% 
  mutate(WestEuropeword = gsub("1", "Western Europe", WestEurope), 
         WestEuropeword = gsub("2", "United Kingdom", WestEuropeword)) 

IndexRoom <- RoomCalc %>%
             ungroup() %>% 
             filter(Country != "Portugal" & Country != "Spain") %>%
             select(c("Country", "Date", "RoomCapMean")) %>% 
             pivot_wider(names_from = Country, values_from = RoomCapMean) %>% 
             select(c("Date","Unitedkingdom", "Westgermany")) %>%
             rename("Westeurope" = "Westgermany") %>%
             mutate(Britainbase = Unitedkingdom) %>%
             mutate(Unitedkingdom = Unitedkingdom*100/Britainbase, 
                    Westeurope = Westeurope*100/Britainbase) %>% 
             select(-c("Britainbase")) %>%
             pivot_longer(-c("Date"), names_to = "Region", values_to = "Value") 


Roomstock <- IndexRoom %>% 
  ggplot(aes(x = Date, y = Value, color = Region)) + geom_line(size = 1.5)
Roomstock 

Growth <- Housestock %>%
          mutate(Housingstock = coalesce(X5, ProxStop)) %>% 
          mutate(PctGrowth = impX1b*100 / Housingstock) %>%
          mutate(PerCapNetInc = impX50b*100/ Housingstock) %>%
          group_by(WestEurope, Date) %>%
          mutate(AvgGrowth = weighted.mean(PctGrowth, X47, na.rm = TRUE)) %>% 
          mutate(NetGrowth = weighted.mean(PerCapNetInc, X47, na.rm = TRUE)) %>%
         mutate(WestEuropeword = gsub("1", "Western Europe", WestEurope), 
         WestEuropeword = gsub("2", "United Kingdom", WestEuropeword)) 
          
Growthgraph <- Growth %>% 
               filter(Date > as.Date("1949-01-01")) %>%
               ggplot(aes(x = Date, y = AvgGrowth, color = WestEuropeword)) + geom_line(size = 1.5) + 
               ylim(0, 4) + labs(title = "Gross Housebuilding Rates", y = "Percentage of Housing Stock")
  
Growthgraph 

GrowthBar <- Growth %>% 
             filter(Date > as.Date("1949-01-01")  & Date < as.Date("1980-01-01")) %>% 
             group_by(Country) %>%
             summarise(GrossAvg = mean(PctGrowth, na.rm = TRUE)) %>% 
             mutate(ToHighlight = ifelse(Country == "Unitedkingdom", "yes", "no" )) %>%
             ggplot(aes(x = GrossAvg, y = reorder(Country, GrossAvg), fill = ToHighlight)) +
             geom_bar(stat="identity", color="black") + labs(x =  "Gross Housebuilding as a Percentage of the Housing Stock", 
              y = "Country", title = "Gross Housebuilding Rate 1950-1979") + scale_fill_manual(values = c("yes" = "yellow", "no" = "green")) + 
             theme(legend.position="none")

GrowthBar
             
NetGrowthgraph <- Growth %>% ggplot(aes(x = Date, y = NetGrowth, color = WestEuropeword)) + geom_line(size = 1.5) + 
                  ylim(0, 4) + labs(title = "Estimated Net Housebuilding Rates", y = "Percentage of Housing Stock")

NetGrowthgraph 


Tenure <- read_csv("1950s Tenure Data.csv") %>% 
          select(-"Tenure") %>% 
          mutate_if(.predicate =  is.numeric, 
            .fun = function(x){ifelse(x == 0,
                                      yes =  NA, no = x)}) %>%
          mutate(Item = gsub("^", "T", Item)) %>% 
          pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
          mutate(Date = gsub("[^0-9]", "", Date)) %>%
          mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
          pivot_wider(names_from = "Item", values_from = "Value") %>%
          full_join(Housestock, by = c("Country" = "Country", "Date" = "Date")) %>% 
          filter(WestEurope != 0) %>%
          group_by(Country) %>% 
          mutate(Popchange = X47 - lag(X47)) %>% 
          mutate(Popchangepct = Popchange*100 / X47) %>%
          mutate(impT4 = na.approx(T4, maxgap = 4, na.rm = FALSE), impT1 = na.approx(T1, maxgap = 4, na.rm = FALSE)) %>%
          mutate(Private = impT4/(impT1 + impT4), Public = impT1/(impT1 + impT4)) %>% 
          mutate(Pubhouse = Public * impX1b, Privhouse = Private * impX1b) %>%
          mutate(PubPct = Pubhouse*100  / Housingstock, PrivPct = Privhouse*100 / Housingstock) %>% 
  mutate(PerCapImp = na.approx(PerCapStock, maxgap = 4, na.rm = FALSE)) %>%
  mutate(PerCapStockChange = PerCapImp - lag(PerCapImp)) %>%
  mutate(PerCapStockPct = PerCapStockChange*100 / PerCapImp) %>%
  mutate(PerCapStockPct = ifelse(PerCapStockPct < 0 | PerCapStockPct > 4.5 , NA, PerCapStockPct)) %>% 
  mutate(PerCapStockPct = na.approx(PerCapStockPct, maxgap = 4, na.rm = FALSE)) %>% 
  mutate(PerCapStockPct = rollmean(PerCapStockPct, k = 3, fill =  NA)) %>%
          group_by(WestEurope, Date) %>%
          mutate(PubAvg = weighted.mean(PubPct, X47, na.rm = TRUE), PrivAvg = weighted.mean(PrivPct, X47, na.rm = TRUE)) %>%
  mutate(WestEuropeword = gsub("1", "Western Europe", WestEurope), 
         WestEuropeword = gsub("2", "United Kingdom", WestEuropeword))

view(Tenure)

Pubgraph <- Tenure %>% 
            filter(Date > as.Date("1949-01-01")) %>%
            ggplot(aes(x = Date, y = PubAvg, color = WestEuropeword)) + geom_line(size = 1.5) + 
            ylim(0, 2) + labs(title = "Public Housebuilding Rates", y = "Percentage of Housing Stock")
Pubgraph

Privgraph <- Tenure %>% 
             filter(Date > as.Date("1949-01-01")) %>%
             ggplot(aes(x = Date, y = PrivAvg, color = WestEuropeword)) + geom_line(size = 1.5) + 
             ylim(0, 3) + labs(title = "Private Housebuilding Rates", y = "Percentage of Housing Stock")
Privgraph 

SumTenuregraphbase <- Tenure %>% 
                filter(Date > as.Date("1954-01-01") & Date < "1980-01-01") %>% 
                group_by(Country) %>% 
                mutate(Pubsumavg = mean(PubPct, na.rm = TRUE), Privsumavg = mean(PrivPct, na.rm = TRUE), Totsumavg = Pubsumavg + Privsumavg) %>%
                select(c("Country", "Pubsumavg", "Privsumavg", "Totsumavg")) %>%
                pivot_longer(-c("Country", "Totsumavg"), names_to = "Item", values_to = "Value") %>%
                filter(Country != "Italy") %>% 
                distinct() 
                
  
SumTenuregraph <- SumTenuregraphbase %>% ggplot(aes(x = Value, y = reorder(Country, Totsumavg), fill = fct_rev(Item))) + 
                  geom_bar(position="stack", stat="identity")

PrivTenuregraph <- SumTenuregraphbase %>% 
                   filter(Item == "Privsumavg") %>% 
                   mutate(Tohighlight = ifelse(Country == "Unitedkingdom", "yes", "no")) %>%
                   ggplot(aes(x = Value, y = reorder(Country, Value), fill = Tohighlight)) + 
                   geom_bar( stat="identity", color = "black") + scale_fill_manual(values = c("yes" = "purple", "no" = "turquoise"), guide = "none") + 
                   labs(title = "Average Gross Private Housebuilding 1955-1980", y = "Country", x  = "Percentage of the Housing Stock")
 
PrivTenuregraph 

PubTenuregraph <- SumTenuregraphbase %>% 
  filter(Item == "Pubsumavg") %>%
  ggplot(aes(x = Value, y = reorder(Country, Value))) + 
  geom_bar( stat="identity", fill = "#F8766D")

PubTenuregraph

view(Tenure)

PercapTenuregraph <- Tenure %>% 
                     mutate(X1Percap = impX1b / X47) %>% 
                     mutate(PercapHousehold = Housingstock / X47) %>%
                     mutate(Percapgrowth = X1Percap / PercapHousehold) %>% 
                     mutate(Percappub = impT1 * Percapgrowth, Percappriv = impT4 * Percapgrowth) %>%
                     filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>% 
                     filter(Country != "Italy") %>%
                     group_by(Country) %>% 
                     mutate(Pubmean = mean(Percappub, na.rm = TRUE), Privmean = mean(Percappriv, na.rm = TRUE), Totmean = Pubmean + Privmean) %>%
                     select(c("Country", "Pubmean", "Privmean", "Totmean")) %>% 
                    pivot_longer(-c("Country", "Totmean"), names_to = "Item", values_to = "Value") %>%
                     distinct() %>%
                       ggplot(aes(x = Value, y = reorder(Country, Totmean), fill = fct_rev(Item))) + 
                       geom_bar(position="stack", stat="identity") 
                     
PercapTenuregraph 

Facet <- Tenure %>% 
         mutate(X1Percap = impX1b / X47) %>%
         ggplot((aes(x = Date, y = X1Percap, color = Country))) + geom_point() 
Facet + facet_wrap(~ Country, ncol = 4)

Facet2 <- Tenure %>% 
          mutate(PubPerCap = Pubhouse / X47) %>%
          ggplot((aes(x = Date, y = PubPerCap, color = Country))) + geom_point() 
Facet2 + facet_wrap(~ Country, ncol = 4) 

Facet3 <- Tenure %>% 
          mutate(PrivPerCap = Privhouse /X47) %>%
  ggplot((aes(x = Date, y = PrivPerCap, color = Country))) + geom_point() 
Facet3 + facet_wrap(~ Country, ncol = 4) 

Facet4 <- Tenure %>% 
           ggplot(aes(x = Date, y = Housingstock, color = Country)) + geom_point() 
Facet4 + facet_wrap(~ Country, ncol = 4) 

Facet5 <- Tenure %>% 
          filter(Country != "France" & Country != "Westgermany" & Country != "Unitedkingdom" & Country != "Italy" & Country != "Netherlands") %>%
  ggplot(aes(x = Date, y = Housingstock, color = Country)) + geom_point() 
Facet5 + facet_wrap(~ Country, ncol = 4) 

Facet6 <- Tenure %>% 
       mutate(impX2b = ifelse(impX2b == 0, NA, impX2b)) %>% 
       mutate(Pctdem = impX2b / Housingstock) %>% 
       group_by(Country) %>%
       mutate(Diffhouse = ProxStop - lag(ProxStop)) %>% 
       mutate(ImpDem = impX1b - Diffhouse) %>% 
       mutate(ImpDem = ifelse(ImpDem < 0 | ImpDem > 100, NA, ImpDem)) %>%
       ggplot(aes(x = Date, y = ImpDem, color = Country)) + geom_point() 
Facet6 + facet_wrap(~ Country, ncol = 4) 

Facet7 <- Tenure %>% 
          group_by(Country) %>%
          mutate(Diffstock = Housingstock - lag(Housingstock)) %>% #
          mutate(X50 = X1 - X2) %>% 
          mutate(X50d = coalesce(X50, Diffstock)) %>% 
          mutate(Netchangepercap = X50d) %>% 
          mutate(Netchangepercap = ifelse(Netchangepercap < 0, NA, Netchangepercap)) %>% 
          mutate(Netchangepercap = ifelse(Country == "Westgermany" & Netchangepercap > 800, NA, Netchangepercap)) %>% 
          filter(Date < as.Date("1989-01-01") & Date > as.Date("1950-01-01")) %>% 
          mutate(Netchangepercap = na.approx(Netchangepercap, maxgap = 4, na.rm = FALSE)) %>% 
          mutate(Netchangepct =  Netchangepercap*100/ Housingstock) %>% 
          mutate(PctGrowth = impX1b*100 / Housingstock) %>%
          group_by(WestEurope, Date) %>%
          mutate(Netchangewavg = weighted.mean(Netchangepct, X47, na.rm = TRUE)) %>%
          mutate(Netchangeavg = mean(Netchangepct, na.rm = TRUE)) %>%
          mutate(AvgGrowth = weighted.mean(PctGrowth, X47, na.rm = TRUE)) %>%
          mutate(Demolition = AvgGrowth - Netchangewavg) 

view(Facet7)

Facet9 <- Facet7 %>% 
          ggplot(aes(x = Date, y = Netchangewavg, color = WestEuropeword)) + geom_line(size = 1.5) +
          ylim(0, 4) + labs(title = "Net Increase in Housing Stock", y = "Percentage", col = "Region")

Facet9
    
Facet8 <-  Facet7 %>% 
           ggplot(aes(x = Date, y = Netchangepercap, color = WestEuropeword )) + geom_point()

Facet8 + facet_wrap(~ Country, ncol = 4)

Facet10 <- Facet7 %>% 
  ggplot(aes(x = Date, y = Demolition, color = WestEuropeword )) + geom_point()

Facet10 

view(Tenure)

PerCapGraph <- Tenure %>%
               ggplot(aes(x = Date, y = PerCapStockPct, color = Country)) + geom_point()
PerCapGraph + facet_wrap(~Country, ncol = 4)

BritNeth <- Tenure %>% 
            filter(Date < as.Date("1981-01-01")) %>%
            filter(Country == "Netherlands" | Country == "Unitedkingdom") %>% 
            ggplot(aes(x  = Date, y = PerCapStockPct, color = Country)) + geom_line(size = 1.5) + 
            scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth", y = "Percentage") + 
            geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black") + 
            geom_vline(xintercept = as.POSIXct(as.Date("1955-09-01")), color = "black", size = 1.5) + 
            geom_vline(xintercept = as.POSIXct(as.Date("1975-07-01")), color = "black", size = 1.5) 
BritNeth 

BritFin <- Tenure %>% 
  filter(Date < as.Date("1988-01-01")) %>%
  filter(Country == "Finland" | Country == "Unitedkingdom") %>% 
  ggplot(aes(x  = Date, y = PerCapStockPct, color = Country)) + geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth") + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black")

BritFin

BritSwiss <- Tenure %>% 
  filter(Date < as.Date("1988-01-01")) %>%
  filter(Country == "Switzerland" | Country == "Unitedkingdom") %>% 
  ggplot(aes(x  = Date, y = PerCapStockPct, color = Country)) + geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth") + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black")

BritSwiss 

BritFrance <- Tenure %>% 
  filter(Date < as.Date("1988-01-01")) %>%
  filter(Country == "Unitedkingdom" | Country == "France") %>% 
  ggplot(aes(x  = Date, y = PerCapStockPct, color = Country)) + geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth") + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black")

BritFrance 

BritSwe <- Tenure %>% 
  filter(Date < as.Date("1981-01-01")) %>%
  filter(Country == "Sweden" | Country == "Unitedkingdom") %>% 
  ggplot(aes(x  = Date, y = PerCapStockPct, color = Country)) + geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth", y = "Percentage") + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black") + 
  geom_vline(xintercept = as.POSIXct(as.Date("1954-07-01")), color = "black", size = 1.5) + 
  geom_vline(xintercept = as.POSIXct(as.Date("1974-07-01")), color = "black", size = 1.5) 

BritSwe 

BritWest <- Tenure %>% 
  filter(Date < as.Date("1988-01-01")) %>%
  filter(Country == "Westgermany" | Country == "Unitedkingdom") %>% 
  ggplot(aes(x  = Date, y = PerCapStockPct, color = Country)) + geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth") + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black")

BritWest 

BritDen <- Tenure %>% 
  filter(Date < as.Date("1988-01-01")) %>%
  filter(Country == "Denmark" | Country == "Unitedkingdom") %>% 
  ggplot(aes(x  = Date, y = PerCapStockPct, color = Country)) + geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth") + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black")

BritDen
view(BritDen) 

view(Tenure)

BritAus <- Tenure %>% 
  filter(Date < as.Date("1988-01-01")) %>%
  filter(Country == "Austria" | Country == "Unitedkingdom") %>% 
  ggplot(aes(x  = Date, y = PerCapStockPct, color = Country)) + geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth") + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black")

BritAus

BritAvg <- Tenure %>% 
            filter(Date < as.Date("1988-01-01")) %>% 
            filter(Country %in% c("Unitedkingdom", "France", "Netherlands", "Sweden", "Switzerland", "Finland", "Denmark")) %>% 
            group_by(Date, WestEuropeword) %>%
            mutate(PerCapavg = mean(PerCapStockPct, na.rm  = TRUE)) %>%
            ggplot(aes(x = Date, y = PerCapavg, color = WestEuropeword)) + geom_line(size = 1.5) + 
              scale_y_continuous(limits = c(0,4)) + labs(title = "Per Capita Housing Stock Growth") + 
              geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black")

BritAvg 

view(Tenure)

TotAvg <- Tenure %>% 
  filter(Date < as.Date("1988-01-01")) %>% 
  group_by(Date, WestEuropeword) %>%
  mutate(TotPct = PrivPct + PubPct) %>%
  mutate(Totmean = mean(TotPct, na.rm  = TRUE)) %>%
  ggplot(aes(x = Date, y = Totmean, color = WestEuropeword)) + geom_line(size = 1.5) + 
  scale_y_continuous(limits = c(0,4)) + labs(title = "Gross Housing Stock Growth") + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black")

TotAvg 

SumHaus <- Facet7 %>% 
           ungroup() %>% 
           filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>%
           group_by(Country) %>% 
           summarise(Mean = mean(Netchangepct, na.rm = TRUE)) 

SumHauspercap <- Facet7 %>% 
  group_by(Country) %>%
  mutate(Popchange = (X47 - lag(X47))/ X47) %>%
  ungroup() %>% 
  filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>%
  group_by(Country, Date) %>% 
  mutate(PercapGross = (impX1b / X47) / PerCapStock) %>% 
  mutate(Percapnet = (Netchangepercap /X47 ) / PerCapStock) %>%
  mutate(Percapadj = Percapnet - Popchange) %>%
  mutate(Firstpercap = Secondstock / X47) %>%
  ungroup() %>%
  group_by(Country) %>%
  mutate(Mean = mean(PercapGross, na.rm = TRUE) * 100, Meannet = mean(Percapnet, na.rm = TRUE) *100, Meanadj = mean(Percapadj, na.rm = TRUE)* 100) %>% 
  mutate(Demolitionratio = (Mean - Meannet) / Mean) %>% 
  mutate(InitialPerCapstock = first(Firstpercap)) %>%
  fill(InitialPerCapstock) 

view(SumHauspercap) 

GrossHouse <- SumHauspercap %>% 
              select(c("Country", "Date", "Meannet", "InitialPerCapstock", "Meanadj"))

view(GrossHouse)

Grossscatter <- GrossHouse %>%
  ggplot(aes(x = InitialPerCapstock, y = Meanadj)) + geom_point() + 
  geom_smooth(method = lm, size = 1) + ylim(0, 3)

Grossscatter        

Scatter <- Calculation %>%
           filter(Country != "Poland" & Country !=  "Greece") %>% 
           mutate(Householdnum = coalesce(X5, ProxStop)) %>% 
           mutate(Housepercap = Householdnum / X47) %>% 
           filter(Date > as.Date("1954-01-01"), Date < as.Date("1980-01-01")) %>% 
           group_by(Country) %>%
           mutate(Firsthousepercap = first(NewPerCapStock)) %>% 
           mutate(Lasthousepercap  = last(NewPerCapStock)) %>%
           mutate(PctChange = (Lasthousepercap - Firsthousepercap)*100 / Firsthousepercap) %>% 
           filter(WestEurope == 1 & Country != "Portugal" & Country !="Spain" | WestEurope == 2) 
           
  
Scatterplot <-  Scatter %>%  ggplot(aes(x = Firsthousepercap, y = PctChange)) + geom_point() + 
                geom_smooth(method = lm, size = 1) + ylim(0, 100)
Scatterplot

view(Scatter)

Brit <- Scatter %>%
        filter(Country == "Unitedkingdom")

view(Brit)

Counterfactual <- Scatter %>%
                  select(c("Firsthousepercap", "PctChange", "Country", "Lasthousepercap")) %>% 
                  distinct() %>%
                  mutate(Country2 = "United Kingdom", Brithaus = 302.5035, Britend = 381.4245) %>%
                  mutate(Increase = 1 + (PctChange / 100)) %>%
                  mutate(CounterBrit = Brithaus * Increase, DiffBrit = CounterBrit - Britend, options(scipen=999)) %>%
                  mutate(Britpop = 55.88 , Houses = DiffBrit * Britpop) %>%
                  mutate(Pctincrease = DiffBrit*100 / Britend) %>% 
                  mutate(Counterhouse = (Britpop * Britend) + Houses) %>% 
  mutate(BritPubPct = 0.48532108, BritPrivPct = 0.5146789) %>%
  mutate(BritPubAlt = Houses * BritPubPct, BritPrivAlt = Houses * BritPrivPct) %>%
                  mutate_if(is.numeric, round, 2)   

view(Counterfactual) 

stargazer(Counterfactual[c("Country", "Houses", "Pctincrease", "CounterBrit", "Counterhouse", "BritPubAlt")],  summary=FALSE, out = "Counterfactual.txt")

Countergraph <- Counterfactual %>% 
                filter(Country != "Belgium", Country != "Italy") %>%
                mutate(ToHighlight = ifelse(Country == "Unitedkingdom", "yes", "no" )) %>%
                ggplot(aes(x = PctChange, y = reorder(Country, PctChange), fill = ToHighlight)) + 
                geom_bar(stat = "identity", color = "black") + scale_fill_manual(values = c("yes" = "orange", "no" = "blue"), guide = "none") + 
                labs(x = "Percentage Change as of 1955 Housing Stock", 
                     y = "Country", 
                     title = "Change in Housing Stock Per Capita 1955-1979")
                
Countergraph 

#IndividualCountries 

view(Tenure)

Britain <- Tenure %>% 
           mutate(TotPct = PrivPct + PubPct) %>%
           filter(Country == "Unitedkingdom") %>% 
           ggplot() +geom_area(aes(x = Date, y = TotPct, fill = "Public")) +
           geom_area(aes(x = Date, y = PrivPct, fill = "Private")) + 
           scale_color_manual(name= NULL,values = c("black","black")) + 
           scale_fill_manual(values = c("tomato2", "steelblue")) + 
           scale_y_continuous(limits=c(0,4)) + labs(title = "Britain")
           
Britain

Sweden <- Tenure %>% 
          mutate(TotPct = PrivPct + PubPct) %>%
          filter(Country == "Sweden") %>% 
          ggplot() +geom_area(aes(x = Date, y = TotPct, fill = "Public")) +
          geom_area(aes(x = Date, y = PrivPct, fill = "Private")) + 
          scale_color_manual(name= NULL,values = c("black","black")) + 
          scale_fill_manual(values = c("tomato2", "steelblue")) + 
          scale_y_continuous(limits=c(0,4)) + labs(title = "Sweden")

Sweden 

Britain2 <- Tenure %>%
            filter(Country == "Unitedkingdom") %>% 
  mutate(TotPct = PrivPct + PubPct) %>%
  ggplot() +geom_area(aes(x = Date, y = TotPct, fill = "Public")) +
  geom_area(aes(x = Date, y = PrivPct, fill = "Private")) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("tomato2", "steelblue")) + 
  scale_y_continuous(limits=c(0,4)) + labs(title = "Britain", y = "", fill = "Tenure") + 
  theme(axis.text.y=element_blank(),
axis.ticks.y=element_blank())
  
Netherlands <- Tenure %>%  
  mutate(TotPct = PrivPct + PubPct) %>%
  filter(Country == "Netherlands") %>% 
  ggplot() +geom_area(aes(x = Date, y = TotPct, fill = "Public")) +
  geom_area(aes(x = Date, y = PrivPct, fill = "Private")) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("tomato2", "steelblue")) + 
  scale_y_continuous(limits=c(0,4)) + labs(title = "Netherlands", y = "Gross Building Rates", fill = "Tenure") 

France <- Tenure %>%  
  mutate(TotPct = PrivPct + PubPct) %>%
  filter(Country == "France") %>% 
  ggplot() +geom_area(aes(x = Date, y = TotPct, fill = "Public")) +
  geom_area(aes(x = Date, y = PrivPct, fill = "Private")) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("tomato2", "steelblue")) + 
  scale_y_continuous(limits=c(0,4)) + labs(title = "France", y = "Gross Building Rates", fill = "Tenure") 

France

Westgermany <- Tenure %>%  
  mutate(TotPct = PrivPct + PubPct) %>%
  filter(Country == "Westgermany") %>% 
  ggplot() +geom_area(aes(x = Date, y = TotPct, fill = "Public")) +
  geom_area(aes(x = Date, y = PrivPct, fill = "Private")) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("tomato2", "steelblue")) + 
  scale_y_continuous(limits=c(0,5)) + labs(title = "West Germany", y = "Gross Building Rates", fill = "Tenure") 

Westgermany 

Switzerland <- Tenure %>%  
  mutate(TotPct = PrivPct + PubPct) %>%
  filter(Country == "Switzerland") %>% 
  ggplot() +geom_area(aes(x = Date, y = TotPct, fill = "Public")) +
  geom_area(aes(x = Date, y = PrivPct, fill = "Private")) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("tomato2", "steelblue")) + 
  scale_y_continuous(limits=c(0,4)) + labs(title = "Switzerland", y = "Gross Building Rates", fill = "Tenure") 

Switzerland + Britain2 + plot_layout(guides = "collect") 

Netherlands + Britain2 + plot_layout(guides = "collect") 

Europe <- Tenure %>%  
  mutate(TotAvg = PrivAvg + PubAvg) %>% 
  filter(Date > as.Date("1949-01-01")) %>%
  group_by(Date, WestEuropeword) %>% 
  summarise(TotAvg = mean(TotAvg), PrivAvg = mean(PrivAvg)) %>%
  ggplot() +geom_area(aes(x = Date, y = TotAvg, fill = "Public")) +
  geom_area(aes(x = Date, y = PrivAvg, fill = "Private")) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("tomato2", "steelblue")) + 
  scale_y_continuous(limits=c(0,4)) + labs(y = "Gross Building Rates")

Europe + facet_wrap(~WestEuropeword, ncol = 2)       

view(Tenure)

Counterten <- Tenure %>% 
              ungroup() %>%
              mutate(TotAvg = PrivAvg + PubAvg) %>% 
              select(c("Country", "Date", "PerCapStock","impX1b","impT1", "impT4", "Housingstock")) %>%
              mutate(impT1 = na.approx(impT1, maxgap = Inf, rule = Inf, na.rm = FALSE)) %>%
              mutate(impT4 = na.approx(impT4, maxgap = Inf, rule = Inf, na.rm = FALSE)) %>%
              mutate(PctPub = impT1 / (impT1 + impT4), PctPriv = impT4 / (impT1 + impT4)) %>%
              filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>% 
              mutate(PubTot = PctPub * impX1b, PrivTot = PctPriv * impX1b) %>% 
              group_by(Country) %>%
              mutate(PubCum = cumsum(PubTot), PrivCum = cumsum(PrivTot)) %>%
              mutate(Startstock = first(Housingstock)) %>%
              filter(Date == as.Date("1979-01-01")) %>%
              mutate(PubShare = PubCum / Startstock, PrivShare = PrivCum / Startstock, TotShare = PubShare + PrivShare) %>%
              select(c("Country", "Startstock","PubShare" ,"PrivShare", "TotShare")) %>%
              mutate(BritainBase = 15418.00) %>%
              mutate(PubAlt = BritainBase*PubShare, PrivAlt = BritainBase*PrivShare, TotAlt  = BritainBase * TotShare) %>%
              mutate(PubPct = PubShare  / TotShare, PrivPct = PrivShare / TotShare) %>%
              mutate(BritPubPct = 0.48532108, BritPrivPct = 0.5146789) %>%
              mutate(BritPubAlt = TotAlt * BritPubPct, BritPrivAlt = TotAlt * BritPrivPct) %>%
              mutate_if(is.numeric, round, 2) 
              
view(Counterten)              

stargazer(Counterten[c("Country", "PubAlt", "PrivAlt", "TotAlt", "BritPubAlt", "BritPrivAlt")], summary=FALSE, out = "Counterfactual.txt")          
              
ComparisonNeth <- Tenure %>% 
                  filter(Country == "Netherlands" | Country == "Unitedkingdom") %>%
                  mutate(ImpT1 = na.approx(impT1, maxgap = 10, na.rm = FALSE), ImpT4 = na.approx(impT4, maxgap = Inf, na.rm = FALSE)) %>%
                  mutate(Pubpct = ImpT1 * impX1b / Housingstock, Privpct = ImpT4 * impX1b / Housingstock ) %>%
                  filter(Date > as.Date("1947-01-01"))

view(ComparisonNeth) 

NethBase <- ComparisonNeth %>% 
                mutate(Ratio = impX2b / impX1b) %>% 
                mutate(Ratio = ifelse(Ratio == 0, NA, Ratio)) %>% 
                group_by(Country) %>%
                fill(Ratio, .direction = "up") %>% 
                mutate(Extradem = impX1b * Ratio) %>%
                mutate(ExtraX50b = impX1b - Extradem) %>% 
                mutate(ExtraSum = cumsum(ExtraX50b)) %>%
                mutate(CorSum = ExtraSum - 329.87124) %>% 
                mutate(ExtraHouse = Secondstock + CorSum) %>%
                mutate(NewHouse = coalesce(Housingstock, ExtraHouse)) %>%
                mutate(Pubpct = Pubhouse * 100 / NewHouse) %>%
                mutate(Privpct = Privhouse * 100/NewHouse)
  
NethPubGraph <- NethBase %>% 
  ggplot(aes(x = Date, y = Pubpct)) +
  geom_area(aes(color = Country, fill = Country), 
            alpha = 0.5, position = position_dodge(0.8)) + 
  labs(title = "Non Profit Housing in the Netherlands and England", y = "Percentage of the Housing Stock") 

view(NethBase)
Nethsurpgraph <- NethBase %>%
                 select(c("Country", "Date", "Pubpct", "Privpct")) %>%
                 pivot_longer(-c("Country", "Date"), names_to = "Tenure", values_to = "Value")  %>%
                 unite("Merge", Country, Tenure) %>%
                 pivot_wider(names_from = Merge, values_from = Value) %>%
                 mutate(UkTot = Unitedkingdom_Pubpct + Unitedkingdom_Privpct) %>%
                 mutate(Nethsurppub = UkTot + Netherlands_Pubpct - Unitedkingdom_Pubpct) %>%
                 mutate(Nethsurppriv = Nethsurppub + Netherlands_Privpct - Unitedkingdom_Privpct) %>% 
                 ggplot() + geom_area(aes(x = Date, y = Nethsurppriv, fill = "Private Surplus")) + 
                 geom_area(aes(x = Date, y = Nethsurppub, fill = "Public Surplus")) + 
                 geom_area(aes(x = Date, y = UkTot, fill = "Uk Public")) + 
                 geom_area(aes(x = Date, y = Unitedkingdom_Privpct, fill = "UK Private")) + 
                 scale_color_manual(name= NULL,values = c("black","black")) + 
                 scale_fill_manual(values = c("yellow", "green", "steelblue", "tomato2")) + 
                 scale_y_continuous(limits=c(0,4)) + labs(title = "Netherlands Surplus Over Britain", y = "Gross Building Rates") 

Nethsurpgraph

ComparisonSwe <- Tenure %>% 
                 filter(Date > as.Date("1949-01-01")) %>%
                 filter(Country == "Sweden" | Country == "Unitedkingdom") %>%
  ggplot(aes(x = Date, y = PubPct)) +
  geom_area(aes(color = Country, fill = Country), 
            alpha = 0.5, position = position_dodge(0.8)) 

ComparisonSwe 

view(Tenure)

Swesurpgraph <- Tenure %>% 
                filter(Date > as.Date("1949-01-01")) %>%
                filter(Country == "Sweden" | Country == "Unitedkingdom") %>% 
  select(c("Country", "Date", "PubPct", "PrivPct")) %>%
  pivot_longer(-c("Country", "Date"), names_to = "Tenure", values_to = "Value")  %>%
  unite("Merge", Country, Tenure) %>%
  pivot_wider(names_from = Merge, values_from = Value) %>%
  mutate(UkTot = Unitedkingdom_PubPct + Unitedkingdom_PrivPct) %>%
  mutate(Swesurppub = UkTot + Sweden_PubPct - Unitedkingdom_PubPct) %>%
  mutate(Swesurppriv = Swesurppub + Sweden_PrivPct - Unitedkingdom_PrivPct) %>% 
  ggplot() + geom_area(aes(x = Date, y = Swesurppriv, fill = "Private Surplus")) + 
  geom_area(aes(x = Date, y = Swesurppub, fill = "Public Surplus")) + 
  geom_area(aes(x = Date, y = UkTot, fill = "Uk Public")) + 
  geom_area(aes(x = Date, y = Unitedkingdom_PrivPct, fill = "UK Private")) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("yellow", "green", "tomato2", "steelblue")) + 
  scale_y_continuous(limits=c(0,4)) + labs(title = "Swedish Surplus Over Britain", y = "Gross Building Rates") 
              
Swesurpgraph
#Note to self. Need to find better household base numbers for Austria and Norway before publication
