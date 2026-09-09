setwd("C:/Users/S.Watling/Documents/Final European Data") 

library(tidyverse)
library(lubridate)
library(zoo) 
library(xts)

Roomstab <- read_csv("1950HouseDataIV.csv") %>%
  select(c("Item", "Value"))
view(Roomstab)

RoomzWest <- read_csv("1950HouseDataIV.csv") %>%
         select(-c("...1", "Value")) %>% 
         filter(Item %in% (12:16) | Item > 45) %>% 
         mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Westgermany", "Unitedkingdom", "Unitedstates") , 1, 0)) %>%
         filter(WestEurope == 1) %>% 
         select(-"WestEurope") %>%
         pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
         mutate(Item = gsub("^", "X", Item)) %>%
         pivot_wider(names_from = "Item", values_from = "Value") %>%
         mutate(Date = dmy(Date)) 

view(RoomzWest)

RoomzGraph12 <- RoomzWest %>% ggplot(aes(x = Date, y = X12, color = Country)) + geom_point()
RoomzGraph12 + facet_wrap(~ Country, ncol = 4) 

RoomzGraph13 <- RoomzWest %>% ggplot(aes(x = Date, y = X13, color = Country)) + geom_point()
RoomzGraph13 + facet_wrap(~ Country, ncol = 4) 

RoomzGraph14 <- RoomzWest %>% ggplot(aes(x = Date, y = X14, color = Country)) + geom_point()
RoomzGraph14 + facet_wrap(~ Country, ncol = 4) 

RoomzGraph15 <- RoomzWest %>% ggplot(aes(x = Date, y = X15, color = Country)) + geom_point()
RoomzGraph15 + facet_wrap(~ Country, ncol = 4)

RoomzGraph16 <- RoomzWest %>% ggplot(aes(x = Date, y = X16, color = Country)) + geom_point()
RoomzGraph16 +  facet_wrap(~ Country, ncol = 4)
              
Rooms <- RoomzWest  %>% 
         mutate(X12b = X12 + X13, X14b = X14 + X15) %>% 
         rename("16b" = "X16") %>%
         select(-c("X12", "X13", "X14", "X15")) %>% 
         pivot_longer(-c(Country, Date), names_to = "Item", values_to = "Value") %>%
         mutate(Item = gsub("X", "", Item)) %>%
         pivot_wider(names_from = "Date", values_from = "Value") %>%
         filter(Country != "Austria")
         
view(Rooms)

RoomsUK <- read_csv("1950srooms.csv") %>%
           slice(6:130) 

names(RoomsUK) <- as.character(unlist(RoomsUK[1,]))
RoomsUK2 <- RoomsUK[-1,]
           
RoomsUK3 <- RoomsUK2 %>% 
            rename("Country" = "Country Pays") %>%
            select("Country", "Item", "1948":"1957") %>%
            mutate(Country = gsub("([^A-Za-z])+", "", Country)) 

nm1 <- RoomsUK3 %>% select("1948":"1957") 
nm2 <- colnames(nm1)
nm2
  
view(RoomsUK3)

RoomsUK4 <- RoomsUK3 %>% 
            mutate_at(vars(nm2), ~ gsub("[^0-9, \\.]+", "", .)) %>%
            mutate_at(vars(nm2), ~ as.numeric(.)) %>%
            fill(Country) %>% 
            mutate(Country = str_to_title(Country)) %>%
            filter(Country == "Unitedkingdom" | Country == "Austria") %>%
            mutate_if(.predicate =  is.numeric, 
            .fun = function(x){ifelse(test = is.na(x),
                                      yes =  0, no = x)}
                                                        ) %>%
            group_by(Country, Item) %>% 
            summarise_at(vars(nm2), ~ sum(.)) %>%
            mutate(Item = gsub("81", "12b", Item), Item = gsub("82", "14b", Item), Item = gsub("83", "16b", Item)) %>%
            filter(Item != 5) %>%
            pivot_longer(-c(Country, Item), names_to = "Date", values_to = "Value") %>%
            mutate(Date = parse_date_time(Date, orders = "y")) %>%
            pivot_wider(names_from = "Date", values_from = "Value")

view(RoomsUK4) 

Rooms60sUK <- read_csv("SecondIIjoin1960s.csv") %>%
              select(-c("...1", "Value")) %>%
              mutate(Item = as.numeric(Item)) %>%
              filter(Country == "Unitedkingdom" & Item %in% (63:66) | Country == "Unitedkingdom" & Item %in% (12:19) | Country == "Austria" & Item %in% (12:16) | Country == "Austria" & Item == "61") %>%
              pivot_longer(-c("Country", "Item"), names_to = "Date" , values_to = "Value") %>% 
              mutate(Item = gsub("^", "X", Item)) %>%
              pivot_wider(names_from = "Item", values_from = "Value") %>%
              mutate(Date = gsub("X", "", Date)) %>%
              mutate(Date = parse_date_time(Date, order = "y")) %>% 
              mutate_if(.predicate =  is.numeric, 
              .fun = function(x){ifelse(test = is.na(x),
                                      yes =  0, no = x)}
                                                       ) %>%
              mutate(X12b = X12 + X13, X14b = X14 + X15, X16b = X16 + X61 + X17 + X18, X65b = X65 + X66) %>%
              select(-c("X12","X13", "X14", "X15", "X16", "X17", "X18","X19", "X61", "X65", "X66")) %>%
              pivot_longer(-c("Country", "Date"), names_to = "Item", values_to = "Value") %>%
              pivot_wider(names_from = "Date", values_from = "Value") %>%
              mutate(Item =  gsub("X63", "X12b", Item), Item =  gsub("X64", "X14b", Item), Item =  gsub("X65b", "X16b", Item)) 
              
              view(Rooms60sUK)

nm1 <- Rooms60sUK %>% select("1958-01-01":"1988-01-01") 
nm2 <- colnames(nm1)
nm2

Rooms60sUK2 <- Rooms60sUK %>%
               group_by(Country, Item) %>%
               summarise_at(vars(nm2), ~ sum(.)) %>%
               mutate(Item = gsub("X", "", Item))

view(Rooms60sUK2)

FullroomsUK <- RoomsUK4 %>% 
               full_join(Rooms60sUK2, by = c("Item" = "Item", "Country" = "Country"))

view(FullroomsUK)            

Full50s <- rbind(FullroomsUK, Rooms) %>%
           mutate(Item = gsub("12b", "1 to 2 Rooms", Item)) %>%
           mutate(Item = gsub("14b", "3 to 4 Rooms", Item)) %>%
           mutate(Item = gsub("16b", "5 Rooms or more", Item)) %>%
           group_by(Item) %>%
           arrange(Country) %>% mutate_if(.predicate =  is.numeric, 
                                          .fun = function(x){ifelse(x == 0,
                                                                    yes =  NA, no = x)}
           ) %>% 
           pivot_longer(-c("Country", "Item"),names_to = "Date", values_to = "Percent") %>% 
           mutate(Date = ymd(Date)) %>%
           group_by(Country, Item) %>% 
           mutate(ImpPercent = na.approx(Percent, maxgap = 4, na.rm = FALSE)) %>%
           select(-c("Percent"))

view(Full50s) 
view(Roomsseries)

Population <- read_csv("population.csv") %>%
  select(-c("X", "Value", "Code")) %>%
  mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Westgermany", "Unitedkingdom", "Unitedstates") , 1, 0)) %>%
  filter(WestEurope == 1) %>% 
  select(-"WestEurope") %>%
  pivot_longer(-c(Country), names_to = "Date", values_to = "Value") %>%
  mutate(Date =  gsub("[^0-9-]", "", Date)) %>%
  mutate(Date = parse_date_time(Date, orders = c("Y", "ymd"))) %>%
  rename("Population" = "Value")

Avggraph <- Full50s %>% 
  pivot_wider(names_from = "Item", values_from = "ImpPercent") %>%
  left_join(Population, by = c("Country" = "Country", "Date" = "Date")) %>%
  mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Westgermany") , 1, 
                             ifelse(Country == "Unitedkingdom", 2 , 0))) %>% 
  filter(WestEurope != 0) %>%
  group_by(Country) %>% 
  group_by(Date, WestEurope) %>% 
  mutate(Meansmall = weighted.mean(`1 to 2 Rooms`,Population, na.rm = TRUE)) %>%
  mutate(Meanmed = weighted.mean(`3 to 4 Rooms`,Population, na.rm = TRUE)) %>%
  mutate(Meanbig = weighted.mean(`5 Rooms or more`,Population, na.rm = TRUE)) %>%
  mutate(WestEuropeword = gsub("1", "Western Europe", WestEurope), 
         WestEuropeword = gsub("2", "United Kingdom", WestEuropeword))
         
view(Avggraph)

Graphsmall <- Avggraph %>% ggplot(aes(x = Date, y = Meansmall, color = WestEuropeword)) + geom_point()
Graphsmall

Graphmed <-  Avggraph %>% ggplot(aes(x = Date, y = Meanmed, color = WestEuropeword)) + geom_point()
Graphmed

Graphbig <- Avggraph %>% ggplot(aes(x = Date, y = Meanbig, color = WestEuropeword)) + geom_point()
Graphbig

Roomgraph <- read_csv("1950HouseDataIV.csv") %>%
  select(-c("...1", "Value")) %>%
  filter(Item == 7) %>% 
  select(-"Item") %>%
  pivot_longer(-c("Country"), names_to = "Date", values_to = "Value") %>%
  mutate(Date = dmy(Date)) %>% 
  group_by(Country) %>%
  mutate(Value = na.approx(Value, maxgap = 4, na.rm = FALSE)) %>% 
  right_join(Full50s, by = c("Country" = "Country", "Date" = "Date")) %>% 
  left_join(Population, by = c("Country" = "Country", "Date" = "Date")) %>%
  rename("Housebuilding" = "Value") %>%
  pivot_wider(names_from = "Item", values_from = "ImpPercent") %>% 
  mutate(PerCapitaHouse = Housebuilding / Population) %>%
  mutate(Smalltot =  `1 to 2 Rooms`* PerCapitaHouse / 100 ) %>%
  mutate(Medtot = `3 to 4 Rooms`* PerCapitaHouse / 100 ) %>%
  mutate(Bigtot = `5 Rooms or more`* PerCapitaHouse / 100 ) %>%
  mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Westgermany") , 1, 
                             ifelse(Country == "Unitedkingdom", 2 , 0))) %>% 
  filter(WestEurope != 0) %>%
  group_by(Date, WestEurope) %>% 
  mutate(MeanSmalltot = weighted.mean(Smalltot,Population, na.rm = TRUE)) %>%
  mutate(MeanMedtot = weighted.mean(Medtot,Population, na.rm = TRUE)) %>%
  mutate(MeanBigtot = weighted.mean(Bigtot,Population, na.rm = TRUE)) %>%
  mutate(WestEuropeword = gsub("1", "Western Europe", WestEurope), 
         WestEuropeword = gsub("2", "United Kingdom", WestEuropeword))
  
view(Roomgraph)

SmallGraph <- Roomgraph %>% ggplot(aes(x = Date, y = MeanSmalltot, color = WestEuropeword)) + geom_line(size= 1.5) + 
              labs(title = "1 to 2 Room Dwellings Built", y = "Dwellings per 1000 People", color = "Region")
   
SmallGraph

MedGraph <- Roomgraph %>% ggplot(aes(x = Date, y = MeanMedtot, color = WestEuropeword)) + geom_line(size= 1.5) + 
  labs(title = "3 to 4 Room Dwellings Built", y = "Dwellings per 1000 People", color = "Region")
MedGraph

BigGraph <- Roomgraph %>% ggplot(aes(x = Date, y = MeanBigtot, color = WestEuropeword)) + geom_line(size= 1.5) + 
  labs(title = "5 or more Room Dwellings Built", ylab = "Dwellings per 1000 People", color = "Region")
BigGraph

Britainslice <- Roomgraph %>% 
                filter(Country == "Unitedkingdom") %>%
                mutate(Smallgross = Housebuilding * `1 to 2 Rooms`* 10) %>%
                mutate(Medgross = Housebuilding * `3 to 4 Rooms`* 10) %>% 
                mutate(Largegross = Housebuilding * `5 Rooms or more`* 10) %>% 
                ungroup() %>%
                select(c("Date", "Smallgross", "Medgross","Largegross")) %>% 
                pivot_longer(-c("Date"), names_to = "Item", values_to = "Value")

view(Britainslice)

Rooms60sEur <- read_csv("SecondIIjoin1960s.csv") %>%
               select(-c("...1", "Value")) %>%
               filter(Item == 11 | Item == 34) %>%
               pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Quantity") %>%
               mutate(Date = gsub("[^0-9-]", "", Date)) %>% 
               mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
               mutate(Item = gsub("^", "X", Item)) %>%
               pivot_wider(names_from = Item, values_from = Quantity) %>%
               left_join(Population, by = c("Country" = "Country", "Date" = "Date")) %>%
               mutate(X34b = X11 / Population, X11b = X34 * Population) %>%
               mutate(X11c = coalesce(X11, X11b), X34c = coalesce(X34, X34b)) %>%
               mutate(X11c = round(X11c, digits = 1), X34c = round(X34c, digits = 1))

view(Rooms60sEur) 

Rooms60sWest <- Rooms60sEur %>% 
                mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Westgermany", "Unitedkingdom", "Unitedstates") , 1, 0)) %>%
                filter(WestEurope == 1) %>% 
                select(-"WestEurope") 
                filter(Country %in% c("Austria", "Belgium", "Denmark", "Finland", "Ireland", "Norway", "Portugal", "Sweden", "Switzerland"))

Roomgraph <- Rooms60sWest %>% ggplot(aes(x = Date, y = X11c, color = Country)) + geom_point()
Roomgraph +  facet_wrap(~ Country, ncol = 4)

Roomspergraph <- Rooms60sWest %>% ggplot(aes(x = Date, y = X34c, color = Country)) + geom_point()
Roomspergraph +  facet_wrap(~ Country, ncol = 4)

RoomsImpWest <- Rooms60sWest %>% 
                mutate(X34cimp = na.approx(X34c , maxgap = 4, na.rm = FALSE)) %>%
  mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Westgermany") , 1, 
                             ifelse(Country == "Unitedkingdom", 2 , 0))) %>% 
  filter(WestEurope != 0) %>%
  group_by(Date, WestEurope) %>% 
  mutate(MeanX34imp = weighted.mean(X34cimp,Population, na.rm = TRUE)) %>%
  mutate(WestEuropeword = gsub("1", "Western Europe", WestEurope), 
         WestEuropeword = gsub("2", "United Kingdom", WestEuropeword))

view(RoomsImpWest)              

RoomsWestGraph <- RoomsImpWest %>% ggplot(aes(x = Date, y = MeanX34imp, color = WestEuropeword)) + geom_line(size = 1.5) + 
                  labs(y = "Per 1000 People", title = "European Room Construction", color = "Region")
RoomsWestGraph

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
  summarise(TotRooms = sum(Rooms, na.rm = TRUE)) %>%
  mutate(Date = ifelse(Country == "Unitedkingdom", format(as.Date("1961-01-01"), format = "%Y-%m-%d"), format(as.Date("1960-01-01"),"%Y-%m-%d"))) %>%
  mutate(Date = ymd(Date))

view(Survey2)       
               
NewRoomData <- RoomsImpWest %>%
               left_join(Survey2, by = c("Country" = "Country", "Date" = "Date")) %>%
               group_by(Country) %>%
               fill(TotRooms) %>% 
               mutate(X11cimp = na.approx(X11c, maxgap = 4, na.rm = FALSE)) %>%
               mutate(CumRooms = cumsum(coalesce(X11cimp, 0)) + X11cimp*0) %>%
               mutate(DateRoomsBase = ifelse(Date == as.Date("1961-01-01"), 1, NA)) %>%
               mutate(CumRoomsBase = CumRooms * DateRoomsBase) %>%
               fill(CumRoomsBase, .direction = "downup") %>%
               mutate(Roomchange = CumRooms - CumRoomsBase) %>%
               mutate(RoomStock = Roomchange + TotRooms) %>%
               mutate(RoomStockPerCap = RoomStock/Population) 
               
view(NewRoomData)

NewRoomGraph <- NewRoomData %>% ggplot(aes(x = Date, y = RoomStockPerCap, color = Country)) + geom_line(size = 1.5)
NewRoomGraph

RoomStockSum <- NewRoomData %>% 
                select(-c("WestEuropeword")) %>%
                filter(Country %in% c("Unitedkingdom", "Denmark", "Westgermany", "Sweden", "Norway", "Netherlands")) %>%
                pivot_longer(-c("Country", "Date"), names_to = "Item", values_to = "Value") %>%
                filter(Item == "RoomStockPerCap") %>%
                pivot_wider(names_from = Country, values_from = Value) %>%
                mutate(BritainBase = Unitedkingdom)

nm1 <- RoomStockSum %>% select("Denmark":"Westgermany") 
nm2 <- colnames(nm1)
nm2 

RoomStockIndex <- RoomStockSum %>% 
                  mutate_at(vars(nm2), ~ . * 100 / BritainBase) %>%
                  select(c("Date", "Sweden", "Westgermany", "Unitedkingdom", "Netherlands")) %>%
                  pivot_longer(-c("Date"), names_to = "Country", values_to = "Value")
                  
view(RoomStockIndex)

RoomIndex <- RoomStockIndex %>% ggplot(aes(x = Date, y = Value, color = Country)) + geom_line(size = 1.5)
RoomIndex 


