setwd("C:/Users/S.Watling/Documents/1990s csv files")

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
 
view(Constructionalt)

ConstructionI <- read_csv("Constructed1.csv") %>% 
                 mutate(WestEurope = ifelse(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Germany") , 1, 0)) %>%
                 filter(WestEurope == 1) %>% 
                 mutate(across("1990":"1998", ~ gsub("[^0-9, .]+", "", .x))) %>%
                 rename("Item" = "WestEurope") %>%
                 mutate(Item = gsub("1", "7", Item)) %>% 
                 pivot_longer(-c("Country", "Item")) %>%
                 rename("Date" = "name", "Value" = "value") %>%
                 rbind(Constructionalt) %>%
                 filter(Item %in% (1:45)) %>%
                 mutate(Item = gsub("^", "X", Item)) %>% 
                 distinct(Country, Item, Date, .keep_all = TRUE) %>%
                 pivot_wider(names_from = Date, values_from = Value) %>%
                 select(-"NA") 
                 
view(ConstructionI) 

Increases <- read_csv("Increases.csv")
names(Increases) <- paste(names(Increases), Increases[1, ], sep = "_")
Increases <- Increases[-1, ] 

Increases1 <- Increases %>% 
              rename_with(~gsub("_NA", "", .x)) %>%
              filter(Item %in% (1:2) | Item %in% (5:6)) %>%
              select(-c("Description")) %>%
              pivot_longer(-c("Item"), names_to = "Index", values_to = "Value") %>%
              separate(Index, c("Country", "Date"), sep = "_") %>% 
  mutate(Value = gsub("(X)", "0", Value)) %>%
  mutate(Value = gsub("S", "8", Value)) %>%
  mutate(Value = gsub("[^0-9, .]+", "", Value)) %>%  
  mutate(Value = gsub(" ", "", Value)) %>% 
  mutate(Item = gsub("^", "X", Item)) %>%
  mutate(Country = gsub(" ", "", Country)) %>% 
  mutate(Country = str_to_title(Country)) %>%
  filter(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Germany", "Unitedkingdom")) 

view(Increases1)

HouseholdI <- read_csv("HousingStock.csv") %>%
              fill(Country) %>% 
              select(-c("Demolition", "UseChange")) %>%
              pivot_longer(-c("Country", "Year"), names_to = "Item", values_to = "Value") %>%
  mutate(Value = gsub("(X)", "0", Value)) %>%
  mutate(Value = gsub("S", "8", Value)) %>%
  mutate(Value = gsub("[^0-9, .]+", "", Value)) %>%  
  mutate(Value = gsub(" ", "", Value)) %>% 
  mutate(Country = gsub(" ", "", Country)) %>% 
  mutate(Country = str_to_title(Country)) %>%
  filter(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Germany", "Unitedkingdom")) %>% 
  mutate(Year = gsub("^", "X", Year)) %>%
  rename("Date" = "Year") %>%
  distinct(Country, Date, Item, .keep_all = TRUE) %>% 
  rbind(Increases1) %>%
  arrange(Date) %>%
  pivot_wider(names_from = Date, values_from = Value) %>%
  mutate(X1990 = coalesce(X1990, `1990`), X1991 = coalesce(X1991, `1991`)) %>%
  select(-c("1990", "1991")) %>%
  rename("X1992" = "1992") %>%
  full_join(ConstructionI, by = c("Country" = "Country", "Item" = "Item")) %>%
  mutate(`1990` = coalesce(`1990`, X1990), `1991` = coalesce(`1991`, X1991), 
         `1992` = coalesce(`1992`, X1992), `1993` = coalesce(`1993`, X1993), 
         `1994` = X1994 , `1995` = coalesce(`1995`, X1995), 
         `1996` = coalesce(`1996`, X1996), `1997` = coalesce(`1997`, X1997), 
         `1998` = coalesce(`1998`, X1998)) %>%
  select(c("Country", "Item", "1990":"1994", ))
  
view(HouseholdI)

PerCap <- read_csv("Percap.csv") %>%
          mutate(across("1980":"1994", ~ gsub("(X)","0" , .x))) %>% 
          mutate(across("1980":"1994", ~ gsub("O","0" , .x))) %>% 
          mutate(across("1980":"1994", ~ gsub("[^0-9, .]+","" , .x))) %>% 
          mutate(Country = gsub(" ", "", Country)) %>% 
          mutate(Country = str_to_title(Country)) %>%
          filter(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Germany", "Unitedkingdom")) %>%
          mutate(Item = "X30") %>%
          mutate(`1991` = NA, `1992` = NA)

view(PerCap)

FullSet <- rbind(HouseholdI, PerCap) %>% 
           mutate(Item = gsub("X", "", Item)) %>% 
           mutate(across("Item":"1994", ~ as.numeric(.x))) %>%
           arrange(Item) %>%
           group_by(Item) %>%
           arrange(Country)

view(FullSet)

MainData <- read_csv("CompleteData1958-1991.csv") %>% 
            mutate(Country = gsub("Westgermany", "Germany", Country)) %>%
            filter(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Germany", "Unitedkingdom")) %>%
            right_join(FullSet, by = c("Country" = "Country", "Item" = "Item")) %>%
            mutate(X1980 = coalesce(X1980, `1980`), X1990 = coalesce(X1990, `1990`), X1991 = coalesce(X1991, `1991`)) %>%
            select(-c("1980", "1990", "1991")) %>%
            rename_with(~ gsub("X", "", .x)) 

view(MainData)

FullData <- read_csv("1950HouseDataV.csv") %>% 
            select(c("Country":"01/01/1957")) %>%
            right_join(MainData, by = c("Country" = "Country", "Item" = "Item")) %>%
            select(-c("Value.x")) %>% 
            arrange(Item) %>%
  group_by(Item) %>%
  arrange(Country) %>%
  mutate(`1993` = gsub("349888.0", "34988.8", `1993`))

view(FullData)

Denmark <- read_csv("Denmark.csv") %>% 
           slice(1:3) %>%
           summarise(across(`1980`:`2020`, ~ sum(.x, na.rm = TRUE))) %>%
           pivot_longer(everything(), names_to = "Date", values_to = "X5") %>%
           mutate(Country = "Denmark") %>%
           mutate(X5 = as.numeric(X5) / 1000)
  
view(Denmark)

France <- read_csv("France.csv") %>%
          slice(1) %>%
          mutate(across(`1982`:`2021`, ~ gsub("[^0-9,.]+", "", .x))) %>%
          select(c("1982":"2020")) %>%
          pivot_longer(everything(), names_to = "Date", values_to = "X5") %>%
          mutate(Country = "France")
view(France) 

Netherlands <- read_csv("Netherlands.csv") %>%
               mutate(Country  = "Netherlands") %>%
               mutate(X7 = X7 / 1000, X2 = X2 / 1000)
               
view(Netherlands)

Sweden <- read_csv("Sweden.csv") %>% 
          summarise(across(`1990`:`2020`, ~ sum(.x, na.rm = TRUE))) %>%
          pivot_longer(everything(), names_to = "Date", values_to = "X5") %>%
          mutate(Country = "Sweden") %>%
          mutate(X5 = as.numeric(X5) / 1000)
  
view(Sweden)

Norway <- read_csv("Norway.csv") %>% 
          summarise(across(`2001`:`2020`, ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "Date", values_to = "X5") %>%
  mutate(Country = "Norway") %>% 
  mutate(X5 = as.numeric(X5) / 1000)
          
view(Norway)

Belgium <- read_csv("Belgium.csv")  %>%
           filter(...3 == "T1" | ...3 == "compteur") %>%
           select(c("1995...6":"2020...201"))

names(Belgium) <- paste(names(Belgium), Belgium[1, ], sep = "_")
Belgium <- Belgium[-1, ]

BelgiumI <- Belgium %>% 
            pivot_longer(everything(), names_to = "Index", values_to = "Value") %>% 
            separate(Index, c("Date", "Item"), sep = "_") %>% 
            mutate(Item = gsub("[^0-9]+", "", Item)) %>%
            filter(Item == 7) %>%
            mutate(Item = gsub("7", "X5", Item)) %>%
            mutate(Date = gsub("\\..*", "", Date)) %>%
            select(-c("Item")) %>% 
            rename("X5" = "Value") %>% 
            mutate(X5 = gsub("[^0-9]+", "",X5)) %>% 
            mutate(X5 = as.numeric(X5) / 1000) %>%
            mutate(Country = "Belgium")

view(BelgiumI)

Britain <- read_csv("Britain2.csv") %>% 
           select(c("Year", "Total")) %>%
           slice(20:62) %>%
           mutate(Year = gsub(" 5", "", Year), Year = gsub(" 6", "", Year)) %>%
           mutate(Total = gsub("\\,", "", Total)) %>%
           mutate(Year = as.numeric(Year), Total = as.numeric(Total)) %>%
           filter(Year < 2021) %>% 
           rename("Date" = "Year", "X5" = "Total") %>% 
           mutate(Country = "Unitedkingdom") %>%
           mutate(Date = as.character(Date))
           
view(Britain) 

Switzerland <- read_csv("Switzerland.csv") %>%
               mutate(Country = "Switzerland") %>%
               mutate(X5 = X5 / 1000) %>%
               rename("Date" = "Year") 

view(Switzerland)

Finland <- read_csv("Finand.csv") %>%
           mutate(X5 = X5 / 1000, Country = "Finland") 
           
view(Finland) 

Germany <- read_csv("Germany.csv") %>%
           row_to_names(row_number = 4) %>%
           mutate(Date = gsub("31/12/", "", Date)) %>%
           mutate(X5 = as.numeric(Total) / 1000 ) %>%
           select(c("Date", "X5")) %>%
           slice(1:26) %>%
           mutate(Country = "Germany")
view(Germany)


view(Netherlands)

Neth <- Netherlands %>% 
        select(c("Country", "Date", "X7", "X5", "X37", "X36", "X2")) 

view(Neth)
  
Comp <- rbind(BelgiumI, France, Denmark, Norway, Sweden, Switzerland, Britain, Finland, Germany) 
        
view(Comp) 

CompI <- Comp %>% 
         expand(Country, Date) %>%
         full_join(Comp, by = c("Country" = "Country", "Date" = "Date")) %>%
         mutate(Date = as.numeric(Date))

view(CompI)

New7 <- read_csv("Kohl Data.csv") %>% 
      select(-c("iso")) %>% 
      filter(Year > 1947) %>% 
      rename("Date" = "Year") %>%
      mutate(Country = gsub("UK", "Unitedkingdom", Country)) %>%
      mutate(across(X7:X37, ~ .x / 1000))

view(New7)

X7 <- New7 %>% 
      full_join(CompI, by = c("Country" = "Country", "Date" = "Date")) %>%
      mutate(X2 = NA) %>%
      filter(Country != "Netherlands")
  
view(X7)

fullcomp <- rbind(X7, Neth) %>% 
            mutate(X5 = as.numeric(X5)) %>%
            mutate(Date = as.character(Date)) %>% 
            filter(Country %in% c("Austria","Belgium", "Denmark", "Finland", "France", "Greece", "Ireland", "Italy", "Netherlands", "Norway", "Portugal", "Spain", "Switzerland","Sweden", "Germany", "Unitedkingdom")) 

view(fullcomp)
view(FullData)
view(FullDataJoin)

FullDataJoin <- FullData %>% 
                select(-c("Value.y")) %>% 
                mutate(across(`01/01/1948`:`1994`, ~ as.numeric(.x))) %>%
                pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value" ) %>%
                filter(Item == 2 | Item == 5 | Item == 7) %>%
                mutate(Date = gsub("01/01/", "", Date)) %>% 
                pivot_wider(names_from = Item, values_from = Value) %>%
                full_join(fullcomp, by = c("Country" = "Country", "Date" = "Date")) %>%
                mutate(Y5 = coalesce(`5`, X5), Y7 = coalesce(`7`, X7), Y7 = coalesce(Y7, X36), Y2 = coalesce(`2`, X2)) %>%
                select(c("Country", "Date","Y5", "Y7", "Y2")) %>% 
                arrange(Date) %>%
                arrange(Country) %>%
                group_by(Country) %>%
                mutate(Firsthaus = Y5) %>%
                fill(Firsthaus, .direction = "up") %>% 
                mutate(OriginHaus = first(Firsthaus)) %>%
                mutate(ImpDem = na.approx(Y2, maxgap = 4, na.rm = FALSE)) %>% 
                mutate(Ratio = ImpDem / Y7) %>%
                fill(Ratio, .direction = "downup") %>% 
                mutate(ImpTot = na.approx(Y7, maxgap = 4, na.rm = FALSE)) %>% 
                mutate(ImpDem = ifelse(is.na(ImpDem), 0, ImpDem)) %>%
                mutate(NetHaus = ImpTot - Ratio*ImpTot) %>% 
                mutate(Cumuhaus = cumsum(replace_na(NetHaus, 0))) %>%
                mutate(SecondHaus = Firsthaus + Cumuhaus) %>%
                mutate(DumStock = ifelse(is.na(Y5), NA, 1)) %>%
                mutate(CumuAdj = Cumuhaus * DumStock) %>% 
                fill(CumuAdj, .direction = "up") %>% 
                mutate(Cumustart = first(CumuAdj)) %>% 
                mutate(Adjcumu = Cumuhaus - Cumustart) %>%  
                mutate(Origcumu = OriginHaus + Adjcumu) %>%
                mutate(DiffHaus = Origcumu - Y5) %>%
                mutate(Adjhaus = SecondHaus - CumuAdj) %>% 
                mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
                mutate(DiffHaus = Y5 - Origcumu) %>%
                mutate(ImpDiffHaus = na.approx(DiffHaus, maxgap = 35, na.rm = FALSE)) %>%
                mutate(Genhaus = Origcumu + ImpDiffHaus) %>%
                mutate(Esthaus = coalesce(Genhaus, Adjhaus)) %>%
                select(c("Country":"Y2", "Esthaus")) %>%
                rename("X5 (Estimate)" = "Esthaus")

view(FullDataJoin) 

Dataprocess <- FullDataJoin %>% 
               pivot_longer(-c("Country", "Date"), names_to = "Item", values_to = "Value") %>%
               pivot_wider(names_from = Date, values_from = "Value")
               
view(Dataprocess)

write.csv(Dataprocess, "Modern Household.csv")

Norway <- FullDataJoin %>%
          filter(Country == "Norway")

view(Norway)

Household <- FullDataJoin %>% 
             ggplot(aes(x = Date, y = `X5 (Estimate)`, color = Country)) + geom_point()
Household + facet_wrap(~ Country, ncol = 4) 

Populationearly <- read_csv("population.csv") %>%
                   mutate(Country = gsub("Westgermany", "Germany", Country))

view(Populationearly)

Populationlate <- read_csv("Modern Population.csv") %>% 
                  row_to_names(row_number = 4) %>%
                  mutate(across(`1960`:`2020`, ~ .x / 1000000)) %>%
                  select(c("Country Name",  "1991":"2020")) %>%
                  mutate(`Country Name` = gsub(" ", "", `Country Name`)) %>%
                  mutate(`Country Name` = str_to_title(`Country Name`)) %>%
                  right_join(Populationearly, by = c("Country Name" = "Country")) %>%
                  select(-c("X", "Code","Value")) %>% 
                  rename("Country" = "Country Name") %>%
                  pivot_longer(-c("Country"), names_to = "Date", values_to = "X50") %>%
                  mutate(Date = gsub("[^0-9]+", "", Date)) %>%
                  mutate(X50 = signif(X50, 4)) %>%  
                  mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
                  right_join(FullDataJoin, by = c("Country" = "Country", "Date" = "Date")) %>% 
                  arrange(Date) %>%
                  arrange(Country)

write.csv(Populationlate, "FullHousehold.csv")

view(Populationlate)
Processed <- Populationlate %>%  
                  mutate(PerCapStock = Esthaus / X50) %>% 
                  mutate(PerCapInc = PerCapStock - lag(PerCapStock)) %>%
                  ungroup() %>%
                  mutate(PerCapPct = PerCapInc*100 / PerCapStock)
 
view(Populationlate)

PopulationgraphNeth <- Populationlate %>%
                   filter(Country %in% c("Netherlands", "Unitedkingdom")) %>% 
                   group_by(Country) %>%
                   mutate(PerCapAvg = rollmean(PerCapPct, k = 3, fill = NA)) %>% 
                   mutate(PerCapAvg = ifelse(Date == as.Date("1988-01-01") & Country == "Unitedkingdom", NA, PerCapAvg)) %>% 
                   mutate(PerCapAvg = ifelse(Date == as.Date("1991-01-01") & Country == "Unitedkingdom", NA, PerCapAvg)) %>% 
                   mutate(PerCapimp = na.approx(PerCapAvg, maxgap = 4, na.rm = FALSE)) %>%
                   ggplot(aes(x = Date, y = PerCapimp, color = Country)) + geom_line(size = 1.2) + 
                   geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black") + 
                   labs(title = "Per Capita Housing Stock Growth", y = "Percentage of the Housing Stock")

PopulationgraphNeth 

PopulationgraphSwe <- Populationlate %>%
  filter(Country %in% c("France", "Unitedkingdom")) %>% 
  group_by(Country) %>%
  mutate(PerCapAvg = rollmean(PerCapPct, k = 3, fill = NA)) %>% 
  mutate(PerCapAvg = ifelse(Date == as.Date("1988-01-01") & Country == "Unitedkingdom", NA, PerCapAvg)) %>% 
  mutate(PerCapAvg = ifelse(Date == as.Date("1991-01-01") & Country == "Unitedkingdom", NA, PerCapAvg)) %>% 
  mutate(PerCapimp = na.approx(PerCapAvg, maxgap = 4, na.rm = FALSE)) %>%
  ggplot(aes(x = Date, y = PerCapimp, color = Country)) + geom_line(size = 1.2) + 
  geom_hline(yintercept = 0, color = "black") + geom_vline(xintercept = as.POSIXct(as.Date("1950-01-01")), color = "black") + 
  labs(title = "Per Capita Housing Stock Growth", y = "Percentage of the Housing Stock") 

PopulationgraphSwe 

SumGraph <- Populationlate %>% 
            select(c("Country", "Date", "PerCapStock")) %>%
            filter(Date == as.Date("1955-01-01") |  Date == as.Date("1980-01-01") |
                   Date == as.Date("2020-01-01")) %>%
            pivot_wider(names_from = Date, values_from = PerCapStock) %>% 
            mutate(Britpop80 = 56.31, Britpop20 = 67.08) %>%
            mutate(Change80 = `1980-01-01` - `1955-01-01`, Change20 = `2020-01-01` - `1980-01-01`, ChangeCumu = `2020-01-01` - `1955-01-01` ) %>%
            mutate(Counterfac80 = Change80 * Britpop80, Counterfac20 = Change20 * Britpop20, CounterfacCumu = ChangeCumu * Britpop20) %>%
            mutate(Deficit80 = Counterfac80 - 4483.695, Deficit20 = Counterfac20 - 3923.5375, DeficitCumu = CounterfacCumu - 9264.795) %>%
            mutate(Frac80 = Change80  / `1955-01-01`, Frac20 = Change20 / `1980-01-01`, FracCumu = ChangeCumu / `1955-01-01`) %>%
            mutate(Brit55 = 302.4917, Brit80 = 382.1169, Brit20 = 440.6073) %>%
            mutate(Counter80 = Frac80 * Brit55, Counter20 = Frac20 * Brit80, CounterCumu = FracCumu * Brit55) %>%
            mutate(Stock80 = Brit55 + Counter80, Stock20 = Brit80 + Counter20, StockCumu = Brit55 + CounterCumu) %>%
            mutate(Deficit80 = Stock80 - Brit80, Deficit20 = Stock20 - Brit20, DeficitCumu = StockCumu - Brit20) %>%
            mutate(Output80 = Deficit80 * Britpop80, Output20 = Deficit20 * Britpop20, OutputCumu = DeficitCumu * Britpop20)

view(Populationlate) 

TenureIII <- TenureII %>%
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

Date7 <- New7 %>%
         mutate(Date = parse_date_time(Date, orders = c("Y", "dmy")))

view(Date7)

Tenure <- read_csv("1950s Tenure Data.csv") %>%
          mutate(across("X1948":"01/01/1988", ~ .x / 100)) %>% 
          mutate(`1989` = NA) %>%
          right_join(TenureIII, by = c("Country" = "Country", "Item" = "Item")) %>% 
          select(-c("Tenure")) %>%
          pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
          mutate(Date = gsub("X", "", Date), Date = gsub("01/01/", "", Date)) %>%
          mutate(Date = parse_date_time(Date, orders = c("Y", "dmy"))) %>%
          mutate(Item = gsub("^", "T", Item)) %>%
          pivot_wider(names_from = Item, values_from = Value) %>%
          left_join(Populationlate, by = c("Country" = "Country", "Date" = "Date")) %>%
          left_join(Date7, by = c("Country" = "Country", "Date" = "Date")) %>%
          mutate(Y7 = coalesce(Y7, X7), TotPub = Y7*T1, TotPriv = Y7 * T4, 
                 PubPct = (TotPub * 100) / Esthaus, PrivPct = (TotPriv * 100) / Esthaus, TotPct = PubPct + PrivPct ) %>%
          mutate(across("T1":"TotPct", ~ ifelse(. == 0, NA, .)))
          
view(Tenure)      

PostWarSummary <- Tenure %>% 
           filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>%
           group_by(Country) %>% 
           summarise(across(c("PerCapPct","PubPct":"TotPct"), 
                            list(mean = ~ mean(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE),
                                 sd = ~ sd(.x, na.rm = TRUE), min = ~  min(.x, na.rm = TRUE), 
                                 IQR = ~ IQR(.x, na.rm = TRUE)))) %>%
           mutate(PerCapRatio = PerCapPct_max / PerCapPct_mean, PrivPctRatio = PrivPct_max / PrivPct_mean, 
                  PrivPctVar = PrivPct_sd / PrivPct_mean)

view(PostWarSummary)

PostWarPriv <- Tenure %>% 
  filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>%
  group_by(Country) %>% 
  select(c("Country", "Date", "PrivPct")) %>%
  pivot_wider(names_from = Country, values_from = PrivPct) %>%
  mutate(BritDef = Netherlands / Unitedkingdom) %>% 
  ggplot(aes(x = Date, y = BritDef)) + geom_line(size = 1.2, color = "orange") +
  geom_hline(yintercept = 0) + geom_vline(xintercept = as.POSIXct(as.Date("1955-01-01"))) + labs(title = "Dutch Private Housebuilding Rates as a Multiple of Britain's")

PostWarPriv 

view(PostWarPriv)

Demolitiongraph <- Tenure %>% 
                   mutate(DemPct = ImpDem * 100 / Esthaus) %>% 
                   filter(Country == "Unitedkingdom" | Country == "Netherlands") %>%
                   ggplot(aes(x = Date, y = DemPct, color = Country)) + geom_point()

Demolitiongraph 
