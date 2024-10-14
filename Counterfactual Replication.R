setwd("C:/Users/samue/Documents/Processed European Data")

library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(zoo)
library(xts)
library(ggpattern) 

setwd("C:/Users/samue/Documents/Processed European Data") 

options(scipen = 999)  

library(tidyverse)
library(lubridate)
library(rlang)
library(zoo)
library(xts)
library(ggpattern) 

Data <- read_csv("Combined.csv") %>% 
  select(-c("...1")) %>% 
  mutate(Date = as.Date(dmy(Date))) %>% 
  filter(Country != "Greece", Country != "Italy", Country != "Spain", Country != "Portugal") %>% 
  group_by(Country) %>% 
  mutate(Popgrowth = X50 - lag(X50), PctPop = (Popgrowth / X50) + 1, PctPop2 = Popgrowth*100 / X50) %>% 
  mutate(PctPop = na.approx(PctPop, maxgap = 2, na.rm = FALSE), PctPop2 = na.approx(PctPop2, maxgap = 2, na.rm = FALSE)) %>%
  filter(Date > as.Date("1954-01-01") & Date < as.Date("2016-01-01")) %>%
  mutate(CumuPop = cumprod(PctPop)) %>%  
  mutate(Private = `4` * Y7, Public = `1` * Y7) %>% 
  mutate(PrivRate = 1 + (Private / `X5 (Estimate)`), PubRate = 1 + (Public / `X5 (Estimate)`), TotRate = PubRate + PrivRate - 1) %>% 
  mutate(Priv = Private / `X5 (Estimate)`, Pub = Public / `X5 (Estimate)`) %>% 
  ungroup() 

view(Data) 

Counterfac <- Data %>% 
  filter(Country == "Unitedkingdom") %>% 
  mutate(Britpop = first(X50)) %>% 
  mutate(Britstock = first(`X5 (Estimate)`)) %>% 
  mutate(BritPerCap = first(PerCapHouse)) %>% 
  rename("BritCumu" = "CumuPop") %>% 
  rename("ContempBritPop" = "X50") %>%
  mutate(DemRatio = Y2 / Y7) %>% 
  mutate(DemRatio = na.approx(DemRatio, maxgap = 2, na.rm = FALSE)) %>% 
  fill(DemRatio) %>% 
  mutate(NewDem = DemRatio * Y7) %>% 
  mutate(BritDem = coalesce(Y2, NewDem)) %>% 
  mutate(DemRate = BritDem / `X5 (Estimate)`) %>% 
  select(c("Date", "DemRate", "Britpop", "ContempBritPop", "Britstock", "BritPerCap", "DemRatio", "BritCumu")) %>%
  left_join(Data, by = c("Date" = "Date")) %>% 
  group_by(Country) %>% 
  mutate(BasePerCap = first(`PerCapHouse`)) %>% 
  mutate(EuroBaseStock = Britpop * BasePerCap) %>% 
  mutate(PopRatio = CumuPop / BritCumu) %>% 
  mutate(EuroPopStock = EuroBaseStock / PopRatio) %>% 
  mutate(NetRate = TotRate - DemRate) %>%
  mutate(Cumustock = cumprod(NetRate)) %>% 
  mutate(Counterstock = EuroPopStock * Cumustock) %>% 
  mutate(Privbuild = Counterstock * Priv, Pubbuild = Counterstock * Pub) %>% 
  mutate(Cumupriv = cumsum(Privbuild), Cumupub = cumsum(Pubbuild)) %>% 
  arrange(Country)

view(Counterfac) 

Swiss <- Counterfac %>% 
  filter(Country %in% c("Switzerland")) %>% 
  group_by(Country) %>%
  mutate(NewDemRate = na.approx(Y2, maxgap = 2, na.rm = FALSE)) %>% 
  mutate(SwiDemRatio = NewDemRate / Y7) %>% 
  fill(SwiDemRatio, .direction = "downup") %>% 
  mutate(NewDemRatio = SwiDemRatio * Y7) %>% 
  mutate(NewDemRate  = coalesce(NewDemRate, NewDemRatio)) %>%
  mutate(NewDemRate = NewDemRate / `X5 (Estimate)`) %>% 
  select(-c("SwiDemRatio", "NewDemRatio"))

view(Swiss) 

SwiBrit <- Counterfac %>% 
  filter(Country == "Unitedkingdom") %>% 
  ungroup() %>%
  select(c("Date", "Counterstock")) %>% 
  rename("BritAlt" = "Counterstock") %>%
  full_join(Swiss, by = c("Date" = "Date")) %>% 
  mutate(StockRatio = Counterstock / BritAlt) 

view(SwiBrit)

Counterfac3 <- Counterfac %>% 
  filter(Country == "Unitedkingdom") %>% 
  ungroup() %>%
  select(c("Date", "Counterstock")) %>% 
  rename("BritAlt" = "Counterstock") %>%
  full_join(Counterfac, by = c("Date" = "Date")) %>% 
  filter(Country != "Switzerland" ) %>%
  mutate(StockRatio = Counterstock / BritAlt) %>% 
  mutate(NewDemRate = DemRate + ((StockRatio - 1) / 100)) %>% 
  rbind(SwiBrit) %>%
  mutate(NewTotRate = TotRate - NewDemRate) %>% 
  mutate(PrivDem = NewDemRate*`4`, PubDem = NewDemRate* `1`) %>%
  group_by(Country) %>%
  mutate(NewCumustock = cumprod(NewTotRate)) %>% 
  mutate(NewCounterstock = EuroPopStock * NewCumustock) %>% 
  mutate(NewPrivbuild = NewCounterstock * Priv, NewPubbuild = NewCounterstock * Pub) %>% 
  mutate(NewCumupriv = cumsum(NewPrivbuild), NewCumupub = cumsum(NewPubbuild)) %>% 
  mutate(NewPrivdem = NewCounterstock*PrivDem, NewPubdem = NewCounterstock*PubDem) %>%
  mutate(CumuPrivdem = cumsum(NewPrivdem), CumuPubdem = cumsum(NewPubdem)) %>%
  arrange(Country) 

View(Counterfac3) 

Table3 <- Counterfac3 %>% 
  filter(Date == as.Date("2015-01-01")) %>% 
  mutate(NewCumupriv = NewCumupriv - CumuPrivdem, NewCumupub = NewCumupub - CumuPubdem) %>%
  select(c("Country", "NewCumupriv", "NewCumupub", "NewCounterstock")) %>% 
  mutate(NewCumuTot = NewCumupriv + NewCumupub) %>%
  pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
  pivot_wider(names_from = Country, values_from = Value) %>% 
  mutate(across(`Austria`:`Switzerland`, ~ .x - Unitedkingdom)) %>% 
  pivot_longer(-c("Item", "Unitedkingdom"), names_to = "Country", values_to = "Value") %>% 
  group_by(Item) %>% 
  mutate(Mean = mean(Value)) 

view(Table3) 

BritRef3 <- Table3 %>%
  select(c("Item", "Unitedkingdom")) %>% 
  distinct() %>%
  pivot_wider(names_from = Item, values_from = Unitedkingdom) %>%
  mutate(Country = "United Kingdom") %>% 
  mutate(PrivRatio = NewCumupriv / NewCumuTot, PubRatio = NewCumupub / NewCumuTot) %>%
  mutate(Cumuinc = NewCounterstock - 15418) %>% 
  mutate(CumuDem = NewCumuTot - Cumuinc) %>% 
  mutate(CumuDemPriv = CumuDem*PrivRatio, CumuDemPub = CumuDem*PubRatio) %>% 
  mutate(AdjCumuPriv = NewCumupriv - CumuDemPriv, AdjCumuPub = NewCumupub - CumuDemPub) %>% 
  select(c("Country", "AdjCumuPriv", "AdjCumuPub", "Cumuinc", "PrivRatio", "PubRatio")) %>% 
  rename("NewCounterstock" = "Cumuinc")

view(BritRef3)

EurTab3 <- Table3 %>% 
  select(-c("Unitedkingdom")) %>% 
  pivot_wider(names_from = Country, values_from = Value) %>% 
  pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>% 
  pivot_wider(names_from = Item, values_from = Value) %>% 
  mutate(DiffNum = NewCounterstock - NewCumuTot) %>% 
  mutate(Country = gsub("Mean", "Western European Average", Country))

view(EurTab3) 

SumEurTab3 <- Counterfac3 %>%  
  filter(Date == as.Date("2015-01-01")) %>%
  select(c("Country", "NewCumupriv", "NewCumupub", "NewCounterstock")) %>% 
  mutate(NewCumuTot = NewCumupriv + NewCumupub) %>%
  pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
  pivot_wider(names_from = Country, values_from = Value) %>% 
  select(-c("Unitedkingdom")) %>% 
  pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>% 
  pivot_wider(names_from = Item, values_from = Value) %>% 
  rename_with(~ tolower(gsub("$", "_Tot", .x))) %>%
  left_join(EurTab3, by = c("country_tot" = "Country")) 

view(SumEurTab3) 

NewTot3 <- SumEurTab3 %>%
  mutate(PrivRatio = newcumupriv_tot / newcumutot_tot, PubRatio = newcumupub_tot / newcumutot_tot) %>% 
  mutate(Privdiff = PrivRatio * DiffNum, Pubdiff = PubRatio * DiffNum) %>% 
  mutate(AdjCumuPriv = NewCumupriv + Privdiff, AdjCumuPub = NewCumupub + Pubdiff) %>% 
  rename("Country" = "country_tot") %>% 
  select(c("Country", "PrivRatio", "PubRatio", "AdjCumuPriv", "AdjCumuPub", "NewCounterstock")) %>% 
  pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
  group_by(Item) %>% 
  mutate(`Western European Average` = mean(Value)) %>% 
  pivot_wider(names_from = Country, values_from = Value) %>%
  pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>%
  pivot_wider(names_from = Item, values_from = Value) %>% 
  arrange(NewCounterstock) 

view(NewTot3)

WestEurope3 <- NewTot3 %>% 
  filter(Country == "Western European Average")

view(WestEurope3)
view(BritRef3)

Export3 <- BritRef3 %>% 
  rbind(NewTot3) %>% 
  filter(Country != "Western European Average") %>% 
  rbind(WestEurope3) %>% 
  mutate(across(`AdjCumuPriv`:`NewCounterstock`, ~ .x * 1000)) %>% 
  mutate(across(`AdjCumuPriv`:`NewCounterstock`, ~ signif(.x, digits = 4))) %>% 
  mutate(across(`PrivRatio`:`PubRatio`, ~ .x * 100)) %>% 
  mutate(across(`PrivRatio`:`PubRatio`, ~ round(.x, digits = 0))) %>% 
  unite("Private : Public Percentage of Additions", PrivRatio:PubRatio, sep = " : ") %>%
  rename("Calculated Private Additions" = "AdjCumuPriv", "Calculated Public Additions" = "AdjCumuPub", 
         "Calculated Total Additions" = "NewCounterstock") 

view(Export3)
write.csv(Export3, "Export3.csv") 


