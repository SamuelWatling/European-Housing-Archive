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

Table <- Counterfac %>% 
  filter(Date == as.Date("2015-01-01")) %>%
  select(c("Country", "Cumupriv", "Cumupub", "Counterstock")) %>% 
  mutate(CumuTot = Cumupriv + Cumupub) %>%
  pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
  pivot_wider(names_from = Country, values_from = Value) %>% 
  mutate(across(`Austria`:`Switzerland`, ~ .x - Unitedkingdom)) %>% 
  pivot_longer(-c("Item", "Unitedkingdom"), names_to = "Country", values_to = "Value") %>% 
  group_by(Item) %>% 
  mutate(Mean = mean(Value))
  
view(Table) 

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

Counterfac2 <- Counterfac %>% 
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
  group_by(Country) %>%
  mutate(NewCumustock = cumprod(NewTotRate)) %>% 
  mutate(NewCounterstock = EuroPopStock * NewCumustock) %>% 
  mutate(NewPrivbuild = NewCounterstock * Priv, NewPubbuild = NewCounterstock * Pub) %>% 
  mutate(NewCumupriv = cumsum(NewPrivbuild), NewCumupub = cumsum(NewPubbuild)) %>% 
  arrange(Country)
               
view(Counterfac2)

Table2 <- Counterfac2 %>% 
         filter(Date == as.Date("2015-01-01")) %>%
         select(c("Country", "NewCumupriv", "NewCumupub", "NewCounterstock")) %>% 
         mutate(NewCumuTot = NewCumupriv + NewCumupub) %>%
         pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
         pivot_wider(names_from = Country, values_from = Value) %>% 
         mutate(across(`Austria`:`Switzerland`, ~ .x - Unitedkingdom)) %>% 
         pivot_longer(-c("Item", "Unitedkingdom"), names_to = "Country", values_to = "Value") %>% 
         group_by(Item) %>% 
         mutate(Mean = mean(Value)) 
         
view(Table2) 

BritRef <- Table2 %>%
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

view(BritRef) 

EurTab2 <- Table2 %>% 
           select(-c("Unitedkingdom")) %>% 
           pivot_wider(names_from = Country, values_from = Value) %>% 
           pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>% 
           pivot_wider(names_from = Item, values_from = Value) %>% 
           mutate(DiffNum = NewCounterstock - NewCumuTot) %>% 
           mutate(Country = gsub("Mean", "Western European Average", Country))

view(EurTab2) 

SumEurTab2 <- Counterfac2 %>%  
  filter(Date == as.Date("2015-01-01")) %>%
  select(c("Country", "NewCumupriv", "NewCumupub", "NewCounterstock")) %>% 
  mutate(NewCumuTot = NewCumupriv + NewCumupub) %>%
  pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
  pivot_wider(names_from = Country, values_from = Value) %>% 
  select(-c("Unitedkingdom")) %>% 
  pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>% 
  pivot_wider(names_from = Item, values_from = Value) %>% 
  rename_with(~ tolower(gsub("$", "_Tot", .x))) %>%
  left_join(EurTab2, by = c("country_tot" = "Country")) 

view(SumEurTab2) 

NewTot <- SumEurTab2 %>%
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

view(NewTot)

WestEurope <- NewTot %>% 
              filter(Country == "Western European Average")

view(WestEurope)
view(BritRef)

Export <- BritRef %>% 
          rbind(NewTot) %>% 
          filter(Country != "Western European Average") %>% 
          rbind(WestEurope) %>% 
          mutate(PctStock = NewCounterstock / 27651.02) %>%
          mutate(across(`AdjCumuPriv`:`NewCounterstock`, ~ .x * 1000)) %>% 
          mutate(across(`AdjCumuPriv`:`NewCounterstock`, ~ signif(.x, digits = 4))) %>% 
          mutate(across(`PrivRatio`:`PctStock`, ~ .x * 100)) %>% 
          mutate(across(`PrivRatio`:`PubRatio`, ~ round(.x, digits = 0))) %>% 
          mutate(PctStock = round(PctStock, digits = 1)) %>%
          unite("Private : Public Percentage of Additions", PrivRatio:PubRatio, sep = " : ") %>%
          rename("Calculated Private Additions" = "AdjCumuPriv", "Calculated Public Additions" = "AdjCumuPub", 
                 "Calculated Total Additions" = "NewCounterstock") 
                 
view(Export) 

setwd("C:/Users/S.Watling/Documents/Counterfactuals") 

write.csv(Export, "StockAdjustedCounterfac2.csv") 

PerCap1980 <- Counterfac2 %>% 
          filter(Date == as.Date("1980-01-01")) %>% 
          mutate(CountPerCap = NewCounterstock / ContempBritPop) %>% 
          select(c("Country", "CountPerCap", "PerCapHouse")) %>% 
          rename("1980 Counterfactual" = "CountPerCap", "1980 Actual" = "PerCapHouse")

view(PerCap1980) 

PerCap2015 <- Counterfac2 %>%  
              filter(Date == as.Date("2015-01-01")) %>% 
              mutate(CountPerCap = NewCounterstock / ContempBritPop) %>% 
              select(c("Country", "CountPerCap", "PerCapHouse")) %>%
              rename("2015 Counterfactual" = "CountPerCap", "2015 Actual" = "PerCapHouse") 

view(PerCap2015) 

AllPerCap <- Counterfac2 %>% 
             filter(Date == as.Date("1955-01-01")) %>% 
             select(c("Country", "PerCapHouse")) %>% 
             rename("1955 Base Stock"= "PerCapHouse") %>%
             full_join(PerCap1980, by = c("Country" = "Country")) %>% 
             full_join(PerCap2015, by = c("Country" = "Country"))  
             
  
WestPerCap <- AllPerCap %>% 
              filter(Country != "Unitedkingdom") %>%
              pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
              group_by(Item) %>% 
              mutate(`Western European Average` = mean(Value)) %>% 
              pivot_wider(names_from = Country, values_from = Value) %>% 
              pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>% 
              pivot_wider(names_from = Item, values_from = Value)
              
view(WestPerCap)

Western <- WestPerCap %>% 
           filter(Country == "Western European Average") 

view(Western) 

Europe <- WestPerCap %>% 
          filter(Country != "Western European Average") %>% 
          arrange(`2015 Counterfactual`) 

view(Europe) 

EuroSummary <- AllPerCap %>% 
               filter(Country == "Unitedkingdom") %>% 
               rbind(Europe) %>% 
               rbind(Western) %>% 
               mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>% 
               mutate(across(`1955 Base Stock`:`2015 Actual`, ~ round(.x, digits = 1)))

view(EuroSummary)  

Export2 <- EuroSummary %>% 
           select(c("Country", "1955 Base Stock", "2015 Counterfactual")) %>% 
           unite("1955 Base Stock : 2015 CounterFactual Housing Stock", `1955 Base Stock`:`2015 Counterfactual`, sep = " : ") %>% 
           full_join(Export, by = c("Country" = "Country"))  
           
view(Export2)

setwd("C:/Users/S.Watling/Documents/Counterfactuals") 

write.csv(Export2, "Export2.csv")
write.csv(EuroSummary, "EuroSummary.csv") 

Control <- Counterfac2 %>% 
           filter(Date > as.Date("1998-01-01") & Date <= as.Date("2015-01-01")) %>%  
           group_by(Country) %>% 
           mutate(StockChange = NewCounterstock - lag(NewCounterstock)) %>% 
           mutate(StockChangePct = (StockChange / NewCounterstock) + 1) %>% 
           filter(Date != as.Date("1999-01-01")) %>%
           summarise(PctBuild = mean(NewTotRate), PctInc = mean(StockChangePct)) 

view(Control) 

ControlAvg <- Control %>% 
              filter(Country != "Unitedkingdom") %>%
              summarise(PctBuild = mean(PctBuild), PctInc = mean(PctInc)) %>% 
              mutate(Country = "Western European Average") %>% 
              rbind(Control) %>% 
              relocate(Country, .before = PctBuild)

view(ControlAvg)

ControlBrit <- ControlAvg %>% 
               filter(Country == "Unitedkingdom") %>% 
               select(-c("Country")) %>% 
               mutate(Join = "J")

view(ControlBrit)

Control2 <- ControlAvg %>% 
            filter(Country != "Unitedkingdom") %>% 
            mutate(Join = "J") %>%
            full_join(ControlBrit, by = c("Join" = "Join")) %>% 
            select(-c("Join")) %>% 
            rename("EuroGross" = "PctBuild.x", "EuroNet" = "PctInc.x", "BritGross" = "PctBuild.y", "BritNet" = "PctInc.y") %>% 
            mutate(GrossAdj = (EuroGross), NetAdj = (EuroNet)) %>% 
            select(c("Country", "GrossAdj", "NetAdj"))
            
view(Control2)

JoinedSummary <- EuroSummary %>% 
                 select(c("Country", "2015 Counterfactual")) %>% 
                 rename("2015 Per Capita Stock" = "2015 Counterfactual") %>%
                 full_join(Export, by = c("Country" = "Country")) %>% 
                 filter(Country != "United Kingdom") %>% 
                 full_join(Control2, by = c("Country" = "Country")) %>% 
                 mutate(`English Total Additions` = `Calculated Total Additions` * 0.833 , EnglishStock = 28277656 * 0.833)
                        
view(JoinedSummary)

linear <-        JoinedSummary %>% 
                 mutate(TotalClear10 = 300000*10 + `English Total Additions`, TotalClear25 = 300000*25 + `English Total Additions`) %>%
                 mutate(Backlog10 = 300000 + `English Total Additions` / 10) %>% 
                 mutate(Backlog25 = 300000 + `English Total Additions` / 25) %>% 
                 mutate(InitialRate10 = Backlog10 / EnglishStock, InitialRate25 = Backlog25 / EnglishStock) %>% 
                 mutate(FinalRate10 = Backlog10 / (EnglishStock + TotalClear10 - Backlog10), FinalRate25 = Backlog25 / (EnglishStock + TotalClear25 - Backlog25)) %>%
                 mutate(Grosslog10 = EnglishStock * (GrossAdj - 1) + `English Total Additions` / 10) %>% 
                 mutate(Grosslog25 = EnglishStock * (GrossAdj - 1) + `English Total Additions` / 25) %>% 
                 mutate(Netlog10 = EnglishStock * (NetAdj - 1) + `English Total Additions` / 10) %>% 
                 mutate(Netlog25 = EnglishStock * (NetAdj - 1) + `English Total Additions` / 25)
                 
view(linear) 

linearsum <- linear %>% 
             select(c("Country", "Backlog10","InitialRate10", "FinalRate10", "Backlog25", "InitialRate25", "FinalRate25")) %>% 
             mutate(InitialRate10 = InitialRate10*100, FinalRate10 = FinalRate10*100, InitialRate25 = InitialRate25*100, FinalRate25 = FinalRate25*100) %>% 
             mutate(across(where(is.numeric), ~ signif(.x, digits = 3))) %>% 
             unite("Initial and Final Building Rates Over 10 Years", InitialRate10:FinalRate10, sep = " - ") %>%  
             unite("Initial and Final Building Rates Over 25 Years", InitialRate25:FinalRate25, sep = " - ") %>% 
             rename("300000 + Backlog in 10 Years" = "Backlog10", "300000 + Backlog in 25 Years" = "Backlog25")
  
view(linearsum) 

write.csv(linearsum, "linearsum.csv")

Compounding1 <- JoinedSummary %>% 
               mutate(ReqTot10 = EnglishStock + 10*300000 + `English Total Additions`, ReqRat10 = ReqTot10 / EnglishStock,
                      ReqTot25 = EnglishStock + 25*300000 + `English Total Additions`, ReqRat25 = ReqTot25 / EnglishStock)  %>% 
               mutate(Rate10 = (ReqRat10)^(1/10), Rate25 = (ReqRat25)^(1/25)) %>%
               mutate(Initial10 = (Rate10 - 1) * EnglishStock, Initial25 = (Rate25 - 1)*EnglishStock) %>% 
               mutate(FinalRate10 = ReqRat10 - Rate10^9, FinalRate25 = ReqRat25 - Rate25^24) %>% 
               mutate(Final10 = FinalRate10* EnglishStock, Final25 = FinalRate25* EnglishStock) 
  
view(Compounding1)

SuggestRates <- Compounding1 %>% 
                filter(Country != "Unitedkingdom") %>% 
                select(c("Country", "Initial10", "Final10","Rate10", "Initial25", "Final25", "Rate25")) %>% 
                mutate(Rate10 = (Rate10 - 1)*100, Rate25 = (Rate25 - 1)*100) %>%
                mutate(across(where(is.numeric), ~ signif(.x, digits = 3))) %>% 
                rename("300000 + Backlog in 10 Years, Initial Rate"= "Initial10", "300000 + Backlog in 25 Years, Initial Rate" = "Initial25", 
                       "10 Year Rate Required" = "Rate10", 
                       "300000 + Backlog in 10 Years, Final Rate" = "Final10", "300000 + Backlog in 25 Years, Final Rate" = "Final25", 
                       "25 Year Rate Required" = "Rate25")

view(SuggestRates) 

setwd("C:/Users/S.Watling/Documents/Counterfactuals") 
write.csv(SuggestRates, "Building Targets1.csv") 

view(JoinedSummary)

CompoundingNet <- JoinedSummary %>% 
                  mutate(CumuNet10 = NetAdj^10, CumuNet25 = NetAdj^25) %>% 
                  mutate(FinNet10 = CumuNet10 * EnglishStock  + `English Total Additions`, 
                         FinNet25 = CumuNet25 * EnglishStock  + `English Total Additions`) %>% 
                  mutate(ReqRate10 = FinNet10 / EnglishStock, ReqRate25 = FinNet25 / EnglishStock) %>%
                  mutate(Rate10 = ReqRate10^(1/10) , Rate25 = ReqRate25^(1/25)) %>% 
                  mutate(InitialBuild10 = (Rate10 - 1)* EnglishStock, InitialBuild25 = (Rate25 - 1)* EnglishStock) %>%
                  mutate(FinBuild10 = (ReqRate10 - Rate10^9)* EnglishStock, FinBuild25 = (ReqRate25 - Rate25^24)* EnglishStock)
                  
view(CompoundingNet)

NetSummary <- CompoundingNet %>% 
              select(c("Country",  "InitialBuild10", "FinBuild10","Rate10", "InitialBuild25", "FinBuild25", "Rate25")) %>% 
              mutate(Rate10 = (Rate10 - 1)*100, Rate25 = (Rate25 - 1)*100) %>% 
              mutate(across(where(is.numeric), ~ signif(.x, digits = 3))) %>% 
              rename("Initial Build Rate to Converge in 10 Years" = "InitialBuild10", 
                     "Final Build Rate to Converge in 10 Years" = "FinBuild10", 
                     "Required Rate of Per Capita Stock Growth for 10 Year Convergence" = "Rate10", 
                     "Initial Build Rate to Converge in 25 Years" = "InitialBuild25", 
                     "Final Build Rate to Converge in 25 Years"= "FinBuild25", 
                     "Required Rate of Per Capita Stock Growth for 25 Year Convergence" = "Rate25")
              
view(NetSummary) 

Years <- JoinedSummary %>% 
         mutate(Target = EnglishStock + `English Total Additions`) %>% 
         mutate(EngRate1 = (240000 / EnglishStock) + 1, EngRate2 = (300000 / EnglishStock) + 1) %>%
         mutate(LogTarg = log(Target), LogEngHouse = log(EnglishStock), LogEuro = log(NetAdj), LogBrit1 = log(EngRate1), LogBrit2 = log(EngRate2)) %>% 
         mutate(Year1 = LogTarg / (LogEngHouse + LogBrit1 - LogEuro))
         
view(Years)

write.csv(NetSummary, "NetSummaryConvergence.csv") 

NetMin <- JoinedSummary %>% 
  mutate(InitialImp = (NetAdj - 1)* EnglishStock) %>%
  mutate(CumuNet10 = NetAdj^10, CumuNet25 = NetAdj^25) %>% 
  mutate(Fin10 = CumuNet10 * EnglishStock, Surp10 = Fin10 - EnglishStock, Rate10 = Surp10 / 10) %>% 
  mutate(Fin25 = CumuNet25 * EnglishStock, Surp25 = Fin25 - EnglishStock, Rate25 = Surp25 / 25)  
  
view(NetMin) 

NetMinSum <- NetMin %>% 
             select(c("Country","InitialImp" , "Rate10", "Rate25")) %>% 
             mutate(across(InitialImp:Rate25, ~ signif(.x, digits = 3))) %>% 
             rename("Compounding Rate" = "InitialImp", "Linear Build for 10 Years Compounding" = "Rate10" , "Linear Build for 25 Years Compounding" = "Rate25")

view(NetMinSum) 
write.csv(NetMinSum, "NetMinSum.csv")

Periodisation <- Counterfac2 %>% 
                 filter(Date == as.Date("1979-01-01") | Date == as.Date("2015-01-01")) %>% 
                 select(c("Country", "Date", "NewCounterstock", "NewCumupriv", "NewCumupub")) 
                 
view(Periodisation) 

PeriodBritRef <- Periodisation %>% 
                 filter(Country == "Unitedkingdom") %>% 
                 ungroup() %>%
                 select(-c("Country")) %>% 
                 rename("UK Counterstock" = "NewCounterstock", "UK Private Total" = "NewCumupriv", "UK Public Total" = "NewCumupub")

view(PeriodBritRef)
    
PeriodEuro <- Counterfac2 %>% 
              filter(Country != "Unitedkingdom" & Date == as.Date("1955-01-01")) %>% 
              select(c("Country", "Date", "Britstock", "EuroBaseStock")) %>% 
              rename("UK Counterstock" = "Britstock", "NewCounterstock" = "EuroBaseStock") %>% 
              mutate(NewCumupriv = NA, NewCumupub = NA) %>%
              rbind(Periodisation) %>%
              full_join(PeriodBritRef, by = c("Date" = "Date")) %>% 
              mutate(`UK Counterstock` = coalesce(`UK Counterstock.x`, `UK Counterstock.y`)) %>%
              select(-c("UK Counterstock.x", "UK Counterstock.y")) %>% 
              relocate(`UK Counterstock`, .before = NewCounterstock) %>% 
              arrange(Country) %>% 
              mutate(TotBuild = NewCumupriv + NewCumupub, UKTotBuild = `UK Private Total` + `UK Public Total`) %>% 
              mutate(StockDiff = NewCounterstock - `UK Counterstock` , PrivDiff = NewCumupriv - `UK Private Total`,  
                     PubDiff = NewCumupub - `UK Public Total`, TotDiff = TotBuild - UKTotBuild) %>% 
              mutate(PrivBuildRatio = NewCumupriv / TotBuild, PubBuildRatio = NewCumupub / TotBuild) %>% 
              group_by(Country) %>% 
              mutate(StockChange = NewCounterstock - lag(NewCounterstock), UKStockChange = `UK Counterstock` - lag(`UK Counterstock`)) %>% 
              filter(Country != "Unitedkingdom")  

view(PeriodEuro)

MainSummary <- PeriodEuro %>% 
               mutate(Adjustment = StockDiff - TotDiff) %>% 
               mutate(PrivAdj = Adjustment * PrivBuildRatio, PubAdj = Adjustment * PubBuildRatio) %>% 
               mutate(AdjPrivUK = `UK Private Total` - PrivAdj, AdjPubUK = `UK Public Total` - PubAdj) %>%
               mutate(PrivSurp = NewCumupriv - AdjPrivUK, PubSurp = NewCumupub - AdjPubUK, TotSurp = PrivSurp + PubSurp)  

view(MainSummary)

PeriodEuroAlt <-   PeriodEuro %>% mutate(NewStockDiff = StockChange - UKStockChange) %>% 
              mutate(AdjValueAlt = TotDiff - NewStockDiff) %>% 
              mutate(PrivAdjAlt = AdjValueAlt * PrivBuildRatio, PubAdjAlt = AdjValueAlt * PubBuildRatio) %>% 
              mutate(AdjAltUKPriv = `UK Private Total` + PrivAdjAlt, AdjAltUKPub = `UK Public Total` + PubAdjAlt) %>% 
              mutate(AltPrivSurp = NewCumupriv - AdjAltUKPriv, AltPubSurp = NewCumupub - AdjAltUKPub, AltTotSurp = AltPrivSurp + AltPubSurp)
              
view(PeriodEuro) 

SummariedMain <- MainSummary %>% 
  filter(Date != as.Date("1955-01-01")) %>% 
  select(c("Country", "Date", "PrivSurp", "PubSurp", "TotSurp")) %>% 
  mutate(across(where(is.numeric), ~ signif(.x, digits = 3))) %>% 
  mutate(across(where(is.numeric), ~ .x * 1000)) %>% 
  pivot_longer(-c("Country", "Date"), names_to = "Item", values_to = "Value") %>% 
  pivot_wider(names_from = Date, values_from = Value) %>% 
  rename("1955 - 1979" = "1979-01-01", "1955 - 2015" = "2015-01-01") %>% 
  mutate(`1980 - 2015` =  `1955 - 2015` - `1955 - 1979`) %>% 
  relocate(`1955 - 2015`, .after = `1980 - 2015`) %>% 
  mutate(PctPostWar = `1955 - 1979`*100 / `1955 - 2015`) %>%
  pivot_longer(-c("Country", "Item"), names_to = "Period", values_to = "Value") %>% 
  unite("New", Item:Period) %>% 
  filter(New != "PrivSurp_PctPostWar" & New != "PubSurp_PctPostWar") %>% 
  separate(New, c("Item", "Period"), sep = "_") %>% 
  mutate(Value = signif(Value, digits = 3)) %>% 
  pivot_wider(names_from = Item, values_from = Value) %>% 
  rename("Private Surplus" = "PrivSurp", "Public Surplus" = "PubSurp", "Total Surplus" = "TotSurp") 

view(SummariedMain) 

setwd("C:/Users/S.Watling/Documents/Counterfactuals")

write.csv(SummariedMain, "MainSummaryPeriod.csv")             
  
AltSummary <- PeriodEuroAlt %>% 
              filter(Date != as.Date("1955-01-01")) %>% 
              select(c("Country", "Date", "AltPrivSurp", "AltPubSurp", "AltTotSurp")) %>% 
              mutate(across(where(is.numeric), ~ signif(.x, digits = 3))) %>% 
              mutate(across(where(is.numeric), ~ .x * 1000)) %>% 
              pivot_longer(-c("Country", "Date"), names_to = "Item", values_to = "Value") %>% 
              pivot_wider(names_from = Date, values_from = Value) %>% 
              rename("1955 - 1979" = "1979-01-01", "1955 - 2015" = "2015-01-01") %>% 
              mutate(`1980 - 2015` =  `1955 - 2015` - `1955 - 1979`) %>% 
              relocate(`1955 - 2015`, .after = `1980 - 2015`) %>% 
              mutate(PctPostWar = `1955 - 1979`*100 / `1955 - 2015`) %>%
              pivot_longer(-c("Country", "Item"), names_to = "Period", values_to = "Value") %>% 
              unite("New", Item:Period) %>% 
              filter(New != "AltPrivSurp_PctPostWar" & New != "AltPubSurp_PctPostWar") %>% 
              separate(New, c("Item", "Period"), sep = "_") %>% 
              mutate(Value = signif(Value, digits = 3)) %>% 
              pivot_wider(names_from = Item, values_from = Value) %>% 
              rename("Private Surplus" = "AltPrivSurp", "Public Surplus" = "AltPubSurp", "Total Surplus" = "AltTotSurp") 

view(AltSummary) 

setwd("C:/Users/S.Watling/Documents/Counterfactuals")

write.csv(AltSummary, "AltSummaryPeriod.csv") 

view(Counterfac2)                 

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
  rbind(NewTot) %>% 
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

BritNeth <- Counterfac3 %>% 
            filter(Country == "Netherlands" & Date == as.Date("2015-01-01")) %>%
            mutate(TotCumu = Cumupriv + Cumupub, TotDem = CumuPrivdem + CumuPubdem, 
                   CumuNet = TotCumu - TotDem, NetAdd = NewCounterstock - EuroBaseStock, 
                   PrivPreDem = Cumupriv + CumuPrivdem, PrePubDem = + Cumupub + CumuPubdem, TotPreDem = TotCumu + TotDem) %>% 
            select(c(""))

view(BritNeth)

view(Counterfac3)

Bloomberg <- Counterfac3 %>% 
             mutate(NewCumupriv = NewCumupriv - CumuPrivdem, NewCumupub = NewCumupub - CumuPubdem) %>%
             select(c("Country","Date", "NewCumupriv", "NewCumupub", "NewCounterstock")) %>% 
             mutate(NewCumuTot = NewCumupriv + NewCumupub) %>% 
             mutate(PrivRatio = )


view(Bloomberg)

BritRefBloom <- Bloomberg %>% 
                filter(Country == "Unitedkingdom") %>% 
  mutate(Cumuinc = NewCounterstock - 15418) %>% 
  mutate(CumuDem = NewCumuTot - Cumuinc) %>% 
  mutate(CumuDemPriv = CumuDem*PrivRatio, CumuDemPub = CumuDem*PubRatio) %>% 
  mutate(AdjCumuPriv = NewCumupriv - CumuDemPriv, AdjCumuPub = NewCumupub - CumuDemPub) 
  
view(BritRefBloom)

tempdir()
