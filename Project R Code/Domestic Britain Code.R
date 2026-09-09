setwd("C:/Users/S.Watling/Documents/Historical Housing Statistics")

library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)
library(xts)

Households <- read_csv("Households.csv") %>% 
              row_to_names(row_number = 3) %>% 
              select(c("Year", "Houses")) %>%
              slice(c(1:70)) %>%
              mutate(Year = as.numeric(Year)) %>%
              mutate(Houses = gsub(",", "", Houses)) %>%
              mutate(Houses = as.numeric(Houses)) %>% 
              filter(Houses > 0) 
 
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
        select(c("Year":"Local Authorities")) %>%
        slice(54:72) %>%
        mutate_all(~ gsub("[^0-9]+", "", .)) %>%
        rename("Total" = "All Dwellings", "Private unassisted" = "Private Enterprise", 
               "Housing associations" = "Housing Associations", "Public Housing" = "Local Authorities") %>%
        mutate(`Private unassisted` = as.numeric(`Private unassisted`), `Private unassisted` = `Private unassisted` / 1000, `Private unassisted` = round(`Private unassisted`, digits = 1)) %>%
        mutate(Total = as.numeric(Total), Total = Total / 1000, Total = round(Total, digits = 1)) %>% 
        mutate(`Housing associations` = as.numeric(`Housing associations`), `Housing associations` = `Housing associations` / 1000, `Housing associations` = round(`Housing associations`, digits = 2)) %>%
        mutate(`Public Housing` = as.numeric(`Public Housing`), `Public Housing` =  `Public Housing` / 1000 , `Public Housing` = round(`Public Housing`, digits = 2)) %>% 
        mutate(`Private assisted` = 0, Prefabs = 0, `Government departments` = 0) %>% 
        mutate(`Private unassisted` = as.character(`Private unassisted`), Total = as.character(Total), `Housing associations` = as.character(`Housing associations`), `Public Housing` = as.character(`Public Housing`))
       
view(Ten4) 

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
               slice(9:171) %>%
               mutate(Date = parse_date_time(Year, orders = c("Y", "ymd"))) %>% 
               mutate(Date = ymd(Date)) %>%
               mutate(across(!Date, as.numeric)) %>% 
               mutate(Netinc = Houses - lag(Houses)) %>%
               mutate(Nethouse = Total - Demolitions) %>% 
               mutate(Nethouse = ifelse(Date < as.Date("1954-01-01"), NA, Nethouse)) %>% 
               mutate(CumulNet = cumsum(replace_na(Nethouse, 0))) %>% 
               mutate(CumulNet = ifelse(CumulNet == 0, NA, CumulNet )) %>%
               mutate(Cumuhouse = cumsum(Total)) %>% 
               mutate(Cumuhouse2 = Cumuhouse) %>%
               mutate(Cumuhouse2 = ifelse(Date >  as.Date("1954-01-01"), NA, Cumuhouse2)) %>%
               fill(Cumuhouse2) %>% 
               mutate(Cumubase = Cumuhouse2 + CumulNet) %>%
               mutate(Cumuhouse = coalesce(Cumubase, Cumuhouse2)) %>%
               mutate(BaseHouse = Houses) %>%
               fill(BaseHouse, .direction = "downup") %>%
               mutate(FirstHouse = first(BaseHouse)) %>%
               mutate(AdjHouse = Cumuhouse - 280 ) %>%
               mutate(ImpHouse = FirstHouse + AdjHouse) %>%
               mutate(Housediff = Houses - ImpHouse) %>% 
               mutate(Census1 = ifelse(Date > as.Date("1860-01-01") & Date < as.Date("1871-01-01"), 1, 
                               ifelse(Date > as.Date("1870-01-01") & Date < as.Date("1881-01-01"), 2, 
                               ifelse(Date > as.Date("1880-01-01") & Date < as.Date("1891-01-01"), 3 , 
                               ifelse(Date > as.Date("1890-01-01") & Date < as.Date("1901-01-01"), 4 , 
                               ifelse(Date > as.Date("1900-01-01") & Date < as.Date("1911-01-01"), 5 , 
                               ifelse(Date > as.Date("1910-01-01") & Date < as.Date("1921-01-01"), 6 , 
                               ifelse(Date > as.Date("1920-01-01") & Date < as.Date("1931-01-01"), 7,  
                               ifelse(Date > as.Date("1930-01-01") & Date < as.Date("1940-01-01"), 8, 
                               ifelse(Date > as.Date("1939-01-01") & Date < as.Date("1945-01-01"), 9, 
                               ifelse(Date > as.Date("1944-01-01") & Date < as.Date("1951-01-01"), 10, 
                               ifelse(Date > as.Date("1950-01-01") & Date < as.Date("1961-01-01"), 11,  
                               ifelse(Date > as.Date("1960-01-01") & Date < as.Date("1970-01-01"), 12, NA))))))))))))) %>%
               mutate(Demo2 = Demolitions) %>%
               mutate(Demo2 = ifelse(Date < as.Date("1950-01-01"), NA, Demo2)) %>%
               mutate(Housediff2 = Housediff) %>% 
               fill(Housediff2) %>%
               mutate(ChangeHousediff = Housediff2 - lag(Housediff2)) %>%
               mutate(ChangeHousediff = ifelse(ChangeHousediff == 0, NA, ChangeHousediff)) %>%
               fill(ChangeHousediff, .direction = "up") %>%
               mutate(ChangeHousediff = ifelse(Date < as.Date("1861-01-01"), NA, ChangeHousediff )) %>%
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
               mutate(Houseestimate  = coalesce(Houses, Housingstock)) %>% 
               mutate(Private = (`Private assisted` + `Private unassisted`)*100 / Houseestimate,
               Public = Private + ((`Public Housing` + `Housing associations` + Prefabs)*100) / Houseestimate)
               
view(Total) 

write.csv(Total, "HistoricEnglandandWales.csv")

MeanCalc <- Total %>% 
            mutate(PublicRate = `Public Housing`*100 / Houseestimate) %>% 
            select(c("Date", "PublicRate")) %>% 
            filter(Date > as.Date("1920-01-01") & Date < ("1980-01-01")) %>% 
            mutate(Decade = ifelse(Date > as.Date("1920-01-01") & Date < ("1930-01-01"), "1920s", 
                            ifelse(Date > as.Date("1930-01-01") & Date < ("1940-01-01"), "1930s", 
                            ifelse(Date > as.Date("1945-01-01") & Date < ("1950-01-01"), "1940s", 
                            ifelse(Date > as.Date("1950-01-01") & Date < ("1960-01-01"), "1950s",
                            ifelse(Date > as.Date("1960-01-01") & Date < ("1970-01-01"), "1960s", 
                            ifelse(Date > as.Date("1970-01-01") & Date < ("1980-01-01"), "1970s", 0))))))) %>% 
                  filter(Decade != 0) %>% 
           group_by(Decade) %>% 
           summarise(`Public Housebuilding Rate` = mean(PublicRate))

view(MeanCalc) 

MeanCalc2 <- Total %>% 
             mutate(BuildRate = Total*100 / Houseestimate, PrivRate = (`Private assisted` + `Private unassisted`)*100 /  Houseestimate, 
                    PubRate = (`Public Housing` + `Housing associations`)*100 / Houseestimate) %>% 
             filter(Date < as.Date("1914-04-01") | Date > as.Date("1920-01-01")) %>% 
             filter(Date < as.Date("1939-06-01") | Date > as.Date("1946-01-01")) %>%
             mutate(Period = ifelse(Date < as.Date("1940-01-01"), "Pre-TCPA (1856 - 1939)", 
                             ifelse(Date >  as.Date("1946-01-01"), "Post-TCPA (1947 - 2019) ", NA))) %>% 
             group_by(Period) %>% 
             mutate(`Average Total Build Rate` = mean(BuildRate), `Average Private Build Rate` = mean(PrivRate), `Average Public Build Rate` = mean(PubRate)) %>% 
             ungroup() %>%
             mutate(SubPeriod = ifelse(Date < as.Date("1914-04-01"), "Victorian (1856 - 1913)", 
                                ifelse(Date > as.Date("1919-04-01") & Date < as.Date("1940-01-01"), "Inter-War (1920 - 1939)", 
                                ifelse(Date > as.Date("1946-01-01") & Date < as.Date("1980-01-01"), "Post-War (1947 - 1979)", "Modern (1980 - 2019)")))) %>% 
             group_by(SubPeriod) %>% 
             mutate(SubPeriodRate = mean(BuildRate), SubPeriodPriv = mean(PrivRate), SubPeriodPub = mean(PubRate)) %>%
             ungroup()
             
view(MeanCalc2) 

Bind <- MeanCalc2 %>% 
        select(c("Period", "Average Total Build Rate", "Average Private Build Rate", "Average Public Build Rate")) %>% 
        distinct()

view(Bind) 

TotSume = MeanCalc2 %>% 
          select(c("SubPeriod","SubPeriodRate", "SubPeriodPriv", "SubPeriodPub")) %>%
          distinct() %>% 
          rename("Period" = "SubPeriod", "Average Total Build Rate" = "SubPeriodRate" , 
                 "Average Private Build Rate" = "SubPeriodPriv", "Average Public Build Rate" = "SubPeriodPub") %>%
          rbind(Bind) %>% 
          mutate(across(where(is.numeric), ~ round(.x, digits = 2)))

view(TotSume)  

setwd("C:/Users/S.Watling/Documents/Counterfactuals")
write.csv(TotSume, "PeriodSummary.csv")

Graph <- Total %>% 
         mutate(Public = (Total  / Houseestimate), Private = (`Private unassisted` / Houseestimate), 
                GrossPre = (Prefabs / Houseestimate), GrossAssoc = (`Housing associations` / Houseestimate), 
                Assist = `Private assisted` / Houseestimate, Assisted = Private + Assist,
                Prefabs = Assisted + GrossPre, NonProfit = Prefabs + GrossAssoc) %>%
         ggplot() + geom_area(aes(x = Date, y = Public, fill = "Public")) + 
         geom_area(aes(x = Date, y = NonProfit, fill = "Voluntary")) +
         geom_area(aes(x = Date, y = Prefabs, fill = "Prefabs")) +  
         geom_area(aes(x = Date, y = Assisted, fill = "Assisted")) +
         geom_area(aes(x = Date, y = Private, fill = "Private")) +
         scale_fill_manual(values = c("yellow", "purple", "steelblue", "tomato2", "green")) +
         labs(title = "Gross British Housebuilding from 1856", y = "Percentage of Housing Stock", fill = "Tenure")

Graph 

Graph2 <- Total %>% 
          mutate(Private = (`Private assisted` + `Private unassisted`)*100 / Houseestimate,
             Public = Private + ((`Public Housing` + `Housing associations` + Prefabs)*100) / Houseestimate) %>% 
             ggplot() + geom_area(aes(x = Date, y = Public, fill = "Public")) + 
             geom_area(aes(x = Date, y = Private, fill = "Private")) + 
             scale_fill_manual(values = c("#1E8BC3", "#E85B4E")) + geom_line(aes(x = Date, y = Public), size = 0.4) + 
             geom_line(aes(x = Date, y = Private), size = 0.4) + 
             geom_segment(x = as.Date("1947-01-01"), xend = as.Date("1947-01-01"), y = Inf, yend = 0, linetype = 2) + 
             geom_segment(x = as.Date("1980-01-01"), xend = as.Date("1980-01-01"), y = Inf, yend = 0, linetype = 2) + 
             geom_text(x = as.Date("1992-04-01"), y = 2.8, label = "Right to Buy", size = 3) + 
  geom_text(x = as.Date("1987-09-01"), y = 2.65, label = "(1980)", size = 3) +
  geom_text(x = as.Date("1954-01-01"), y = 2.8, label = "TCPA", size = 3) + 
  geom_text(x = as.Date("1954-04-01"), y = 2.65, label = "(1947)", size = 3) +
             labs(title = "Gross Housebuilding in England and Wales from 1856", y = "New Homes as a Share of Existing Stock (%)", x = "Year", fill = "Tenure") + 
              geom_hline(yintercept = 0) + scale_x_date(breaks = as.Date(c("1860-01-01", "1880-01-01", "1900-01-01", "1920-01-01", "1940-01-01", "1960-01-01", "1980-01-01", "2000-01-01", "2020-01-01")), date_labels = "%Y") + geom_vline(xintercept = as.Date("1856-01-01"), size = 1) +
            theme(panel.background = element_blank(), plot.title = element_text(size = 10))
  
Graph2  

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")

ggsave(filename = "Figure 1.png",plot = Graph2, width= 7.5, height=4, dpi=300)
ggsave(filename = "Figure 1.eps",plot = Graph2, width= 7.5, height=4, dpi=300)

view(Total) 

setwd("C:/Users/S.Watling/Documents/Historical Housing Statistics")

Mean <- Total %>% 
        filter(Date < as.Date("1914-01-01") | Date > as.Date("1918-01-01")) %>% 
        filter(Date < as.Date("1940-01-01") | Date > as.Date("1946-04-01")) %>%
        mutate(Period = ifelse(Date < as.Date("1940-01-01"), 1, 0)) %>% 
        mutate(GrossRate = Total * 100 /Houseestimate) %>% 
        mutate(Private = (`Private assisted` + `Private unassisted`)*100 / Houseestimate ) %>%
        group_by(Period) %>% 
        summarise(Totalmean = mean(GrossRate), `Total Private` = mean(Private))
  
view(Mean) 

view(Total)

HouseStock <- Total %>% 
              mutate(StockChange = Houseestimate - lag(Houseestimate)) %>% 
              mutate(PctStockChange = StockChange * 100 / Houseestimate) %>% 
              mutate(MeanAvg = rollmean(PctStockChange, k = 2, fill=NA)) %>% 
              rename("Tot" = "Public") %>% 
              mutate(Public = Tot - Private)
              
              
view(HouseStock) 

HouseGraph <- HouseStock %>% 
              ggplot(aes(x = Date, y = MeanAvg)) + geom_line(size = 1) + 
              geom_hline(yintercept = 0) + geom_vline(xintercept = as.numeric(as.Date("1857-04-01"))) + 
              labs(title = "Pct Growth in Housing Stock in England and Wales 1857-2019")

HouseGraph 

ggsave(filename = "Housestock.png", plot = HouseGraph, width= 7.5, height=4, dpi=300)

HouseLab <- HouseStock %>% 
  filter(Date > as.Date("1920-01-01") & Date < ("2020-01-01")) %>% 
  mutate(Decade = ifelse(Date > as.Date("1920-01-01") & Date < ("1930-01-01"), "1920s", 
                  ifelse(Date > as.Date("1930-01-01") & Date < ("1940-01-01"), "1930s", 
                  ifelse(Date > as.Date("1945-01-01") & Date < ("1950-01-01"), "1940s", 
                  ifelse(Date > as.Date("1950-01-01") & Date < ("1960-01-01"), "1950s",
                  ifelse(Date > as.Date("1960-01-01") & Date < ("1970-01-01"), "1960s", 
                  ifelse(Date > as.Date("1970-01-01") & Date < ("1980-01-01"), "1970s",  
                  ifelse(Date > as.Date("1980-01-01") & Date < ("1990-01-01"), "1980s", 
                  ifelse(Date > as.Date("1990-01-01") & Date < ("2000-01-01"), "1990s", 
                  ifelse(Date > as.Date("2000-01-01") & Date < ("2010-01-01"), "2000s", 
                  ifelse(Date > as.Date("2010-01-01") & Date < ("2020-01-01"), "2010s", 0))))))))))) %>% 
                  filter(Decade != 0)

view(HouseLab)

setwd("C:/Users/S.Watling/Documents/Historical Housing Statistics")
MeanRate <- HouseLab %>% 
            mutate(Private = `Private assisted` + `Private unassisted`, Public = `Public Housing` + `Housing associations`) %>% 
            mutate(PrivRate = Private * 100 / Houseestimate, PubRate = Public * 100 / Houseestimate) %>% 
            group_by(Decade) %>% 
            summarise(`Mean Private Building Rate` = mean(PrivRate), `Mean Public Building Rate` = mean(PubRate)) %>% 
            mutate(`Mean Total Building Rate` = `Mean Private Building Rate` + `Mean Public Building Rate`) %>%
            mutate(across(where(is.numeric), ~ signif(.x, digits = 3)))

view(MeanRate) 

write.csv(MeanRate, "MeanEng.csv")

MaxRate <- HouseLab %>% 
  mutate(TotRate = Total*100 / Houseestimate) %>%
  group_by(Decade) %>% 
  mutate(Maxrate = max(TotRate), Maxadd = max(PctStockChange)) %>%
  filter(TotRate == Maxrate | PctStockChange == Maxadd) %>% 
  select(c("Decade", "Date", "Maxrate", "PctStockChange")) %>% 
  rename("Year of Highest Housingstock Growth Rate" = "Date")  
  
view(MaxRate)

write.csv(Housemax)

Population <- read_csv("HistoricalPopulation.csv") %>%
              row_to_names(row_number = 1) %>% 
              slice(5:154) %>% 
              select(-c("See note")) %>%
              rename("Population" = "Total Population", "Date" = "Year") %>% 
              mutate(Population = gsub("\\,", "", Population)) %>% 
              mutate(Date = as.numeric(Date)) %>%
              mutate(Population = as.numeric(Population)) 
view(Population) 

GraphNet <- Total %>% 
         mutate(Date = gsub("\\-.*", "", Date)) %>% 
         mutate(Date = as.numeric(Date)) %>%
         mutate(Netgain = Houseestimate - lag(Houseestimate)) %>% 
         mutate(Netchange = Netgain / Houseestimate) %>%
         filter(Date > 1870) %>% 
         full_join(Population, by = c("Date" = "Date")) %>%
         mutate(Date = parse_date_time(Date, orders = c("Y", "ymd"))) %>% 
         mutate(Population = Population / 1000000) %>%
         mutate(HousePerCap = Houseestimate / Population) %>% 
         mutate(PerCapNetgain = HousePerCap - lag(HousePerCap)) %>% 
         mutate(PerCapPct = PerCapNetgain*100 / HousePerCap) %>% 
         mutate(PerCapPct = ifelse(PerCapPct < -5 | PerCapPct > 5, NA, PerCapPct)) %>% 
         mutate(PerCapPct = ifelse(Date > as.Date("1910-01-01") & Date < as.Date("1920-01-01"), NA, PerCapPct)) %>%
         mutate(PerCapPct = ifelse(Date > as.Date("1939-01-01") & Date < as.Date("1947-01-01"), NA, PerCapPct)) %>% 
         mutate(PerCapPct = ifelse(Date == as.Date("1991-01-01"), NA, PerCapPct)) %>% 
         mutate(PerCapAvg = rollmean(PerCapPct, k = 2 , fill = NA)) %>% 
         mutate(ImpCapAvg = na.approx(PerCapAvg, maxgap = 4, na.rm = FALSE))

view(GraphNet)

NetPerCapGraph <- GraphNet %>%
                  ggplot(aes(x =Date, y = ImpCapAvg)) + geom_line(size = 1.2, color = "blue") + 
                  geom_hline(yintercept = 0)

NetPerCapGraph

view(Total)

Export <- Total %>% 
          mutate(Country = "Unitedkingdom") %>%
          select(c("Country", "Date", "Total", "Houseestimate", "Private unassisted", "Netinc")) %>%
          filter(Date > as.Date("1991-03-01"))

view(Export)
write.csv(Export, "Britain.csv") 

HousePrices <- read_csv("UK_House_Price_Since_1952.csv") %>% 
               rename("Date" = "...1", "House Price" = "...3") %>% 
               select(c("Date", "House Price")) %>% 
               mutate(`House Price` = gsub("\\,", "", `House Price`), Date = gsub(" ", "\\/", Date)) %>%
               slice(4:283) %>% 
               mutate(Date = as.Date(as.yearqtr(Date, format = "Q%q/%Y")))
              
view(HousePrices) 

Wages <- read_csv("Wage Price Data.csv") %>% 
         select(c("Date","...2", "CPI Index", "Earnings")) %>% 
         fill(Date) %>% 
         slice(-(1:6)) %>% 
         unite("Date", Date:...2) %>%
         mutate(Date = yq(Date)) %>% 
         right_join(HousePrices, by = c("Date" = "Date")) %>% 
         mutate(across(`CPI Index`:`House Price`, ~ as.numeric(.x))) %>% 
         mutate(`CPI Index` = `CPI Index` / 6.57) %>%
         mutate(across(`Earnings`:`House Price`, ~.x / `CPI Index`)) %>% 
         mutate(BaseEarnings = Earnings / 0.9250000, BaseHousePrice = `House Price` / 218.9000) %>%
         mutate(across(`BaseEarnings`:`BaseHousePrice`, ~ log10(.x), .names = "{fn}_{col}")) %>% 
         mutate(across(`BaseEarnings`:`BaseHousePrice`, ~ .x * 10)) %>%
         filter(Date >= as.Date("1960-01-01"))
         
view(Wages) 

write.csv(Wages, "Price Wage Comparison.csv")


Graph1 <- Wages %>% 
          select(c("Date", "1_BaseHousePrice"))  

Graph2 <- Wages %>% 
          select(c("Date", "1_BaseEarnings")) 

view(Graph2)

colors <- c("House Prices" = "#274F9E", "Wages" = "#E6223F")

Graph <- ggplot() + 
  geom_hline(yintercept = 1) + 
  geom_line(aes(x = Date, y = `1_BaseHousePrice`, color = "House Prices"), data = Graph1) + 
  geom_line(aes(x = Date, y = `1_BaseEarnings`, color = "Wages"), data = Graph2) + 
  labs(x = "Year",
       y = "Log of Real House Prices and Wages, 1960 = 1",
       color = "Legend", title = "Real House Prices and Wages in England from 1960-2015") +
  scale_color_manual(values = colors) + 
  geom_vline(xintercept = as.numeric(as.Date("1960-01-01"))) +
  geom_rect(data = data.frame(xmin = as.Date("1973-07-01", "%Y-%m-%d"),
                              xmax = as.Date("1974-04-01", "%Y-%m-%d"),
                              ymin = 1,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", alpha = 0.35, inherit.aes=FALSE) + 
  geom_rect(data = data.frame(xmin = as.Date("1975-07-01", "%Y-%m-%d"),
                              xmax = as.Date("1976-01-01", "%Y-%m-%d"),
                              ymin = 1,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", alpha = 0.4, inherit.aes=FALSE) + 
  geom_rect(data = data.frame(xmin = as.Date("1980-01-01", "%Y-%m-%d"),
                              xmax = as.Date("1981-04-01", "%Y-%m-%d"),
                              ymin = 1,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", alpha = 0.35, inherit.aes=FALSE) + 
  geom_rect(data = data.frame(xmin = as.Date("1990-07-01", "%Y-%m-%d"),
                              xmax = as.Date("1991-10-01", "%Y-%m-%d"),
                              ymin = 1,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", alpha = 0.35, inherit.aes=FALSE) + 
  geom_rect(data = data.frame(xmin = as.Date("2008-04-01", "%Y-%m-%d"),
                              xmax = as.Date("2009-07-01", "%Y-%m-%d"),
                              ymin = 1,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "black", alpha = 0.35, inherit.aes=FALSE) + theme(panel.background = element_blank()) + 
            scale_x_date(breaks = as.Date(c("1960-01-01", "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")), 
                         labels = c("1960", "1970", "1980", "1990", "2000", "2010", "2020"))+ theme(plot.title = element_text(size = 10))

Graph 

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")

ggsave("Figure 2.png" ,plot = Graph, width= 7.5, height=4, dpi=300) 
ggsave("Figure 2.eps" ,plot = Graph, width= 7.5, height=4, dpi=300, device = cairo_ps) 

SecondGraph <- ggplot() + 
  geom_hline(yintercept = 1) + 
  geom_line(aes(x = Date, y = `1_BaseHousePrice`, color = "House Prices"), data = Graph1) + 
  geom_line(aes(x = Date, y = `1_BaseEarnings`, color = "Wages"), data = Graph2) + 
  labs(x = "Year",
       y = "Log of Real Values Where 1960 = 1",
       color = "Legend", title = "Real House Prices and Wages from 1960") +
  scale_color_manual(values = colors) + 
  geom_rect(data = data.frame(xmin = as.Date("1971-07-01", "%Y-%m-%d"),
                              xmax = as.Date("1974-01-01", "%Y-%m-%d"),
                              ymin = 1,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "red", alpha = 0.2, inherit.aes=FALSE) + 
  geom_rect(data = data.frame(xmin = as.Date("1977-07-01", "%Y-%m-%d"),
                              xmax = as.Date("1979-10-01", "%Y-%m-%d"),
                              ymin = 1,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "red", alpha = 0.2, inherit.aes=FALSE) + theme(plot.title = element_text(size = 12))
SecondGraph 

ggsave("Price Wage2.png", plot = SecondGraph ,width= 7.5, height=4, dpi=300) 

Graph3Alt <- Wages %>%
             filter(Date < as.Date("1980-04-01"))
  
Graph4Alt <- Wages %>%
             filter(Date < as.Date("1980-04-01")) 

view(Graph3Alt)

AltGraph <- ggplot() + 
            geom_hline(yintercept = 100) + 
  geom_line(aes(x = Date, y = BaseHousePrice, color = "House Prices"), data = Graph3Alt) + 
  geom_line(aes(x = Date, y = BaseEarnings, color = "Wages"), data = Graph4Alt) + 
  labs(x = "Year",
       y = "Indexed Real Values, 1960 = 100",
       color = "Legend", title = "Real House Prices and Wages from 1960") +
  scale_color_manual(values = colors) 

AltGraph 

ggsave("Price Wage3.png", plot = AltGraph ,width= 7.5, height=4, dpi=300)

AltGraph 

geom_smooth(aes(x = Date, y = BaseHousePrice, color = "House Prices"), data = Graph3Alt, 
            method = "lm", se = FALSE) + 
  geom_smooth(aes(x = Date, y = BaseEarnings, color = "Wages"), data = Graph4Alt, 
              method = "lm", se = FALSE) +

LmHouse <- Prices %>% 
           lm(formula = NewHouse ~ Date) 



LmHouse

summary(LmHouse)

LmWage <-