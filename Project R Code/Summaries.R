setwd("C:/Users/S.Watling/Documents/Processed Modern") 

library(tidyverse) 
library(lubridate)

Combined <- read_csv("Combined.csv") %>% 
            select(-c("...1")) %>% 
            filter(Country != "Italy" & Country != "Greece" & Country != "Portugal" & Country != "Spain") %>%
            mutate(Date = dmy(Date)) %>% 
            mutate(`Build Rate` = Y7 * 100 / `X5 (Estimate)`) %>% 
            mutate(`Public Build Rate` = `Build Rate`* `1`) %>%
            mutate(`Private Build Rate` = `Build Rate`* `4`) 

view(Combined) 

PostWarSummary <- Combined %>% 
                  filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>% 
                  select(c("Date", "Country", "Build Rate", "Private Build Rate", "Public Build Rate")) 

EuroAvg <- PostWarSummary %>% 
           filter(Country != "Unitedkingdom") %>% 
           group_by(Date) %>% 
           summarise(`Build Rate` = mean(`Build Rate`), `Private Build Rate` = mean(`Private Build Rate`), 
            `Public Build Rate` = mean(`Public Build Rate`)) %>% 
           mutate(Country = "Western European Average")

view(EuroAvg)
           
FinalSum <- PostWarSummary %>% 
            rbind(EuroAvg) %>%
                  group_by(Country) %>%
                  mutate(`Avg Build Rate` = mean(`Build Rate`), `Avg Private Build Rate` = mean(`Private Build Rate`), 
                         `Avg Public Build Rate` = mean(`Public Build Rate`), `Maximum Private Build Rate` = max(`Private Build Rate`)) %>% 
                  filter(Date == as.Date("1979-01-01") | `Maximum Private Build Rate` == `Private Build Rate`) %>% 
            arrange(`Avg Private Build Rate`)

view(FinalSum) 

FinalSum2 <- FinalSum %>% 
             filter(Date == as.Date("1979-01-01")) %>% 
             select(c("Country", "Private Build Rate")) %>% 
             rename("1979 Private Build Rate" = "Private Build Rate") %>% 
             full_join(FinalSum, by = c("Country" = "Country")) %>% 
             filter(Date != as.Date("1979-01-01")) %>% 
             rename("Year of Maximum Private Build Rate" = "Date") %>% 
             select(c("Country", "Year of Maximum Private Build Rate", "Maximum Private Build Rate", "Avg Private Build Rate", "1979 Private Build Rate")) %>% 
             mutate(across(`Maximum Private Build Rate`:`1979 Private Build Rate`, ~ round(.x, digits = 2))) %>% 
             relocate(`Year of Maximum Private Build Rate`, .after = `Maximum Private Build Rate`) %>% 
             relocate(`Avg Private Build Rate`, .after = Country) %>% 
             mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country))

view(FinalSum2) 

WestEuro2 <- FinalSum2 %>% 
             filter(Country == "Western European Average") 

WestEuro3 <- FinalSum2 %>% 
             filter(Country != "Western European Average") %>% 
             rbind(WestEuro2) 

view(WestEuro3)

setwd("C:/Users/S.Watling/Documents/Counterfactuals")

write.csv(WestEuro3, "FinalSum.csv")  

PubSum <- PostWarSummary %>% 
          group_by(Country) %>%
          summarise(Meanpub = mean(`Public Build Rate`)) 

Mean <- PostWarSummary %>% 
        filter(Country != "Switzerland" & Country != "Belgium" & Country != "Finland" & Country != "Unitedkingdom") %>% 
        summarise(Pubrate = mean(`Public Build Rate`))
  
          
view(Mean) 

Mean2 <- PostWarSummary %>% 
  filter(Country != "Switzerland" & Country != "Belgium" & Country != "Finland" & Country != "Unitedkingdom" & Country != "Ireland" & Country != "Austria") %>% 
  summarise(Pubrate = mean(`Public Build Rate`))

view(Mean2)

Surplus <- PostWarSummary %>% 
           filter(Country == "Unitedkingdom") %>% 
           select(c("Date", "Private Build Rate")) %>% 
           rename("BritPriv" = "Private Build Rate") %>% 
           full_join(PostWarSummary, by = c("Date" = "Date")) %>% 
           filter(Country != "Unitedkingdom") %>% 
           mutate(Index = ifelse(`Private Build Rate` > BritPriv, 1, 0)) %>% 
           group_by(Country) %>% 
           summarise(True = mean(Index)) %>% 
           filter(True == 1)

view(Surplus)

DeepComp <- Combined %>% 
             filter(Country %in% c("Switzerland", "Sweden", "Netherlands", "Unitedkingdom")) %>% 
             filter(Date > as.Date("1947-01-01") & Date < as.Date("2016-01-01")) %>% 
             mutate(Period = ifelse(Date < as.Date("1980-01-01"), "1948 - 1979", "1980 - 2015")) %>% 
             group_by(Country, Period) %>% 
             mutate(`Avg Build Rate` = mean(`Build Rate`), `Avg Private Build Rate` = mean(`Private Build Rate`), `Avg Public Build Rate` = mean(`Public Build Rate`)) %>%
             ungroup() %>%
             group_by(Country) %>% 
             mutate(`Avg Build Rate 1948 - 2015` = mean(`Build Rate`), `Avg Private Build Rate 1948 - 2015` = mean(`Private Build Rate`), `Avg Public Build Rate 1948 - 2015` = mean(`Public Build Rate`)) %>% 
             select(c("Country","Period", "Avg Build Rate", "Avg Private Build Rate", "Avg Public Build Rate", "Avg Build Rate 1948 - 2015", "Avg Private Build Rate 1948 - 2015", "Avg Public Build Rate 1948 - 2015")) %>%
             distinct() 

view(DeepComp) 

PeriodComp <- DeepComp %>% 
              select(c("Country", "Period", "Avg Build Rate", "Avg Private Build Rate", "Avg Public Build Rate")) %>% 
              pivot_longer(-c("Country", "Period"), names_to = "Item", values_to = "Value") %>% 
              relocate(Item, .before = Period) %>%
              unite("Index", Item:Period, sep = " ") %>% 
              pivot_wider(names_from = Index, values_from = Value)

view(PeriodComp)

FullComp <- DeepComp %>% 
            select(c("Country", "Avg Build Rate 1948 - 2015", "Avg Private Build Rate 1948 - 2015", "Avg Public Build Rate 1948 - 2015")) %>% 
            distinct() %>% 
            full_join(PeriodComp, by = c("Country" = "Country")) %>% 
            mutate(across(`Avg Build Rate 1948 - 2015`:`Avg Public Build Rate 1980 - 2015`, ~ round(.x, digits = 2)))

view(FullComp) 

setwd("C:/Users/S.Watling/Documents/Counterfactuals") 

write.csv(FullComp, "Full1948.csv")

UKSum <- Combined %>% 
         filter(Country == "Unitedkingdom") %>% 
         mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>% 
         filter(Date > as.Date("2009-01-01") & Date < as.Date("2020-01-01")) %>%
         summarise(`Total Build` = mean(`Build Rate`), `Private Build` = mean(`Private Build Rate`), `Public Build` = mean(`Public Build Rate`))

view(UKSum)
