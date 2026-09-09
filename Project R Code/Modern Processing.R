setwd("C:/Users/S.Watling/Documents/Processed Modern") 

options(scipen = 999)  

library(tidyverse)
library(lubridate)
library(rlang)
library(zoo)
library(xts)
library(ggpattern)

Household <- read_csv("FullHousehold.csv") 
view(Household) 

Tenure <- read_csv("ModernTenure.csv") %>%
          pivot_longer(-c("Country", "Item"), names_to = "Date", values_to = "Value") %>%
          pivot_wider(names_from = Item, values_from = Value) %>%
          mutate(Country = gsub("Westgermany", "Germany", Country)) %>%
          group_by(Country) %>%
          mutate(across(`1`:`7`, ~ na.approx(.x, na.rm = FALSE, maxgap = 5))) %>% 
          fill(`1`:`7`, .direction = "downup")
view(Tenure)

Combined <- Household %>% 
            mutate(PerCapHouse = `X5 (Estimate)`/ X50) %>%
            full_join(Tenure, by = c("Country" = "Country", "Date" = "Date")) %>% 
            mutate(Total = Y7 * 100 / `X5 (Estimate)`) %>% 
            mutate(Publicbuild = `1` * Y7, Privatebuild = `4` * Y7) %>%
            mutate(Public = Publicbuild*100 / `X5 (Estimate)`, Private = Privatebuild*100 / `X5 (Estimate)`, Sum = Private + Public) %>%
            mutate(Date = dmy(Date)) %>% 
            group_by(Country) %>%
            mutate(PerCapPct = (PerCapHouse - lag(PerCapHouse))*100/ PerCapHouse) %>%
            ungroup()
           
view(Combined) 

Combined2 <- Household %>% 
  mutate(PerCapHouse = `X5 (Estimate)`/ X50) %>%
  full_join(Tenure, by = c("Country" = "Country", "Date" = "Date")) 

write.csv(Combined2, "Combined.csv") 

Sum <-Combined %>%
       filter(Date < as.Date("1980-01-01") & Date > as.Date("1954-01-01")) %>% 
       summarise(Mean = mean(Total, na.rm = TRUE))

Sum 

DemRate <- Combined %>% 
           mutate(DemRate = Y2 / Y7) %>% 
           filter(Country == "Unitedkingdom")

view(DemRate)

view(Sum)

GrossHaus <- function(Data, Nation, Value) { 
  Nation <- ensym(Nation)
  Value <- ensym(Value)
  p <- Data %>% 
    mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
    filter(Country == Nation | Country == "United Kingdom" ) %>%
    ggplot(aes(x = Date, y = !!Value , color = Country)) + geom_line(size = 1.2) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = as.numeric(as.Date("1950-01-01"))) + 
    ggtitle(paste("Gross", as_label(enquo(Value)) , "Housebuilding")) + labs(y = "% of the Housing Stock") + 
    scale_color_manual(values = c("#006E97", "#E6223F"))
  ggsave(paste0(Nation, '.png'),plot = p, width=6, height=4, dpi=300)
} 

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Private Comparison")
sapply(unique(Combined$Country), function(x) GrossHaus(Combined, !!x, Private))

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Public Comparison") 
sapply(unique(Combined$Country), function(x) GrossHaus(Combined, !!x, Public)) 

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Gross Comparison")  
sapply(unique(Combined$Country), function(x) GrossHaus(Combined, !!x, Total)) 

view(Combined)

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Net Comparison") 
sapply(unique(Combined$Country), function(x) GrossHaus(Combined, !!x, PerCapPct)) 

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Stock Comparison") 
sapply(unique(Combined$Country), function(x) GrossHaus(Combined, !!x, PerCapHouse)) 

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Line Graphs") 

Combined %>% 
select(c("Country", "Date", "Public")) %>% 
filter(Country %in% c("Denmark", "Germany", "Netherlands", "Norway", "Sweden", "Unitedkingdom")) %>% 
filter(Date > as.Date("1949-01-01") & Date < ("2016-01-01")) %>%
mutate(Category = ifelse(Country == "Unitedkingdom", "United Kingdom", "Control Group")) %>% 
group_by(Category, Date) %>% 
mutate(Mean = mean(Public)) %>% 
select(c("Date", "Category", "Mean")) %>% 
rename("Country" = "Category", "Public" = "Mean") %>% 
GrossHaus(`Control Group`, Public) 

PerCapHausGraph <- Combined %>% 
                   filter(Country %in% c("Unitedkingdom", "Finland", "Sweden", "Netherlands", "Switzerland", "Denmark", "Germany")) %>% 
                   mutate(AltPerCapHouse = `X5 (Alternative)` / X50) %>% 
                   mutate(Country = gsub("Germany", "West Germany", Country)) %>%
                   select(c("Country", "Date", "AltPerCapHouse")) %>% 
                   pivot_wider(names_from = "Country", values_from = "AltPerCapHouse") %>% 
                   mutate(across(`Denmark`:`Unitedkingdom`, ~ .x * 100 / Unitedkingdom)) %>% 
                   select(-c("Unitedkingdom")) %>%
                   pivot_longer(-c("Date"), names_to = "Country", values_to = "Value") %>% 
                   filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>% 
                   ggplot(aes(x = Date, y = Value, color = Country)) + geom_line(size = 1.2) + 
                   scale_y_continuous(limits = c(75, 125)) + scale_color_manual(values = c("#E85B4E", "#1F74BA","#078E51" , "#E76D25", "#CAD510", "#872281")) + 
                   geom_hline(yintercept = 100, color = "black", size = 1.2) + geom_vline(xintercept = as.numeric(as.Date("1955-01-01")), size = 1) + 
                   geom_text(x = as.numeric(as.Date("1960-03-01")), y = 97,label = "UK = 100", color = "black") + labs(title = "Ratio of Homes Per Person Relative to the UK from 1955-1979", y = "Ratio of Homes Per Person", x = "Year") + 
                   theme(panel.background = element_blank(), plot.title = element_text(size = 10)) + scale_x_date(breaks = as.Date(c("1955-01-01", "1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01")),
                                                                             labels = c("1955", "1960", "1965", "1970", "1975", "1980")
                                                                            )
                   
PerCapHausGraph  

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
ggsave(filename = "Figure 6.png", plot = PerCapHausGraph, width=6, height=4, dpi=300)
ggsave(filename = "Figure 6.eps", plot = PerCapHausGraph, width=6, height=4, dpi=300)

PerCapHausGraph2 <- Combined %>% 
  filter(Country %in% c("Unitedkingdom", "Finland", "Sweden", "Netherlands", "Switzerland", "France", "Denmark")) %>% 
  mutate(AltPerCapHouse = `X5 (Alternative)` / X50) %>%
  select(c("Country", "Date", "AltPerCapHouse")) %>% 
  pivot_wider(names_from = "Country", values_from = "AltPerCapHouse") %>% 
  mutate(across(`Denmark`:`Unitedkingdom`, ~ .x * 100 / Unitedkingdom)) %>% 
  select(-c("Unitedkingdom")) %>%
  pivot_longer(-c("Date"), names_to = "Country", values_to = "Value") %>% 
  filter(Date > as.Date("1979-01-01") & Date < as.Date("2016-01-01")) %>% 
  ggplot(aes(x = Date, y = Value, color = Country)) + geom_line(size = 1.2) + 
  scale_y_continuous(limits = c(85, 130)) + scale_color_manual(values = c("#872281", "#E85B4E", "#1F74BA", "#E76D25" , "#CAD510", "#078E51" )) + 
  geom_hline(yintercept = 100, color = "black", size = 1.2) + geom_vline(xintercept = as.numeric(as.Date("1980-01-01")), size = 1) + 
  geom_text(x = as.numeric(as.Date("1983-10-01")), y = 97,label = "UK = 100", color = "black") + labs(title = "Ratio of Homes Per Person Relative to the UK from 1980-2015", y = "Ratio of Homes Per Person", x = "Year") + 
  theme(panel.background = element_blank(), plot.title = element_text(size = 10)) + scale_x_date(breaks = as.Date(c("1980-01-01","1985-01-01", "1990-01-01", "1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01")), 
                                                           labels = c("1980", "1985", "1990", "1995", "2000", "2005", "2010", "2015"))

PerCapHausGraph2

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
ggsave(filename = "Figure 12.png", plot = PerCapHausGraph2, width=6, height=4, dpi=300)
ggsave(filename = "Figure 12.eps", plot = PerCapHausGraph2, width=6, height=4, dpi=300)

RatioGraph <- function(Data, Nation, Value) { 
              Nation <- ensym(Nation) 
              Value <- ensym(Value)
              p <- Data %>% 
              filter(Country %in% c(as_string(Nation), "Unitedkingdom")) %>% 
              select(c("Country", "Date", as_string(Value))) %>% 
              pivot_wider(names_from = Country, values_from = !!Value) %>% 
              mutate(Diff = !!Nation - Unitedkingdom) %>% 
              ggplot(aes(x = Date, y = Diff)) + geom_line(color = "078E51", size = 1.2) + 
              geom_hline(yintercept = 0, color = "black") + 
              ggtitle(paste("Difference in Gross Building Rates Between", as_label(enquo(Nation)) , "and Britain")) + labs(y = element_blank()) 
              ggsave(paste0(Nation, '.png'),plot = p, width=6, height=4, dpi=300)
              }

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/DiffRate Graphs") 
sapply(unique(Combined$Country), function(x) RatioGraph(Combined, !!x, Total)) 

Convergencegraph <- Combined %>%  
                    filter(Country %in% c("Netherlands", "Sweden", "Denmark", "Germany", "Unitedkingdom")) %>% 
                    filter(Date > as.Date("1969-01-01") & Date < as.Date("2017-01-01")) %>%
                    select(c("Country", "Date", "Total")) %>% 
                    pivot_wider(names_from = Country, values_from = Total) %>% 
                    mutate(across(`Germany`:`Sweden`, ~ .x - Unitedkingdom )) %>% 
                    select(-("Unitedkingdom")) %>% 
  group_by(Date) %>%
  mutate(Converging = mean(`Germany`:`Sweden`)) %>%
  ungroup() %>% 
  select(c("Date", "Converging")) 
  
Divergencegraph <- Combined %>% 
  filter(Country %in% c("Austria", "Finland","Switzerland" , "France", "Unitedkingdom")) %>% 
  filter(Date > as.Date("1969-01-01") & Date < as.Date("2017-01-01")) %>%
  select(c("Country", "Date", "Total")) %>% 
  pivot_wider(names_from = Country, values_from = Total) %>% 
  mutate(across(`Austria`:`France`, ~ .x - Unitedkingdom )) %>% 
  select(-("Unitedkingdom")) %>% 
  group_by(Date) %>%
  mutate(Diverging = mean(`Austria`:`France`)) %>%
  ungroup() %>% 
  select(c("Date", "Diverging")) %>%
  full_join(Convergencegraph, by = c("Date" = "Date")) %>% 
  pivot_longer(-c("Date"), names_to = "Item", values_to = "Value") %>%
  ggplot(aes(x = Date, y = Value, color = Item)) + geom_line(size = 1.2) + 
  geom_hline(yintercept = 0, size = 1.2, color = "black") + scale_y_continuous(limits = c(-0.5, 1.6)) + 
  geom_vline(xintercept = as.numeric(as.Date("1970-01-01")), size = 1.2) + 
  labs(title = "Relative Performance Compared to Britain", y = "Building Rates Above Britain", fill = element_blank()) + 
  scale_color_manual(values = c("#E6223F", "#078E51"))

Divergencegraph 

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Line Graphs")
ggsave("DivergenceCon.eps", plot = Divergencegraph, width=6, height=4, dpi=300)

StackGraph <- function(Data, Nation) { 
              Nation <- ensym(Nation)
              p <-  Data %>% 
              filter(Country %in% c(as_string(Nation), "Unitedkingdom")) %>% 
              mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
              ggplot() + geom_area(aes(x = Date, y = Sum, fill = "Public")) + 
              geom_area(aes(x = Date, y = Private, fill = "Private")) +  
              scale_color_manual(name= NULL,values = c("black","black")) + 
              scale_fill_manual(values = c("#1E8BC3", "#E85B4E")) + 
              geom_line(aes(x = Date, y = Private), size = 0.4) + geom_line(aes(x = Date, y = Sum), size = 0.4) +
              geom_vline(xintercept = as.numeric(as.Date("1948-01-01"))) + 
              geom_hline(yintercept = 0) +
              labs(title = "Gross Housebuilding in Switzerland and the UK from 1948-2015", y = "New Homes as Share of Existing Stock (%)", x = "Year", fill = "Tenure") + facet_wrap(~Country, ncol = 2) + 
                theme(panel.background = element_blank(), plot.title = element_text(size = 10)) 
              ggsave(paste0(Nation, 'Stack.png'),plot = p, width=6, height=4, dpi=300)
} 

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Stack Graphs")
sapply(unique(Combined$Country), function(x) StackGraph(Combined, !!x)) 
  
BarGraph <- function(Data, Value) { 
  Value <- ensym(Value)
  Data %>%  
    filter(Country %in% c("Unitedkingdom", "Denmark",  "Norway", "Austria", "Germany", "Switzerland", "Netherlands", "Finland", "Austria", "Ireland", "Sweden", "France", "West Germany", "United Kingdom", "Belgium", "Belgium*")) %>%
    mutate(Tohighlight = ifelse(Country == "Unitedkingdom" | Country == "United Kingdom", "yes", "no")) %>% 
    ggplot(aes(x = !!Value, y = reorder(Country, !!Value), fill = Tohighlight)) + 
    geom_bar(stat="identity", color = "black") + geom_vline(xintercept = 0, size = 0.9) +
    ggtitle(paste0(title = "Average ", as_label(enquo(Value)) , " Housebuilding Rate 1980-2000")) + labs(y = "Country", x  = "Percentage of the Housing Stock")
} 

Pubgraphfinal <- Combined %>% 
                 filter(Date > as.Date("1989-01-01")) %>% 
                 filter(Country %in% c("Unitedkingdom", "Denmark", "Germany", "Ireland", "Austria", "Netherlands", "Sweden", "Switzerland")) %>% 
  mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
  group_by(Country) %>%
  summarise(Meanpriv = mean(Private, na.rm = TRUE), Public = mean(Public, na.rm = TRUE))  %>%
  BarGraph(Public) + scale_x_continuous(limits = c(0,2)) + 
  geom_vline(xintercept = 0.89, linetype = "longdash") + geom_vline(xintercept = 1.41, linetype = "longdash") + 
  geom_text(x=0.6, y=4.68, label="Postwar British", size = 3) + geom_text(x=0.575, y=4.32, label="building rate", size = 3) + 
  geom_text(x=1.7, y=4.68, label="Postwar Dutch", size = 3) + geom_text(x=1.68, y=4.32, label="building rate", size = 3) + 
  labs(title = "Average Public Housebuilding Rate from 1990 Onwards") + guides(fill="none") + 
  geom_segment(aes(x = 0.75, y = 4.5, xend = 0.85, yend = 4.5), arrow = arrow(length = unit(0.2, "cm"))) + 
  geom_segment(aes(x = 1.55, y = 4.5, xend = 1.45, yend = 4.5), arrow = arrow(length = unit(0.2, "cm"))) +
  scale_fill_manual(values = c("#E85B4E", "#1E8BC3")) 

Pubgraphfinal

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Comparison Bar Charts")
ggsave("Pub1990.png", plot = Pubgraphfinal, width=7.5, height=4, dpi=300)

PubChangegraph <- Combined %>%  
                  filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01") | Date > as.Date("1989-01-01")) %>% 
  mutate(Period = ifelse(Date > as.Date("1980-01-01"), 2, 1)) %>% 
  group_by(Country, Period) %>% 
  summarise(Meanpriv = mean(Private, na.rm = TRUE), Meanpub = mean(Public, na.rm = TRUE)) %>% 
  mutate(Total = Meanpriv + Meanpub) %>%
  pivot_longer(-c("Country", "Period"), names_to = "Item", values_to = "Value") %>%
  unite("time", Period:Item) %>%
  pivot_wider(names_from = time, values_from = Value) %>%
  mutate(Pubchange = `2_Meanpub` - `1_Meanpub`, Privchange =  `2_Meanpriv` - `1_Meanpriv`, PubPct = Pubchange * 100 / `1_Meanpub`, PrivPct = Privchange * 100 / `1_Meanpriv`) %>%
                  filter(Country %in% c("Ireland", "Austria", "Netherlands", "Sweden", "Unitedkingdom", "Denmark", "Norway", "Germany")) %>% 
                  mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
                  BarGraph(PubPct) + labs(title = "Change in Average Public Housebuilding Rates by 1990", x = "Decrease from 1955-1980 Average") + 
                  scale_fill_manual(values = c("#E85B4E", "#1E8BC3"), guide="none") 
PubChangegraph 
ggsave("PubChange.eps", plot = PubChangegraph, width=7.5, height=4, dpi=300)

GrossBuildgraph <- Combined %>% 
  filter(Date > as.Date("1954-01-01")) %>% 
  mutate(Period = ifelse(Date > as.Date("1980-01-01"), 2, 1)) %>% 
  mutate(Growth = Y7 * 100 / `X5 (Estimate)`) %>%
  group_by(Country, Period) %>%
  summarise(AvgGrowth = mean(Growth, na.rm = TRUE)) %>% 
  pivot_wider(names_from = Period, values_from = AvgGrowth) %>% 
  mutate(`Change in` = `2`- `1`, `% Change in` = `Change in` * 100 / `1`) 

view(GrossBuildgraph)


PrivGraph <- Combined %>% 
             filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>%
             group_by(Country) %>%
             summarise(Private = mean(Private, na.rm = TRUE)) %>% 
             BarGraph(Private) + ggtitle("Average Private Housebuilding Rate 1955-1979") + 
             scale_fill_manual(values = c("yes" = "#E76D25", "no" = "#1F74BA"), guide = "none")
PrivGraph 

ggsave("PerCapita.png", plot = PrivGraph , width=7.5, height=4, dpi=300)

Barchart <- Combined %>% 
            filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>% 
            group_by(Country) %>% 
            mutate(Country = gsub("Germany", "West Germany", Country), Country = gsub("Unitedkingdom", "United Kingdom", Country) , 
                   Country = gsub("Belgium", "Belgium*", Country)) %>%
            summarise(Min = min(PerCapHouse), Max = max(PerCapHouse)) %>% 
            mutate(Diff = Max - Min, `Net Growth` = Diff * 100 / Min) %>%
            BarGraph(`Net Growth`) + labs(title = "Growth in the Ratio of Homes Per Person from 1955-1979", 
                      x =  "Percentage of the 1955 Homes Per Person Ratio") + 
            scale_fill_manual(values = c("yes" = "#F39207", "no" = "#97d700"), guide="none") + theme(panel.background = element_blank(), plot.title = element_text(size = 10)) + 
            scale_x_continuous(limits = c(0, 70), breaks = c(0, 10, 20, 30, 40, 50, 60, 70)) 

Barchart

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
ggsave("Figure 5.png", plot = Barchart, width=7.5, height=4, dpi=300)
ggsave("Figure 5.eps", plot = Barchart, width=7.5, height=4, dpi=300)

TenureChart <- function(Data) { 
  p <- Data %>% 
    filter(Country %in% c("Unitedkingdom", "Denmark",  "Norway", "Austria", "Germany", "Switzerland", "Netherlands", "Finland", "Austria", "Ireland", "Sweden", "Belgium", "France", "West Germany", "United Kingdom", "France*")) %>%
    mutate(Country = gsub("Germany", "West Germany", Country)) %>%
    group_by(Country) %>% 
    summarise(Private = mean(Private, na.rm = TRUE), Public = mean(Public, na.rm = TRUE)) %>% 
    pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
    ggplot(aes(x = Value, y = reorder(Country, Value), fill = fct_rev(Item))) + geom_bar(stat = "identity", color = "black") +
    scale_fill_manual(values=c("#E85B4E", "#1E8BC3" )) +
    labs(x = "Average Annual Gross Housebuilding as a Share of Housing Stock (%)", y = "Country", fill = "Tenure") +
    geom_vline(xintercept = 0, size = 1) + scale_x_continuous(limits = c(0, 4)) + theme(panel.background = element_blank(),plot.title = element_text(size = 10))} 

TenurebuildPostwar <- Combined %>% 
                      filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>% 
                      mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
                      TenureChart() + ggtitle("Average Annual Gross Housebuilding by Tenure in Europe 1955-1979") + scale_x_continuous(limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5))
TenurebuildPostwar 

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
ggsave("Figure 3.png", plot = TenurebuildPostwar , width= 7.5, height=4, dpi=300)
ggsave("Figure 3.eps", plot = TenurebuildPostwar , width= 7.5, height=4, dpi=300)

Tenurebuild80s <- Combined %>% 
  filter(Date > as.Date("1979-01-01") & Date < as.Date("2016-01-01")) %>% 
  mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>% 
  mutate(Country = gsub("France", "France*", Country)) %>%
  TenureChart() + ggtitle("Gross Building Rates By Tenure 1980-2015") + 
  geom_vline(xintercept = 3.01, linetype = "longdash")  + geom_text(x=3.52, y=6.25, label="Postwar Dutch", size = 3) + geom_text(x=3.48, y=5.75, label="building rate", size = 3) + 
  geom_vline(xintercept = 2.26, linetype = "longdash")  + geom_text(x = 1.75, y = 6.5, label = "Mean European", size = 3) + geom_text(x = 1.75, y = 6, label = "postwar building", size = 3) + geom_text(x = 1.48, y = 5.5, label = "rate", size = 3) + 
  geom_segment(aes(x = 2.15, y = 6, xend = 2.25, yend = 6), arrow = arrow(length = unit(0.2, "cm"))) + geom_segment(aes(x = 3.15, y = 6, xend = 3.05, yend = 6), arrow = arrow(length = unit(0.2, "cm")))

Tenurebuild80s 

ggsave("Post80s.eps", plot = Tenurebuild80s , width= 7.5, height=4, dpi=300) 

NewBar <- Combined %>% 
  filter(Date > as.Date("1954-01-01") & Date < as.Date("2016-01-01")) %>%
  filter(Country %in% c("Unitedkingdom", "Denmark",  "Norway", "Austria", "Germany", "Switzerland", "Netherlands", "Finland", "Austria", "Ireland", "Sweden", "Belgium", "France", "West Germany", "United Kingdom", "France*")) %>% 
  mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country), Country = gsub("France", "France*", Country)) %>%
  mutate(Period = ifelse(Date > as.Date("1979-01-01"), "Modern", "Post-war")) %>% 
  group_by(Country, Period) %>% 
  summarise(Privmean = mean(Private), Pubmean = mean(Public), Totmean = mean(Total)) %>% 
  pivot_longer(-c("Country", "Period"), names_to = "Tenure", values_to = "Value") %>% 
  pivot_wider(names_from = "Period", values_from = "Value") %>% 
  mutate(Diff = `Post-war` - Modern) %>% 
  pivot_longer(-c("Country", "Tenure"), names_to = "Period", values_to = "Value") %>% 
  unite("Bloc", Tenure:Period) %>% 
  pivot_wider(names_from = Bloc, values_from = Value) %>% 
  select(c("Privmean_Modern", "Privmean_Diff", "Pubmean_Modern", "Pubmean_Diff", "Pubmean_Modern", "Totmean_Modern")) %>% 
  pivot_longer(-c("Country", "Totmean_Modern"), names_to = "Item", values_to = "Value") %>% 
  relocate(Totmean_Modern, .after = Value) %>% 
  mutate(alphaT = ifelse(Item %in% c("Pubmean_Modern", "Privmean_Modern"), "yes", "no")) %>% 
  mutate(Item = gsub("Privmean_Modern","Post-1980 Private", Item), Item = gsub("Pubmean_Modern", "Post-1980 Public", Item), 
         Item = gsub("Privmean_Diff","Private Reduction", Item), Item = gsub("Pubmean_Diff","Public Reduction", Item))
  
view(NewBar) 
  
NewBarGraph <- NewBar %>% ggplot(aes(x = Value, y = reorder(Country, Totmean_Modern), fill = factor(Item, levels=c("Public Reduction","Private Reduction", "Post-1980 Public", "Post-1980 Private")), pattern = Item , alpha = alphaT, pattern_fill = Item, pattern_color = Item)) + 
  geom_bar_pattern(position="stack", stat = "identity", color = "black", pattern_spacing = .02, size = 0.75) +
  scale_fill_manual(values=c("#E85B4E", "#1E8BC3", "#E85B4E", "#1E8BC3" )) + 
  labs(x = "Average Annual Gross Building as a Share of Housing Stock (%)", y = "Country", fill = "Tenure", title = "Average Annual Gross Housebuilding in Europe from 1980-2015") +
  geom_vline(xintercept = 0, size = 1) + scale_x_continuous(limits = c(0, 4)) +
  scale_pattern_manual(values=c( 'none', "none", "crosshatch", "crosshatch")) +
  scale_pattern_type_manual(values=c(NA, NA, NA, NA)) + 
    scale_pattern_color_manual(values=c("#1E8BC3", "#1E8BC3", "#1E8BC3", "#E85B4E" )) +
    scale_pattern_fill_manual(values=c("#E85B4E", "#1E8BC3", "#E85B4E", "#1E8BC3" )) +
  guides(fill = guide_legend(override.aes = 
                               list(
                                 pattern = c("crosshatch", "crosshatch","none" , "none"),
                                 pattern_spacing = .02,
                                 pattern_angle = c(33, 33, 0, 0), 
                                 pattern_fill = c("#E85B4E", "#1E8BC3", "#E85B4E", "#1E8BC3" ), 
                                 pattern_color = c("#E85B4E", "#1E8BC3", "#E85B4E", "#1E8BC3" ),
                                 alpha = c(0, 0, 1,  1), 
                                 shape = NA
                               ) 
  ), pattern = "none", pattern_color = "none", pattern_fill = "none") + scale_alpha_discrete(range = c(0, 1), guide = "none") + 
  theme(panel.background = element_blank(), plot.title = element_text(size = 10)) + scale_x_continuous(limits = c(0,3.5), breaks = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5)) +
  scale_fill_manual(values = c("#E85B4E","#1E8BC3", "#E85B4E", "#1E8BC3" ), labels = c("Public Reduction \nfrom 1955-1979", "Private Reduction \nfrom 1955-1979", "Post 1980 Public", "Post 1980 Private"))

NewBarGraph 

scale_shape_identity() + geom_point(aes(x = Totmean_Modern, y = Country), color = "black", shape = 73, size = 8.4, position = position_nudge(y = 0.015)) 

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
ggsave("Figure 10.png", plot = NewBarGraph , width= 7.5, height=4, dpi=300)
ggsave("Figure 10.eps", plot = NewBarGraph , width= 7.5, height=4, dpi=300)

view(NewBar)
view(Combined)

Tenurebuild90s <- Combined %>%
                  filter(Date > as.Date("1989-01-01")) %>% 
                  mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
                  filter(Country != "Norway", Country != "France", Country != "Finland", Country != "Belgium") %>%
                  TenureChart() + ggtitle("Gross Building Rates By Tenure 1990 Onwards") + scale_x_continuous(limits = c(0, 3.5)) +
                  geom_vline(xintercept = 3.01, linetype = "longdash")  + geom_text(x=2.5, y=4.25, label="Postwar Dutch", size = 3) + geom_text(x=2.455, y=3.75, label="building rate", size = 3)

Tenurebuild90s
ggsave("Post90Tenure.png", plot = Tenurebuild90s, width= 7.5, height=4, dpi=300)

Thatcher <- function(Data, Nation) { 
  Nation <- ensym(Nation)
  p <- Data %>% 
  filter(Country %in% c(as_string(Nation), "Unitedkingdom")) %>%
  ggplot() +  geom_rect(data = data.frame(xmin = as.Date("1948-01-01", "%Y-%m-%d"),
                              xmax = as.Date("1979-05-04", "%Y-%m-%d"),
                              ymin = 0,
                              ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "brown", alpha = 0.3, inherit.aes=FALSE) + 
  geom_rect(data = data.frame(xmin = as.Date("1979-05-04", "%Y-%m-%d"),
                                 xmax = as.Date("2020-01-01", "%Y-%m-%d"),
                                 ymin = 0,
                                 ymax = Inf),
               aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
               fill = "green", alpha = 0.3, inherit.aes=FALSE) +
  geom_area(aes(x = Date, y = Total, fill = "Public")) +
  geom_area(aes(x = Date, y = Private, fill = "Private")) +  
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("tomato2", "blue")) + 
  geom_segment(x = as.numeric(as.Date("1979-05-04")),xend = as.numeric(as.Date("1979-08-04")), y = 0, yend = Inf) + 
  geom_text(label = "Thatcher", y = 3.06, x = as.numeric(as.Date("1989-01-04")), size = 3) + 
  geom_text(label = "Elected", y = 2.94, x = as.numeric(as.Date("1988-01-04")), size = 3) + 
  geom_vline(xintercept = as.numeric(as.Date("1948-01-01"))) + 
  geom_hline(yintercept = 0) + 
  labs(y = "Gross Building Rates", fill = "Tenure") + 
  facet_wrap(~Country, ncol = 2) 
  ggsave(paste0(Nation, 'ThatcherStack.png'),plot = p, width=6, height=4, dpi=300)
}

Thatcher(Combined, Denmark) 
Thatcher(Combined, Netherlands)

Combined %>% 
filter(Date > as.Date("1950-01-01")) %>%
Thatcher(Sweden)

setwd("C:/Users/S.Watling/Documents/ggplot2 Graphs/Thatcher Stack")
sapply(unique(Combined$Country), function(x) Thatcher(Combined, !!x)) 

view(Combined)

Rainbow <- Combined %>%
  select(c("Country", "Date", "Public", "Private")) %>% 
  filter(Date < as.Date("1981-01-01")) %>%
  filter(Country %in% c("Netherlands", "Unitedkingdom")) %>% 
  pivot_longer(-c("Country", "Date"), names_to = "Tenure", values_to = "Value")  %>%
  unite("Merge", Country, Tenure) %>%
  pivot_wider(names_from = Merge, values_from = Value) %>%
  mutate(UkTot = Unitedkingdom_Public + Unitedkingdom_Private) %>% 
  mutate(NethTot = Netherlands_Public + Netherlands_Private) %>% 
  mutate(Nethsurppriv = Netherlands_Private - Unitedkingdom_Private) %>% 
  mutate(Nethsurppriv = ifelse(Nethsurppriv < 0, 0, Nethsurppriv)) %>%
  mutate(Nethsurppub = NethTot - abs(Nethsurppriv)) %>%
  ggplot() + geom_area(aes(x = Date, y = NethTot, fill = "Private Surplus")) + geom_line(aes(x = Date, y = NethTot), size = 1) +
  geom_area(aes(x = Date, y = Nethsurppub, fill = "Public Surplus")) + geom_line(aes(x = Date, y = Nethsurppub)) +
  geom_area(aes(x = Date, y = UkTot, fill = "UK Public")) + geom_line(aes(x = Date, y = UkTot), size = 1) +
  geom_area(aes(x = Date, y = Unitedkingdom_Private, fill = "UK Private")) + geom_line(aes(x = Date, y = Unitedkingdom_Private)) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("#872281", "#97d700", "#1E8BC3", "#E85B4E")) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = as.numeric(as.Date("1948-01-01"))) + 
  scale_y_continuous(limits=c(0,4)) + 
   scale_x_date(breaks = as.Date(c("1950-01-01", "1955-01-01", "1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01")), 
                labels = c("1950", "1955", "1960","1965", "1970", "1975", "1980")) +
   labs(title = "Dutch Housebuilding Surplus over Britain by Tenure from 1948-1979", y = "New Homes as a Share of Existing Stock (%)",x = "Year", fill = "Tenure") + 
   theme(panel.background = element_blank(),plot.title = element_text(size = 10))

Rainbow 

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
ggsave("Figure 4 Pt I.png", plot = Rainbow, width=7.5, height=4, dpi=300)
ggsave("Figure 4 Pt I.eps", plot = Rainbow, width=7.5, height=4, dpi=300)

SweRainbow <- Combined %>%
  select(c("Country", "Date", "Public", "Private")) %>% 
  filter(Date < as.Date("1981-01-01")) %>%
  filter(Country %in% c("Sweden", "Unitedkingdom")) %>% 
  pivot_longer(-c("Country", "Date"), names_to = "Tenure", values_to = "Value")  %>%
  unite("Merge", Country, Tenure) %>%
  pivot_wider(names_from = Merge, values_from = Value) %>%
  mutate(UkTot = Unitedkingdom_Public + Unitedkingdom_Private) %>% 
  mutate(NethTot = Sweden_Public + Sweden_Private) %>% 
  mutate(Nethsurppriv = Sweden_Private - Unitedkingdom_Private) %>% 
  mutate(Nethsurppriv = ifelse(Nethsurppriv < 0, 0, Nethsurppriv)) %>%
  mutate(Nethsurppub = NethTot - abs(Nethsurppriv)) %>%
  ggplot() + geom_area(aes(x = Date, y = NethTot, fill = "Private Surplus")) + geom_line(aes(x = Date, y = NethTot), size = 1) +
  geom_area(aes(x = Date, y = Nethsurppub, fill = "Public Surplus")) + geom_line(aes(x = Date, y = Nethsurppub)) +
  geom_area(aes(x = Date, y = UkTot, fill = "UK Public")) + geom_line(aes(x = Date, y = UkTot), size = 1) +
  geom_area(aes(x = Date, y = Unitedkingdom_Private, fill = "UK Private")) + geom_line(aes(x = Date, y = Unitedkingdom_Private)) + 
  scale_color_manual(name= NULL,values = c("black","black")) + 
  scale_fill_manual(values = c("#872281", "#97d700", "#1E8BC3", "#E85B4E")) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = as.numeric(as.Date("1948-01-01"))) + 
  scale_y_continuous(limits=c(0,4)) + labs(title = "Swedish Housebuilding Surplus over Britain by Tenure 1948-1979", y = "New Homes as a Share of Existing Stock (%)",x = "Year" , fill = "Tenure") + 
  scale_x_date(breaks = as.Date(c("1950-01-01", "1955-01-01", "1960-01-01", "1965-01-01", "1970-01-01", "1975-01-01", "1980-01-01")), 
               labels = c("1950", "1955", "1960","1965", "1970", "1975", "1980")) + theme(panel.background = element_blank(),plot.title = element_text(size = 10))

SweRainbow 

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
ggsave("Figure 4 Pt II.png", plot = SweRainbow, width=7.5, height=4, dpi=300)
ggsave("Figure 4 Pt II.eps", plot = SweRainbow, width=7.5, height=4, dpi=300)

GenRainbow <- Combined %>% 
  select(c("Country", "Date", "Public")) %>% 
  filter(Country %in% c("Denmark", "Germany", "Netherlands", "Norway", "Sweden", "Unitedkingdom")) %>% 
  filter(Date > as.Date("1949-01-01") & Date < ("2016-01-01")) %>%
  mutate(Category = ifelse(Country == "Unitedkingdom", "United Kingdom", "Control Group")) %>% 
  group_by(Category, Date) %>% 
  mutate(Mean = mean(Public)) %>% 
  select(c("Date", "Category", "Mean")) %>% 
  distinct() %>%
  pivot_wider(names_from = Category, values_from = Mean) %>% 
  mutate(Base = ifelse(`Control Group` < `United Kingdom`, `Control Group`, `United Kingdom`))
  
view(GenRainbow) 
  
PubRainbow <- GenRainbow %>% 
              ggplot() +  geom_line(aes(x = Date, y = `United Kingdom`), size = 1.5) + 
              geom_line(aes(x = Date, y = `Control Group`), size = 1.5) +
              geom_area(aes(x = Date, y = `Control Group`, fill = "Mixed - Tenure\nAverage Surplus \nOver Britain")) + 
              geom_area(aes(x = Date, y = `United Kingdom`, fill = "British Surplus \nOver Mixed -\nTenure Average")) + 
              geom_area(aes(x = Date, y = `Base`, fill = "Public Housebuilding")) + 
              geom_line(aes(x = Date, y = `Base`)) + 
              scale_fill_manual(values = c("#078E51", "#E85B4E", "#1E8BC3")) + 
              geom_hline(yintercept = 0) + geom_vline(xintercept = as.numeric(as.Date("1950-01-01"))) + 
              labs(y = "New Homes as a Share of Existing Stock (%)", title = "Public Housebuilding in the UK and Mixed-Tenure Countries from 1948-2015", fill = "Country", x = "Year") + theme(panel.background = element_blank(),plot.title = element_text(size = 10)) + 
               scale_x_date(breaks = as.Date(c("1950-01-01", "1960-01-01",  "1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01")), 
               labels = c("1950", "1960", "1970", "1980", "1990", "2000", "2010")) 
              
       
PubRainbow 

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
ggsave("Figure 8.png", plot = PubRainbow, width=7.5, height=4, dpi=300) 
ggsave("Figure 8.eps", plot = PubRainbow, width=7.5, height=4, dpi=300)
  
Post1990graph <- Combined %>% 
                 filter(Date > as.Date("1989-01-01")) %>% 
                 mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
                 group_by(Country) %>%
                 summarise(MeanGrossRate = mean(Total, na.rm = TRUE)) %>%
                 BarGraph(MeanGrossRate) + scale_fill_manual(values = c("#078E51", "#872281")) + 
                 labs(title = "Average Gross Housebuilding Rate from 1990", x = "Gross Building as a Percentage of the Housing Stock") + guides(fill="none") + scale_x_continuous(limits = c(0, 3.7)) +
                 geom_vline(xintercept = 3.01, linetype = "longdash")  + geom_text(x=3.47, y=6.25, label="Postwar Dutch", size = 3) + geom_text(x=3.43, y=5.75, label="building rate", size = 3) + 
                 geom_vline(xintercept = 2.26, linetype = "longdash")  + geom_text(x = 1.75, y = 6.5, label = "Mean European", size = 3) + geom_text(x = 1.75, y = 6, label = "postwar building", size = 3) + geom_text(x = 1.543, y = 5.5, label = "rate", size = 3) + 
                 geom_segment(aes(x = 2.05, y = 6, xend = 2.2, yend = 6), arrow = arrow(length = unit(0.2, "cm"))) + geom_segment(aes(x = 3.2, y = 6, xend = 3.05, yend = 6), arrow = arrow(length = unit(0.2, "cm")))

Post1990graph 
ggsave("Post90Gross.png", Post1990graph, width=7.5, height=4, dpi=300 )

#CounterFactual1 

Post1980Net <- Combined %>% 
               filter(Country %in% c("Unitedkingdom", "Sweden", "Netherlands", "Finland", "France", "Switzerland"))  %>% 
               filter(Date == as.Date("1955-01-01") | Date == as.Date("1980-01-01") | Date == as.Date("2020-01-01")) %>% 
               select(c("Country", "Date", "PerCapHouse")) %>% 
               pivot_wider(names_from = Date, values_from = PerCapHouse) %>% 
               mutate(Pct55 = `1955-01-01`*100/ 302.4917 , Pct80 = `1980-01-01`*100 / 382.1169 , Pct20 = `2020-01-01`*100 / 440.6073) %>% 
               mutate(Diff20 = Pct20 - Pct55, Diff80 = Pct80 - Pct55) %>%
               mutate(DiffMod = Pct20 - Pct80) %>%
               mutate(Counterfactual = DiffMod + Pct55) %>% 
               mutate(PctPost = Diff80 * 100/ Diff20) %>% 
               mutate(PctPost80 = 100 - PctPost) %>%
               mutate(across(-("Country"), ~ round(.x, digits = 1))) 
               
view(Post1980Net)  

Post1980Year <-  Combined %>% 
  filter(Country %in% c("Unitedkingdom", "Sweden", "Netherlands", "Finland", "France", "Switzerland"))  %>% 
  filter(Date > as.Date("1954-01-01") & Date < as.Date("2020-01-01")) %>% 
  mutate(Period = ifelse(Date > as.Date("1979-01-01"), 2, 1)) %>% 
  group_by(Country, Period) %>% 
  summarise(PerCapMean = mean(PerCapPct)) %>% 
  pivot_wider(names_from = Period, values_from = PerCapMean) %>%
  mutate(Diff1 = `1` - 0.9825749, Diff2 = `2` - 0.3443559) %>% 
  mutate(PeriodRatio = Diff1 / Diff2) %>% 
  mutate(across(`1`:PeriodRatio, ~ signif(.x, digits = 3))) 
  
view(Post1980Year)

AltTenuregraph <- function(Data) { 
  p <- Data %>% 
    filter(Country %in% c("Unitedkingdom", "Denmark",  "Norway", "Austria", "Germany", "Switzerland", "Netherlands", "Finland", "Austria", "Ireland", "Sweden", "Belgium", "France", "West Germany", "United Kingdom")) %>%
    group_by(Country) %>% 
    summarise(Private = mean(Private, na.rm = TRUE), Public = mean(Public, na.rm = TRUE)) %>% 
    pivot_longer(-c("Country"), names_to = "Type", values_to = "Value") %>%
    pivot_wider(names_from = Country, values_from = Value) %>% 
    mutate(across(-c("Type","Unitedkingdom"), ~ .x - Unitedkingdom)) %>%
    select(-c("Unitedkingdom")) %>% 
    pivot_longer(-c("Type"), values_to = "Value", names_to = "Country") %>%
    pivot_wider(values_from = Value, names_from = Type) %>%
    mutate(Total = Private + Public) %>% 
    pivot_longer(-c("Country", "Total"), names_to = "Type", values_to = "Value") %>%
    ggplot() + geom_bar(aes(x = Value, y = reorder(Country, Total), fill = Type), stat = "identity", color = "black") +
    scale_fill_manual(values=c("#1E8BC3", "#E85B4E")) + geom_vline(xintercept = 0, size = 1) + scale_shape_identity() + geom_point(aes(x = Total, y = Country), color = "black", shape = 73, size = 8.4, position = position_nudge(y = 0.015)) + 
    labs(y = "Country", x = "Mean Build Rate Above Britain")
  }

AltGross <- Combined %>%
     filter(Date > as.Date("1954-01-01") & Date < ("1980-01-01")) %>%
     AltTenuregraph() + ggtitle("Gross Building Comparisons by Tenure 1955-1979") + labs(y = "Country", x = element_blank()) + 
     scale_x_continuous(limits = c(-1, 2.1))
AltGross 

ggsave("PostWarGrossAlt.png", plot = AltGross, width=7.5, height=4, dpi=300)

AltGross2 <- Combined %>%
  filter(Date > as.Date("1979-01-01") & Date < ("2016-01-01")) %>%
  AltTenuregraph() + ggtitle("Gross Building Comparisons by Tenure 1980-2004") + 
  scale_x_continuous(limits = c(-1, 2.1))
AltGross2 

ggsave("ModernGrossAlt.png", plot = AltGross2, width=7.5, height=4, dpi=300)

x <- Combined %>% 
  filter(Country %in% c("Netherlands", "Austria", "Switzerland", "Unitedkingdom", "Sweden", "Denmark", "Germany", "Ireland")) %>%
  filter(Date > as.Date("1999-01-01")) %>%
  AltTenuregraph() 

x

GrossCounterfac <- Combined %>% 
  filter(Date > as.Date("1954-01-01") & Date < as.Date("2001-01-01")) %>% 
  filter(Country %in% c("Unitedkingdom", "Denmark",  "Norway", "Austria", "Germany", "Switzerland", "Netherlands", "Finland", "Austria", "Ireland", "Sweden", "Belgium", "France", "West Germany", "United Kingdom")) %>%
  mutate(Period = ifelse(Date > as.Date("1980-01-01"), 2, 1)) %>% 
  group_by(Country, Period) %>% 
  summarise(Meanpriv = mean(Private, na.rm = TRUE), Meanpub = mean(Public, na.rm = TRUE)) %>% 
  mutate(Total = Meanpriv + Meanpub) %>%
  pivot_longer(-c("Country", "Period"), names_to = "Item", values_to = "Value") %>%
  pivot_wider(names_from = Country, values_from = Value) %>% 
  mutate(Britain = Unitedkingdom) %>%
  pivot_longer(-c("Period", "Item", "Britain"), names_to = "Country", values_to = "Value") %>% 
  mutate(Surplus = Value - Britain) %>%
  select(-c("Britain", "Value")) %>% 
  pivot_wider(names_from = Item, values_from = Surplus) %>% 
  pivot_longer(-c("Period", "Country"), names_to = "Item", values_to = "Value") %>% 
  relocate(Item, .after = Period) %>%
  unite("Type", Item:Period) %>% 
  pivot_wider(names_from = Type, values_from = Value) %>% 
  mutate(Private = Meanpriv_2 - Meanpriv_1, Public = Meanpub_2 - Meanpub_1, Total = Total_2 - Total_1) %>% 
  select(c("Country", "Private", "Public", "Total")) %>% 
  pivot_longer(-c("Country", "Total"), names_to = "Item", values_to = "Value") 
  

view(Combined) 

Summary <- function(Data, Date1, Date2) { 
  Date1 <- ensym(Date1) 
  Date2 <- ensym(Date2) 
  
p <- Data  %>%
    filter(Country == "Unitedkingdom") %>% 
    filter(Date > as.Date(as_string(Date1)) & Date < as.Date(as_string(Date2))) %>% 
    mutate(DemRate = Y2 * 100 / `X5 (Estimate)`)  %>% 
    mutate(Basestock = first(`X5 (Estimate)`)) %>% 
    mutate(Endstock = last(`X5 (Estimate)`)) %>%
    mutate(DemRate = na.approx(DemRate, maxgap = 2, na.rm = FALSE)) %>%
    fill(DemRate) %>%
    select(c("Date", "DemRate", "Basestock")) %>% 
    left_join(Combined, by = "Date") %>% 
    filter(Country %in% c("Unitedkingdom", "Austria", "Belgium", "Denmark",  "Norway", "Germany", "Switzerland", "Netherlands", "Finland", "Ireland", "Sweden", "France", "West Germany", "United Kingdom")) %>%
    mutate(NetBuild = ((Total - DemRate )/ 100) + 1) %>% 
    group_by(Country) %>% 
    mutate(Cumustock = cumprod(NetBuild)) %>% 
    mutate(Counterstock = Basestock * Cumustock) %>% 
    mutate(Privbuild = (Private / 100) * Counterstock, Pubbuild = (Public / 100)* Counterstock) %>% 
    summarise(Counterpriv = sum(Privbuild), Counterpub = sum(Pubbuild)) %>% 
    mutate(Countertot = Counterpriv + Counterpub) %>% 
    pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
    pivot_wider(names_from = Country, values_from = Value) %>%
    mutate(Britain = Unitedkingdom) %>% 
    pivot_longer(-c("Item", "Britain"), names_to = "Country", values_to = "Value") %>% 
    mutate(Surplus = (Value - Britain) * 1000, PctSurplus = Surplus / (Britain * 10)) %>% 
    mutate(across(`Value`:`Surplus`, ~ signif(.x, digits = 4))) %>% 
    mutate(PctSurplus = round(PctSurplus, digits = 1)) %>% 
    filter(Country != "Unitedkingdom") %>% 
    select(-c("Britain", "Value")) %>% 
    pivot_longer(-c("Country", "Item"), names_to = "Time", values_to = "Value") %>% 
    relocate(Country, .before = Item) %>%
    unite("Type", (Item:Time)) %>% 
    pivot_wider(names_from = Type, values_from = Value) %>% 
    relocate(Countertot_Surplus:Countertot_PctSurplus, .after = Country) %>%
    arrange(Countertot_Surplus) %>% 
    mutate(PctMadeUp = abs(Counterpub_Surplus) / Counterpriv_Surplus*100 ) %>% 
    mutate(PctMadeUp = round(PctMadeUp, digits = 1))
} 

view(Combined)
Newcomp <- Combined %>% 
           select(-c("...9")) %>% 
           group_by(Country) %>%
           mutate(Popgrowth = X50 - lag(X50), PctPop = (Popgrowth / X50) + 1, PctPop2 = Popgrowth*100 / X50) %>% 
           mutate(PctPop = na.approx(PctPop, maxgap = 2, na.rm = FALSE), PctPop2 = na.approx(PctPop2, maxgap = 2, na.rm = FALSE)) %>% 
           ungroup()

view(Newcomp)

NewSummary <- function(Data, Date1, Date2) { 
  Date1 <- ensym(Date1) 
  Date2 <- ensym(Date2) 
  
  p <- Data  %>%
    filter(Country == "Unitedkingdom") %>% 
    filter(Date > as.Date(as_string(Date1)) & Date < as.Date(as_string(Date2))) %>% 
    mutate(DemRate = Y2 * 100 / `X5 (Estimate)`)  %>% 
    mutate(Basestock = first(`X5 (Estimate)`)) %>% 
    mutate(Endstock = last(`X5 (Estimate)`)) %>% 
    mutate(Basepop = first(`X50`)) %>% 
    mutate(Cumupop = cumprod(PctPop)) %>%
    mutate(DemRate = na.approx(DemRate, maxgap = 2, na.rm = FALSE)) %>% 
    rename("BritSum" = "Sum", "Britpop" = "PctPop2", "BritCumuPop" = "Cumupop") %>%
    fill(DemRate) %>% 
    fill(Basepop) %>%
    select(c("Date", "DemRate", "Basestock", "BritSum", "Basepop", "Britpop", "BritCumuPop")) %>% 
    left_join(Data, by = "Date") %>% 
    filter(Country %in% c("Unitedkingdom", "Austria", "Belgium", "Denmark",  "Norway", "Germany", "Switzerland", "Netherlands", "Finland", "Ireland", "Sweden", "France", "West Germany", "United Kingdom")) %>% 
    mutate(Surp = Total - BritSum) %>% 
    mutate(NewDemRate = Surp*0.5 + DemRate) %>% 
    mutate(NetBuild = ((Total - NewDemRate )/ 100) + 1) %>% 
    group_by(Country) %>% 
    mutate(CountPerCap = first(`PerCapHouse`)) %>%
    mutate(Cumupop = cumprod(PctPop)) %>%
    mutate(Cumustock = cumprod(NetBuild)) %>% 
    mutate(Counterstock = Basestock * Cumustock) %>% 
    mutate(Counterpop = Cumupop * Basepop) %>% 
    mutate(Privbuild = (Private / 100) * Counterstock, Pubbuild = (Public / 100)* Counterstock) %>% 
    mutate(Counterpriv = cumsum(Privbuild), Counterpub = cumsum(Pubbuild)) %>% 
    mutate(Countertot = Counterpriv + Counterpub) 
} 

x <- Newcomp %>% 
     NewSummary("1954-01-01", "2016-01-01") 

view(x) 
view(Combined) 

Sumtown <- x %>% 
           filter(Date == as.Date("2015-01-01")) %>% 
           select(c("Country", "CountPerCap", "Counterpriv", "Counterpub", "Countertot", "Counterstock")) %>% 
           mutate(Counterstock = ifelse(Country == "Unitedkingdom",  28277.656  , Counterstock)) %>%
           pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
           pivot_wider(names_from = Country, values_from = Value) %>% 
           mutate(across(`Austria`:`Switzerland`, ~ .x - Unitedkingdom)) 

view(Sumtown) 

Summarytown <- Sumtown %>% 
               pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>% 
               pivot_wider(names_from = "Item", values_from = "Value")

view(Summarytown)

Ratiotown <- x %>% 
  filter(Date == as.Date("2015-01-01")) %>% 
  select(c("Country", "Counterstock", "Counterpriv", "Counterpub", "Countertot")) %>% 
  mutate(Privratio = Counterpriv * 100 / Countertot, Pubratio  = Counterpub *100 / Countertot) %>% 
  select(c("Country", "Privratio", "Pubratio"))

view(Ratiotown)

Pcttown <- Sumtown %>% 
  mutate(across(`Austria`:`Switzerland`, ~ .x*100 / Unitedkingdom)) %>% 
  select(-c("Unitedkingdom")) %>%
  pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>% 
  pivot_wider(names_from = "Item" ,values_from = "Value") %>%
  select(-c("Counterpriv", "Counterpub"))  %>% 
  rename("StockPct" = "Counterstock", "TotalPct" = "Countertot", "CountPerCapPct" = "CountPerCap")
  
view(Pcttown) 

Merged <- Summarytown %>% 
          full_join(Pcttown, by = c("Country" = "Country")) %>% 
          full_join(Ratiotown, by = c("Country" = "Country")) %>% 
          mutate(Britpop = 65.12) 
          
view(Merged) 

Tab1 <- Merged %>% 
        filter(Country != "Ireland") %>%
        select(c("Country", "CountPerCap", "Countertot", "Counterpriv", "Counterpub", "TotalPct", "Privratio", "Pubratio", "Britpop", "Counterstock")) %>% 
        relocate(Countertot, .after = Country) %>% 
        relocate(TotalPct, .after = Countertot) %>% 
        mutate(PerCap = (Counterstock / Britpop) + CountPerCap + 434.2392) %>%
        mutate(PerCap = ifelse(Country == "Unitedkingdom", 434.2392, PerCap))
        
view(Tab1) 

Tabe1 <- Tab1 %>% 
         filter(Country != "Unitedkingdom") %>% 
         arrange(Countertot)

Tab1Sum <- Tabe1 %>% 
           summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(Country = "Western European Average") 

view(Tab1Sum)

Brit1 <- Tab1 %>% 
         filter(Country == "Unitedkingdom") %>% 
         rbind(Tabe1) %>% 
         rbind(Tab1Sum) %>% 
         mutate(across(where(is.numeric), ~ round(.x, digits = 1))) 
         
view(Brit1) 

Britx <- Tab1 %>% 
  filter(Country == "Unitedkingdom") %>% 
  rbind(Tabe1) %>% 
  rbind(Tab1Sum)

SumPerCap <- Britx %>% 
             select(c("Country", "PerCap")) %>% 
             rename("CounterfacPerCap2015" = "PerCap")

view(SumPerCap) 

Join <- Britx %>% 
        select("Country", "CountPerCap") 

view(Join)

Town1980 <- x %>% 
            filter(Date == as.Date("1980-01-01")) %>%
            select(c("Counterstock")) %>%
            mutate(Counterstock = ifelse(Country == "Unitedkingdom", 21517.000, Counterstock)) %>% 
            mutate(CounterfacPerCap1980 = Counterstock / 56.310) %>% 
            filter(Country != "Ireland") %>% 
            full_join(Join, by = c("Country" = "Country")) %>% 
            mutate(CounterfacPerCap1980 = CounterfacPerCap1980 + CountPerCap) %>% 
            mutate(CounterfacPerCap1980 = ifelse(Country == "Unitedkingdom", 382.1169, CounterfacPerCap1980)) %>% 
            filter(Country != "Western European Average")

view(Town1980)
view(Combined) 

Sum1980 <- Town1980 %>% 
  filter(Country != "Unitedkingdom") %>%
  ungroup() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(Country = "Western European Average") 

view(Sum1980) 

Comp <- Town1980 %>% 
            rbind(Sum1980) %>% 
            select(c("CounterfacPerCap1980")) %>% 
            full_join(SumPerCap, by = c("Country" = "Country")) %>% 
            select(-c("CountPerCap"))

view(Comp) 

PerCapx <- Combined %>% 
           filter(Date == as.Date("1955-01-01") | Date == as.Date("1980-01-01") | Date == as.Date("2015-01-01")) %>% 
           select(c("Country", "Date", "PerCapHouse")) %>% 
           pivot_wider(names_from = "Date", values_from = "PerCapHouse") %>%
           filter(Country != "Greece" & Country != "Italy" & Country != "Portugal" & Country != "Spain" & Country != "Ireland") %>% 
           rename("1955Real" = "1955-01-01", "1980Real" = "1980-01-01", "2015Real" = "2015-01-01")  
           
view(PerCapx)  

PerCapAvg <- PerCapx %>% 
             filter(Country != "Unitedkingdom") %>%
  ungroup() %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(Country = "Western European Average")  
             
view(PerCapAvg)  

PerCapy <- PerCapx %>% 
           rbind(PerCapAvg) %>% 
           full_join(Comp, by = c("Country" = "Country")) %>% 
           relocate(CounterfacPerCap1980, .after = `1955Real`) %>% 
           relocate(CounterfacPerCap2015, .after = `1980Real`) %>%
           mutate(across(where(is.numeric), ~ round(.x, digits = 1))) %>% 
           arrange(`2015Real`)

view(PerCapy) 



Tab2Sum <- Merged %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
  mutate(Country = "Western European Average") 
  
view(Tab2Sum) 

Tab2 <- Merged %>% 
        filter(Country != "Unitedkingdom") %>% 
        arrange(AltStock) %>% 
  rbind(Tab2Sum) %>% 
  relocate(Country, .before = CountPerCap) 
  
view(Tab2) 

Brit <- Merged %>% 
  filter(Country == "Unitedkingdom") %>% 
  rbind(Tab2) %>% 
  select(-c("Britpop")) %>% 
  mutate(across(where(is.numeric), ~ round(.x, digits = 1))) %>%
  select(c("Country","AltStock", "Pubratio", "Privratio", "CountPerCapPct", "AltPriv", "AltPub")) %>% 
  relocate(CountPerCapPct:AltPub, .after = AltStock)
  
view(Brit)

view(Tab2Sum) 

setwd("C:/Users/S.Watling/Documents/Counterfactuals") 

write.csv(PerCapy, "PerCapAnt.csv")
write.csv(Brit1, "Total1.csv")
write.csv(Brit, "Total2.csv")

Tab2 <- Merged %>% 
  select(-c("Counterstock", "StockPct")) %>% 
  relocate(Countertot, .after = Country) %>% 
  filter(Country != "Ireland")

view(Merge1)
          

Currentyear <- Combined %>% 
               filter(Date == as.Date("2015-01-01")) 
               
view(Currentyear)

uptown <- x %>% 
          

Newsummaryz <- function(Data, Date1) 
  
summaryz <- function(Data, Date1) { 
  Date1 <- ensym(Date1) 

p <- Data %>%
     filter(Date == as.Date(as_string(Date1))) %>% 
     select(c("Country", "Counterpriv", "Counterpub", "Countertot")) %>%
  pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
  pivot_wider(names_from = Country, values_from = Value) %>%
  pivot_longer(-c("Item", "Unitedkingdom"), names_to = "Country", values_to = "Value") %>% 
  mutate(Surplus = (Value - Unitedkingdom)) %>% 
  mutate(PctSurplus = Surplus * 100 / Unitedkingdom) %>% 
  mutate(across(`Value`:`Surplus`, ~ signif(.x, digits = 4))) %>% 
  mutate(PctSurplus = round(PctSurplus, digits = 1)) %>% 
  select(-c("Unitedkingdom", "Value")) %>% 
  pivot_longer(-c("Item", "Country"), names_to = "Time", values_to = "Value") %>% 
  relocate(Country, Item) %>% 
  unite("New", Item:Time) %>% 
  pivot_wider(names_from = New, values_from = Value) %>% 
  arrange(Countertot_PctSurplus) %>% 
  mutate(PctMadeUp = (-1 * Counterpub_Surplus)*100 / Counterpriv_Surplus) %>% 
  mutate(PctMadeUp = round(PctMadeUp, digits = 1)) %>% 
  select(-c("Counterpriv_PctSurplus", "Counterpub_PctSurplus")) %>% 
  mutate(across(`Counterpriv_Surplus`:`Countertot_Surplus`, ~ .x * 1000))
  }

y <- x %>% 
     summaryz("1979-01-01") 
     
view(y) 

q <- x %>% 
     summaryz("2015-01-01") %>%
     select(c("Country", "Countertot_Surplus")) %>% 
     rename("1955-2015 Surplus"= "Countertot_Surplus") %>% 
     left_join(y, by = c("Country" = "Country")) %>% 
     select(c("Country", "1955-2015 Surplus", "Countertot_Surplus")) %>% 
     filter(Country != "Ireland") %>% 
     mutate(`1980-2015 Surplus` =  `1955-2015 Surplus` - Countertot_Surplus) %>% 
     rename("1955-1979 Surplus" = "Countertot_Surplus") %>%
     mutate(`Pct PostWar` = `1955-1979 Surplus`*100 / `1955-2015 Surplus`) %>% 
     mutate(`Pct PostWar` = round(`Pct PostWar`, digits = 1)) %>%
     arrange(desc(`Pct PostWar`))

view(q) 

j <- x %>% 
     summaryz("2015-01-01") %>% 
     filter(Country != "Ireland")
     
view(j) 

jsum <- j %>% 
        summarise(Priv = mean(Counterpriv_Surplus), Pub = mean(Counterpub_Surplus), tot = mean(Countertot_Surplus)) %>% 
        mutate(Ratio = -1 *Pub * 100 / Priv) %>% 
        mutate(Pct = 100 * 8252800 / (6348000/0.41))
   
view(jsum)

Britxzq <- Combined %>% 
           filter(Country == "Unitedkingdom") %>% 
           filter(Date > as.Date("1979-01-01") & Date < as.Date("2016-01-01")) %>% 
           summarise(sum(Y7))

view(Britxzq)

h <- x %>% 
  summaryz("2015-01-01") %>% 
  left_join(y, by = c("Country" = "Country")) %>% 
  select(-c("PctMadeUp.y", "PctMadeUp.x", "Countertot_PctSurplus.y", "Countertot_PctSurplus.x")) %>% 
  mutate(`Private Surplus` = Counterpriv_Surplus.x - Counterpriv_Surplus.y, 
         `Public Surplus` = Counterpub_Surplus.x - Counterpub_Surplus.y, 
         `Total Surplus` = Countertot_Surplus.x - Countertot_Surplus.y) %>% 
  select(c("Country", "Private Surplus", "Public Surplus", "Total Surplus")) %>% 
  mutate(PctSurplus = -100 * `Public Surplus` / `Private Surplus`) %>% 
  mutate(PctTot = `Total Surplus` / 69190.27) %>% 
  mutate(across(PctSurplus:PctTot, ~ round(.x, digits = 1))) %>% 
  filter(Country != "Ireland") %>% 
  arrange(`Total Surplus`)

view(h)

setwd("C:/Users/S.Watling/Documents/Counterfactuals") 

write.csv(y, "Continuous Adjusted.csv") 
write.csv(q, "Continuous Adjusted2.csv") 
write.csv(j, "Continuous AdjustedTot.csv")
write.csv(h, "Continuous Adjusted1980-2015.csv")

WestAvg <- q %>% 
           summarise(across(`1955-2015 Surplus`:`1980-2015 Surplus`, ~ mean(.x))) %>% 
           mutate(`Pct PostWar` = `1955-1979 Surplus`*100 / `1955-2015 Surplus`)

view(WestAvg)

g <- Combined %>% 
     Summary("1954-01-01", "1980-01-01") 

view(g)

write.csv(g, "Postwarcounter.csv")

g2 <- Combined %>% 
      Summary("1979-01-01", "2005-01-01") 

write.csv(g2, "Postwarcounter2.csv")
view(g2) 

g3 <- Combined %>% 
      Summary("1979-01-01", "2015-01-01") 

view(g3) 

write.csv(g3, "Postwarcounter") 

g4 <- Combined %>% 
      Summary("1954-01-01", "2016-01-01") 

view(g4)  

AltSummary <- function(Data, Date1, Date2) { 
  Date1 <- ensym(Date1) 
  Date2 <- ensym(Date2)
  
 p <- Data  %>%
  filter(Country == "Unitedkingdom") %>% 
  filter(Date > as.Date(as_string(Date1)) & Date < as.Date(as_string(Date2))) %>% 
  mutate(DemRate = Y2 * 100 / `X5 (Estimate)`)  %>% 
  mutate(Basestock = first(`X5 (Estimate)`)) %>% 
  mutate(Endstock = last(`X5 (Estimate)`)) %>%
  mutate(DemRate = na.approx(DemRate, maxgap = 2, na.rm = FALSE)) %>%
  fill(DemRate) %>%
  select(c("Date", "DemRate", "Basestock")) %>% 
  left_join(Combined, by = "Date") %>% 
  filter(Country %in% c("Unitedkingdom", "Austria", "Belgium", "Denmark",  "Norway", "Germany", "Switzerland", "Netherlands", "Finland", "Ireland", "Sweden", "France", "West Germany", "United Kingdom")) %>%
  mutate(NetBuild = ((Total - DemRate )/ 100) + 1) %>% 
  mutate(Privbuild = (Private / 100) * Basestock, Pubbuild = (Public / 100)* Basestock) %>% 
  group_by(Country) %>% 
   summarise(Counterpriv = sum(Privbuild), Counterpub = sum(Pubbuild)) %>% 
   mutate(Countertot = Counterpriv + Counterpub) %>% 
   pivot_longer(-c("Country"), names_to = "Item", values_to = "Value") %>% 
   pivot_wider(names_from = Country, values_from = Value) %>%
   mutate(Britain = Unitedkingdom) %>% 
   pivot_longer(-c("Item", "Britain"), names_to = "Country", values_to = "Value") %>% 
   mutate(Surplus = (Value - Britain) * 1000, PctSurplus = Surplus / (Britain * 10)) %>% 
   mutate(across(`Value`:`Surplus`, ~ signif(.x, digits = 4))) %>% 
   mutate(PctSurplus = round(PctSurplus, digits = 1)) %>% 
   filter(Country != "Unitedkingdom") %>% 
   select(-c("Britain", "Value")) %>% 
   pivot_longer(-c("Country", "Item"), names_to = "Time", values_to = "Value") %>% 
   relocate(Country, .before = Item) %>%
   unite("Type", (Item:Time)) %>% 
   pivot_wider(names_from = Type, values_from = Value) %>% 
   relocate(Countertot_Surplus:Countertot_PctSurplus, .after = Country) %>%
   arrange(Countertot_Surplus) %>% 
   mutate(PctMadeUp = abs(Counterpub_Surplus) / Counterpriv_Surplus*100 ) %>% 
   mutate(PctMadeUp = round(PctMadeUp, digits = 1))
 } 

Lowerbound <- Combined %>% 
              AltSummary("1954-01-01", "1980-01-01") 

view(Lowerbound) 

Lowerbound2 <- Combined %>% 
               AltSummary("1979-01-01", "2016-01-01") 

view(Lowerbound2) 

setwd("C:/Users/S.Watling/Documents/Counterfactuals")
write.csv(Lowerbound, "Lowerbound.csv") 
write.csv(Lowerbound2, "Lowerbound2.csv")

Compare <- g2 %>% 
           left_join(g, by = ("Country")) %>% 
           mutate(Total = Countertot_Surplus.x + Countertot_Surplus.y) %>% 
           mutate(PctPostwar = Countertot_Surplus.y * 100 / Total) %>% 
           select(c("Country", "PctPostwar", "Countertot_Surplus.x", "Countertot_Surplus.y", "Total")) %>% 
           mutate(PctPostwar = round(PctPostwar, digits = 1)) %>% 
           mutate(PctStock = Total / (28277.656 * 10) ) %>% 
           mutate(PctStock2015 = Countertot_Surplus.x / (28277.656 * 10)) %>% 
           mutate(PctStockPostWar = Countertot_Surplus.y / (21314.000 * 10)) %>%
           mutate(across(PctStock: PctStockPostWar, ~ round(.x, digits = 1))) %>%
           arrange(desc(PctStockPostWar)) 

view(Compare) 
view(Combined)

write.csv(Compare, "Comparison1.csv")

Compare2 <- g3 %>% 
  left_join(g, by = ("Country")) %>% 
  mutate(Total = Countertot_Surplus.x + Countertot_Surplus.y) %>% 
  mutate(PctPostwar = Countertot_Surplus.y * 100 / Total) %>% 
  select(c("Country", "PctPostwar", "Countertot_Surplus.x", "Countertot_Surplus.y", "Total")) %>% 
  mutate(PctPostwar = round(PctPostwar, digits = 1)) %>% 
  mutate(PctStock = Total / (28277.656 * 10) ) %>% 
  mutate(PctStock2015 = Countertot_Surplus.x / (28277.656 * 10)) %>% 
  mutate(PctStockPostWar = Countertot_Surplus.y / (21314.000 * 10)) %>%
  mutate(across(PctStock: PctStockPostWar, ~ round(.x, digits = 1))) %>%
  arrange(desc(PctStock2015)) 

view(Compare2)
write.csv(Compare2, "SecondComparison.csv") 

CompareAlt <- Lowerbound %>% 
              left_join(Lowerbound2, by = c("Country")) %>% 
              select(c("Country", "Countertot_Surplus.x", "Countertot_Surplus.y")) %>% 
              mutate(Total = Countertot_Surplus.x + Countertot_Surplus.y) %>% 
              mutate(PctPostwar = Countertot_Surplus.x * 100 / Total) %>% 
              filter(Country != "Ireland") %>% 
              arrange(desc(PctPostwar))

EuropeAvg <- CompareAlt %>% 
  summarise(Countertot_Surplus.x = mean(Countertot_Surplus.x), Countertot_Surplus.y = mean(Countertot_Surplus.y), Total = mean(Total)) %>% 
  mutate(PctPostwar = Countertot_Surplus.x * 100 / Total) %>% 
  mutate(Country = "European Average") %>%
  rbind(CompareAlt) %>% 
  mutate(PctPostwar = round(PctPostwar, digits = 1))

view(EuropeAvg) 

write.csv(EuropeAvg, "LowerBoundSum.csv")

view(CompareAlt)

EuropeAvg <- Compare2 %>% 
             filter(Country != "Ireland") %>%
             summarise(PostWar = mean(Countertot_Surplus.y), Post80 = mean(Countertot_Surplus.x), Total = mean(Total)) %>% 
             mutate(PctPostwar = PostWar * 100 / Total, Country = "European Average")

view(EuropeAvg) 

Initial <- Combined %>% 
           filter(Date == as.Date("1955-01-01")) %>% 
           select(c("Country", "PerCapHouse")) %>% 
           filter(Country != "Spain" & Country != "Portugal" & Country != "Italy" & Country != "Greece" & Country != "Ireland") %>% 
           mutate(Europe = ifelse(Country == "Unitedkingdom", "UK", "Europe")) %>% 
           group_by(Europe) %>% 
           summarise(PerCap = mean(PerCapHouse)) %>% 
           mutate(Counter = PerCap * 50.970) 

view(Initial)
view(Combined)

Compare3 <- g4 %>% 
  select(c("Country", "Countertot_Surplus")) %>%
  mutate(Countertot_Pct = Countertot_Surplus / (28277.656 * 10)) %>% 
  mutate(Countertot_Pct = round(Countertot_Pct, digits = 1)) %>%
  arrange(desc(Countertot_Pct))
 
view(Compare3) 
write.csv(Compare3, "Thirdcomparison.csv")

Sumtenure <- Combined %>% 
             filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>%
             select(c("Country", "Date", "Public", "Private")) %>% 
             group_by(Country) %>% 
             mutate(Private.mean = mean(Private), Public.mean = mean(Public)) %>% 
             filter(Private == max(Private) | Date == as.Date("1979-01-01")) %>% 
             arrange(Private.mean) 
             
view(Sumtenure)     

view(Combined)

Pubtenure <- Combined %>% 
             filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>% 
             group_by(Country) %>% 
             summarise(Ratio = mean(`1`), Total = mean(Public)) %>% 
             filter(Country %in% c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Ireland", "Netherlands", "Norway", "Sweden", "Switzerland", "Unitedkingdom")) %>% 
             arrange(desc(Ratio)) %>% 
             mutate(Ratio = 100 * Ratio) %>% 
             mutate(across(`Ratio`:`Total`, ~ signif(.x, digits = 3))) 


view(Pubtenure) 

write.csv(Pubtenure, "Pubtenure.csv")

FinalCounterFac <- Combined %>% 
                   filter(Date > as.Date("1954-01-01") & Date < as.Date("2016-01-01")) %>%
                   mutate(Period = ifelse(Date < ("1980-01-01"), "PostWar", "Modern")) %>% 
                   filter(Country %in% c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Netherlands", "Norway", "Sweden", "Switzerland", "Unitedkingdom")) %>% 
                   group_by(Country, Period) %>% 
                   summarise(MeanTotal = mean(Total)) %>% 
                   pivot_wider(names_from = Country, values_from = MeanTotal) %>% 
                   mutate(across(Austria:Switzerland, ~ .x - Unitedkingdom)) %>% 
                   select(-c("Unitedkingdom")) %>% 
                   pivot_longer(-c("Period"), names_to = "Country", values_to = "Value") %>%  
                   mutate(Value = round(Value, digits = 2)) %>%
                   pivot_wider(names_from = Period, values_from = Value) %>% 
                   summarise(Postwar = mean(PostWar), Modern = mean(Modern))
                   
view(FinalCounterFac) 

setwd("C:/Users/S.Watling/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning/Graphics")
Alt <- Combined %>% 
       filter(Country %in% c("Ireland", "Sweden", "Unitedkingdom", "Switzerland", "Finland")) %>% 
       mutate(Country = gsub("Unitedkingdom", "United Kingdom", Country)) %>%
       filter(Date > as.Date("1969-01-01") & Date < as.Date("2021-01-01")) %>% 
       mutate(PerCap = `X5 (Alternative)` / X50) %>% 
       mutate(PerCap = na.approx(PerCap, maxgap = 5, na.rm = FALSE)) %>%
       ggplot(aes(x = Date, y = PerCap, color = Country)) + geom_line(size = 1.2) + 
       scale_y_continuous(limits = c(250, 600)) + 
       geom_vline(xintercept = as.numeric(as.Date("1970-01-01")), size = 1) + 
       geom_hline(yintercept = 250, size = 1) + labs(title = "Homes Per Person in Ireland and Europe from 1970-2020", y = "Homes Per 1,000 People", x = "Year") + 
       scale_color_manual(values = c("#F39207", "#078E51", "#274F9E", "#CF5D9F", "#E6223F")) + 
       theme(panel.background = element_blank(),plot.title = element_text(size = 10)) + scale_x_date(breaks = as.Date(c("1970-01-01", "1980-01-01","1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01")),
                                                                labels = c(1970, 1980, 1990, 2000, 2010, 2020))
   
Alt

ggsave("Figure 11.png", plot = Alt, width=7.5, height=4, dpi=300)
ggsave("Figure 11.eps", plot = Alt, width=7.5, height=4, dpi=300)

FinComp <- Combined %>% 
             filter(Country == "Finland") %>% 
             filter(Date > as.Date("1959-01-01") & Date < as.Date("1985-01-01")) %>% 
             summarise(Total = mean(Total))

view(FinComp)

IreComp <- Combined %>% 
           filter(Country == "Ireland") %>% 
           filter(Date > as.Date("1982-01-01") & Date < as.Date("2008-01-01")) %>% 
           summarise(Total = mean(Total))

view(IreComp) 

NethComp <- Combined %>% 
  filter(Country == "Netherlands") %>% 
  filter(Date > as.Date("1954-01-01") & Date < as.Date("1980-01-01")) %>% 
  summarise(Total = mean(Total))

view(NethComp) 


