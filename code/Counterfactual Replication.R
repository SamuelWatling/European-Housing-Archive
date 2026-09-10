# Counterfactual Replication.R
# =============================================================================
# Reproduces Table 3 of
#   Watling, S. and Breach, A. (2023), "The Housebuilding Crisis",
#   Centre for Cities -- "The missing homes from underbuilding from 1955-2015".
#
# THE QUESTION
# ------------
# How many homes would Britain have added between 1955 and 2015 if it had built
# at the rate of another European country? Answering it honestly means adjusting
# for the ways those countries differ from Britain other than in how much they
# built -- population growth, initial homes per person, demolitions and tenure
# mix -- because otherwise faster-growing or worse-housed countries look like
# better builders. The stages below do that adjustment.
#
# The headline result is the Western European Average row: Britain would have
# added 4.25 million more homes, 5.86 million more privately and 1.60 million
# fewer socially.
#
# METHOD
# ------
# Report Box 8 states it in prose; the technical annex
# (Methodology-The-housebuilding-crisis-February-2023.pdf) gives the algebra,
# in Stages 1-7. The stage headings below follow the annex.
#
# WHAT IT READS
# -------------
# data/Combined.csv -- the assembled European panel, 1948-2021, 16 countries.
# Its columns carry the UN Bulletin's own codes rather than readable names:
#
#   X50               population, millions
#   Y5                reported housing stock, thousands (sparse)
#   Y7                gross building, thousands per year
#   Y2                demolitions, thousands per year (very sparse)
#   X5 (Estimate)     modelled housing stock -- the denominator of every rate
#   PerCapHouse       dwellings per 1,000 people
#   `1` / `4`         public / private share of gross building
#
# data/Replication Data full.xlsx carries the same data under readable names and
# is what Summary Replication.R uses. Either reproduces Table 3's totals; the
# CSV is kept here because it holds full precision, where the workbook rounds
# the stock estimate and shifts three rows by one unit in the last digit.
#
# A KNOWN DISCREPANCY, NOT YET RESOLVED
# -------------------------------------
# This script reproduces published Table 3's TOTALS exactly, on all 13 rows.
# Its PRIVATE/PUBLIC SPLIT differs -- the UK row comes out 8,054,000/4,179,000
# against the published 7,875,000/4,358,000. The differences cancel on every
# row, so it is a re-split of the same total rather than a different projection.
#
# The cause is narrowed but not closed. Both figures divide the same 12,233,000;
# this script uses a tenure ratio weighted by the counterfactual stock path
# (0.65841), while the plain cumulative building ratio is 0.64442 against the
# 0.64380 the published table implies. A residue of 0.06pp is unexplained.
# Published Switzerland and Belgium also show public additions of exactly
# -4,358,000, i.e. zero counterfactual public building, which this script does
# not produce -- there may be a floor at zero that is missing here.
#
# A REVISED TABLE ALONGSIDE THE PUBLISHED ONE
# -------------------------------------------
# Every comparator gets the British demolition rate, adjusted for the size of
# its counterfactual stock, except the countries that use their own reported
# demolitions. The published table did that for Switzerland only. For Ireland
# the adjusted British rate goes negative in 1978-96, adding about 239,000
# homes, so Table 3 is built twice: exactly as published (asserted below), and
# revised, with Ireland and Sweden also using their own reported demolitions
# (Y2: Ireland 1966-98, Sweden 1954-79 and 1989-2019; gaps filled the same way
# as Switzerland's). Only the Ireland, Sweden and Western European Average rows
# differ, and the script checks that.
#
# The arithmetic of the published run is UNCHANGED from the 2022 original. This
# file was reorganised and documented on 10 September 2026 and its output
# verified identical; resolving the split is a separate change with its own diff.
# =============================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(zoo)
library(xts)

options(scipen = 999)

# xts masks dplyr::first() and last() and warns loudly about lag(). Verified
# harmless here -- forcing dplyr::first() changes no output -- but the warnings
# are alarming and worth knowing are noise.

# ---- Configuration ----------------------------------------------------------
# No setwd(). The original set it twice, at lines 1 and 11, which is why output
# from this script used to land in the data folder rather than beside it.

# Paths resolve whether you run from the repo root or from code/.
ROOT     <- if (dir.exists("../data")) ".." else "."
DATA_DIR <- file.path(ROOT, "data")
OUT_DIR  <- file.path(ROOT, "output")
stopifnot("Run this from the repo root or from code/" = dir.exists(DATA_DIR))
dir.create(OUT_DIR, showWarnings = FALSE)

# Countries that use their own reported demolitions instead of the adjusted
# British rate. The published Table 3 used Switzerland only. The revised table
# adds Ireland, whose adjusted British rate goes negative in 1978-96 and adds
# about 239,000 homes (CLAUDE.md §10.8), and Sweden, which also reports its
# demolitions (CLAUDE.md §10.10).
OWN_DEMOLITIONS_PUBLISHED <- c("Switzerland")
OWN_DEMOLITIONS_REVISED   <- c("Switzerland", "Ireland", "Sweden")

# =============================================================================
# STAGES 1-2: population growth, and building rates by tenure
# =============================================================================
# Each country's population is turned into a cumulative growth index, and its
# gross building is split into private and public and expressed as a share of
# the housing stock. Those shares are the "rates" every counterfactual applies.
#
# The four southern European countries are dropped here, as in the report: the
# UN series for them is too incomplete to carry a 60-year counterfactual.
#
# Note the window opens at 1955 but the data is read from 1948, because
# cumprod() on the population index needs the earlier years to be present and
# then discarded -- filtering first would rebase the index on the wrong year.
Data <- read_csv(file.path(DATA_DIR, "Combined.csv")) %>% 
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
# =============================================================================
# STAGES 3-5: build the counterfactual UK, one per comparator country
# =============================================================================
# The question is: how many homes would Britain have if it had added them at
# another country's rate? Three differences have to be controlled for first,
# because otherwise a country that simply grew faster or started poorer would
# look like it had built more (report, Box 8):
#
#   1. POPULATION GROWTH.  PopRatio rescales the comparator's cumulative
#      population growth by Britain's, so the counterfactual is not credited
#      with building that merely housed a faster-growing population.
#   2. INITIAL HOMES PER PERSON.  EuroBaseStock is Britain's 1955 population at
#      the comparator's 1955 homes-per-person, so a country that started with
#      fewer homes is not credited for catching up from a lower base.
#   3. DEMOLITIONS.  Britain's own demolition rate is applied, since the
#      counterfactual is Britain -- with Britain's stock to maintain.
#
# British demolitions are reported patchily, so DemRatio (demolitions as a share
# of gross building) is interpolated and filled, then multiplied back up by
# gross building to fill the gaps. coalesce() keeps every genuinely reported
# figure and uses the modelled one only where none exists.
#
# first() here takes each group's 1955 value, which is why the filter above had
# to run before this point.
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
# =============================================================================
# STAGES 6-7 AS ONE FUNCTION, so Table 3 can be built for published and revised
# choices of which countries use their own demolitions
# =============================================================================
build_table3 <- function(own_demolitions) {
  # =============================================================================
  # Countries with their own demolitions: handled separately
  # =============================================================================
  # Every other country's demolition rate is the British rate adjusted in Stage 6
  # below via the stock ratio. The countries in `own_demolitions` use their own
  # reported series (Y2) instead, built by interpolating, filling a
  # demolitions-to-building ratio, and multiplying back up -- the same
  # construction used for Britain above. Switzerland in the published table;
  # Switzerland, Ireland and Sweden in the revised one.
  #
  # They are then bound back in and treated identically from Stage 6 onward.
  OwnDem <- Counterfac %>% 
    filter(Country %in% own_demolitions) %>% 
    group_by(Country) %>%
    mutate(NewDemRate = na.approx(Y2, maxgap = 2, na.rm = FALSE)) %>% 
    mutate(OwnDemRatio = NewDemRate / Y7) %>% 
    fill(OwnDemRatio, .direction = "downup") %>% 
    mutate(NewDemRatio = OwnDemRatio * Y7) %>% 
    mutate(NewDemRate  = coalesce(NewDemRate, NewDemRatio)) %>%
    mutate(NewDemRate = NewDemRate / `X5 (Estimate)`) %>% 
    select(-c("OwnDemRatio", "NewDemRatio"))
  OwnBrit <- Counterfac %>% 
    filter(Country == "Unitedkingdom") %>% 
    ungroup() %>%
    select(c("Date", "Counterstock")) %>% 
    rename("BritAlt" = "Counterstock") %>%
    full_join(OwnDem, by = c("Date" = "Date")) %>% 
    mutate(StockRatio = Counterstock / BritAlt) 
  # =============================================================================
  # STAGE 6: adjust demolitions for the size of the counterfactual stock
  # =============================================================================
  # A counterfactual Britain with more homes than the real one would also have
  # demolished more of them. StockRatio measures how much larger the
  # counterfactual stock is, and the demolition rate is scaled by it, so the
  # estimate does not credit the counterfactual with homes it would have lost.
  #
  # This is what makes the estimates conservative rather than generous, and it is
  # also where the running totals for each tenure are formed.
  Counterfac3 <- Counterfac %>% 
    filter(Country == "Unitedkingdom") %>% 
    ungroup() %>%
    select(c("Date", "Counterstock")) %>% 
    rename("BritAlt" = "Counterstock") %>%
    full_join(Counterfac, by = c("Date" = "Date")) %>% 
    filter(!Country %in% own_demolitions) %>%
    mutate(StockRatio = Counterstock / BritAlt) %>% 
    mutate(NewDemRate = DemRate + ((StockRatio - 1) / 100)) %>% 
    rbind(OwnBrit) %>%
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
  # =============================================================================
  # STAGE 7: express every counterfactual as a difference from the actual UK
  # =============================================================================
  # Published Table 3 reports how many MORE homes Britain would have. So each
  # country's 2015 cumulative additions are differenced against the UK's own row,
  # leaving the UK at zero by construction.
  #
  # Cumulative demolitions are netted off each tenure first, so the figures are
  # additions to the stock rather than gross building.
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
  # ---- The UK reference row ---------------------------------------------------
  # The actual UK: 12.23m homes added 1955-2015, split by tenure. Its private and
  # public figures are the total split by the tenure ratio of its own building,
  # after apportioning demolitions across the two tenures in the same proportion.
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
  EurTab3 <- Table3 %>% 
    select(-c("Unitedkingdom")) %>% 
    pivot_wider(names_from = Country, values_from = Value) %>% 
    pivot_longer(-c("Item"), names_to = "Country", values_to = "Value") %>% 
    pivot_wider(names_from = Item, values_from = Value) %>% 
    mutate(DiffNum = NewCounterstock - NewCumuTot) %>% 
    mutate(Country = gsub("Mean", "Western European Average", Country))
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
  # ---- Allocating the population-adjustment discrepancy -----------------------
  # Controlling for population growth leaves a gap between the total net change in
  # stock and the sum of gross building and demolitions, and compounding turns it
  # into millions of homes over sixty years. The methodology annex splits it
  # across tenures "in the same tenure ratio as the total tenure ratio of the net
  # additions to the housing stock":
  #
  #     Private additions = net private building - (private share) x discrepancy
  #
  # which is what `AdjCumuPriv = NewCumupriv + PrivRatio * DiffNum` computes,
  # DiffNum being the negative of that discrepancy.
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
  WestEurope3 <- NewTot3 %>% 
    filter(Country == "Western European Average")
  # ---- Output -----------------------------------------------------------------
  # Rounded to four significant figures, matching the published table.
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
  Export3
}

# ---- The published table, and the revised one -------------------------------
Export3         <- build_table3(OWN_DEMOLITIONS_PUBLISHED)
Export3_revised <- build_table3(OWN_DEMOLITIONS_REVISED)

# ---- Verification against the published table --------------------------------
# Published Table 3, p44. The totals must reproduce exactly; the tenure split is
# checked separately and is the known discrepancy described in the header.

published_totals <- c(
  `United Kingdom`           = 12230000, Switzerland = 1647000, Sweden  = 2137000,
  Denmark                    =  2445000, Belgium     = 2795000, Netherlands = 2836000,
  Norway                     =  3349000, Germany     = 3835000, France  = 5393000,
  Austria                    =  7007000, Ireland     = 7076000, Finland = 8276000,
  `Western European Average` =  4254000)

check <- Export3 %>%
  transmute(Country,
            total = `Calculated Total Additions`,
            published = published_totals[Country],
            ok = abs(total - published) <= 1000)

message("Table 3 totals against the published figures:")
print(as.data.frame(check), row.names = FALSE)

stopifnot(
  "Table 3 does not reproduce -- every published total should match" = all(check$ok),
  "Table 3 should have 13 rows" = nrow(Export3) == 13
)
message("  All 13 published totals reproduce.")

# ---- The revised table ------------------------------------------------------
# The revision may only move the rows it touches: the countries added to the
# own-demolitions list, and the Western European Average built from them.
revised_rows <- c(setdiff(OWN_DEMOLITIONS_REVISED, OWN_DEMOLITIONS_PUBLISHED),
                  "Western European Average")
untouched <- function(tab) tab %>% filter(!Country %in% revised_rows) %>% arrange(Country)
stopifnot("The revised table changed rows it should not touch" =
            isTRUE(all.equal(as.data.frame(untouched(Export3)),
                             as.data.frame(untouched(Export3_revised)))))

tidy_rows <- function(tab, label) tab %>%
  filter(Country %in% revised_rows) %>%
  select(Country, Private = `Calculated Private Additions`,
         Public = `Calculated Public Additions`, Total = `Calculated Total Additions`) %>%
  mutate(Table = label)
revision <- bind_rows(tidy_rows(Export3, "published"), tidy_rows(Export3_revised, "revised")) %>%
  arrange(Country, Table)
message("Revised table -- Ireland and Sweden use their own reported demolitions:")
print(as.data.frame(revision), row.names = FALSE)

write_csv(Export3, file.path(OUT_DIR, "Table 3 - missing homes 1955-2015.csv"))
message("Wrote ", file.path(OUT_DIR, "Table 3 - missing homes 1955-2015.csv"))

write_csv(Export3_revised, file.path(OUT_DIR, "Table 3 revised - own demolitions.csv"))
message("Wrote ", file.path(OUT_DIR, "Table 3 revised - own demolitions.csv"))
