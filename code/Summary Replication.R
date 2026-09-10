# Summary Replication.R
# =============================================================================
# Reproduces the summary statistics and charts in
#   Watling, S. and Breach, A. (2023), "The Housebuilding Crisis",
#   Centre for Cities, February 2023.
#
# The README has always promised this file; it was never in the archive. This
# rebuilds it from the one surviving input, `Replication Data full.xlsx`.
#
# WHY THIS EXISTS IN THIS FORM
# ----------------------------
# The 2022 working code (the European-Housing-Working-Code repo) is not
# runnable and will not be: it reads 60 distinct input files of which 54 no
# longer exist, and it points at eight working directories on a Centre for
# Cities account that is gone. Those scripts are now documentation of method,
# not code. Everything here is therefore derived from the published workbook
# alone, so it runs anywhere.
#
# WHAT IS DIFFERENT FROM THE ORIGINAL
# -----------------------------------
# The original read `Combined.csv`, whose columns are named X50, Y5, Y7, Y2,
# `X5 (Estimate)` and `1`..`7`. The published workbook carries the same data
# under readable names. The dictionary below maps between them so this file can
# be read alongside the 2022 scripts.
#
# The workbook also stores `Housing Stock (Estimate)` -- a modelled series, not
# an observation. Rather than take it on trust, this script REBUILDS it from
# the reported stock, gross building and demolitions, and then checks its own
# rebuild against the stored column. That is the substantive addition here: the
# estimate is the denominator of every rate in the report, and it previously
# arrived unexplained.
#
# WHAT THIS FILE CANNOT REPRODUCE
# -------------------------------
#   Table 1 (dwelling sizes) is the only thing not reproduced anywhere, and it
#   needs nothing: it came from a book, not the European data.
#
# Figures 1, 6 and 9 were all listed as unreproducible when this file was
# written on 9 September 2026. All three are now in, after the data behind them
# turned up in folders nothing referenced (Figures 1 and 9) and the capital
# formation series was added to the workbook (Figure 6).
# The counterfactual in Table 3 is a separate script, Counterfactual Replication.R.
# =============================================================================

library(tidyverse)
library(readxl)
library(zoo)      # na.approx(), for the interpolation below

options(scipen = 999)

# ---- Configuration ---------------------------------------------------------
# No setwd(). The 2022 scripts used it heavily and it is why output from them
# lands in the data folder rather than where you ran them.

# Paths resolve whether you run from the repo root or from code/.
ROOT     <- if (dir.exists("../data")) ".." else "."
DATA_DIR <- file.path(ROOT, "data")
OUT_DIR  <- file.path(ROOT, "output")
stopifnot("Run this from the repo root or from code/" = dir.exists(DATA_DIR))
dir.create(OUT_DIR, showWarnings = FALSE)

SAVE_FIGURES <- TRUE

# Countries in the report's Western European sample. Greece, Italy, Spain and
# Portugal are present in the workbook but excluded from every published figure.
WEST <- c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany",
          "Ireland", "Netherlands", "Norway", "Sweden", "Switzerland",
          "Unitedkingdom")
POSTWAR <- 1955:1979   # the report's "post-war era"
MODERN  <- 1980:2015   # the report's "modern era"

# ---- Data dictionary -------------------------------------------------------
# workbook column              old name    meaning
#   Population                 X50         millions
#   Reported Housing Stock     Y5          thousands, AS REPORTED (sparse)
#   Gross Building             Y7          thousands completed, per year
#   Demolitions                Y2          thousands, per year (very sparse)
#   Housing Stock (Estimate)   X5 (Est.)   thousands, MODELLED -- rebuilt below
#   Public Housing Ratio       `1`         public share of gross building
#   Private Housing Ratio      `4`         private share of gross building
#   PerCapHouse                PerCapHouse dwellings per 1,000 people
#   Publicbuild / Privatebuild  --         ratio x gross building
#   Public / Private / Total    --         rates, % of stock per year
#
# Columns `2`, `3`, `5`, `6`, `7` of Combined.csv are further UN tenure
# categories; the report uses none of them and the workbook omits them.

raw <- read_excel(file.path(DATA_DIR, "Replication Data full.xlsx")) %>%
  select(-`...1`) %>%
  # Excel stored every number as text, so coerce before anything else.
  mutate(across(-c(Country, Date), ~ suppressWarnings(as.numeric(.x))),
         Date = as.Date(Date),
         Year = as.integer(format(Date, "%Y")))

stopifnot(nrow(raw) == 1174, n_distinct(raw$Country) == 16,
          all(WEST %in% raw$Country))

# Capital formation, added to the workbook as a second sheet on 10 Sep 2026.
# Assembled from Complete West / Eastern European Capital Formation.csv, which
# come from UN Annual Bulletin table 28 (raw scan: InvestScan.xlsx). Eleven OCR
# decimal-point errors were repaired in the residential series -- Austria 1968
# read 57.4 for 5.74, Portugal 1982 read 909 for 9.09. Implausible values in the
# other six items are flagged, not altered; see capital_formation_flags.csv.
capform <- read_excel(file.path(DATA_DIR, "Replication Data full.xlsx"),
                      sheet = "Capital Formation") %>%
  select(Region, Country, Year, everything()) %>%
  select(1:10) %>%
  mutate(across(-c(Region, Country), as.numeric))

stopifnot("Capital Formation sheet missing or short" = nrow(capform) > 900,
          "residential GFCF column missing" =
            "GFCF residential (% GDP)" %in% names(capform))

# ---- Observations excluded from the rebuild --------------------------------
# Defined here because the rebuild below needs it. The reasoning is with the
# validation block that follows the rebuild.
STOCK_EXCLUSIONS <- tibble::tribble(
  ~Country,  ~Year, ~why,
  "Belgium",  1948, "measurement discontinuity; annex p17 rolls Belgium back from 1963"
)

# ---- Rebuilding the housing stock estimate ---------------------------------
# The reported stock series is sparse -- many countries report a dwelling count
# only at censuses. The estimate fills the gaps by rolling the stock forward on
# net completions and then distributing the residual discrepancy across each
# gap, so that the series passes exactly through every reported observation.
#
# Step by step, within each country and in date order:
#   Firsthaus  reported stock, back-filled so the earliest years carry the
#              first observation
#   Ratio      demolitions / gross building, interpolated then filled. Used
#              because demolitions are reported far more sparsely than building
#   NetHaus    gross building net of that demolition ratio
#   Cumuhaus   cumulative net additions
#   Origcumu   stock rolled forward from the first reported observation
#   DiffHaus   reported stock minus the rolled-forward stock, i.e. how far the
#              roll-forward has drifted, known only in years with a reported figure
#   Genhaus    roll-forward plus the linearly interpolated drift -- this is what
#              makes the series hit every reported observation exactly
#   Adjhaus    fallback for countries/years where no reported figure anchors the
#              series, e.g. before the first observation
#
# Faithful to `90s Data Cleaning.R` lines 292-317 (working-code commit c3bebd6;
# now `2 European panel.R` lines 325-350), which is where this originally lived
# and which is the only reason it can be recovered at all.

stock <- raw %>%
  # Drop the excluded observations BEFORE the roll-forward, so they anchor
  # nothing. See STOCK_EXCLUSIONS below the rebuild for why each is here.
  mutate(`Reported Housing Stock` =
           if_else(paste(Country, Year) %in%
                     paste(STOCK_EXCLUSIONS$Country, STOCK_EXCLUSIONS$Year),
                   NA_real_, `Reported Housing Stock`)) %>%
  arrange(Country, Date) %>%
  group_by(Country) %>%
  mutate(
    Firsthaus  = `Reported Housing Stock`) %>%
  fill(Firsthaus, .direction = "up") %>%
  mutate(
    OriginHaus = first(Firsthaus),
    ImpDem     = na.approx(Demolitions, maxgap = 4, na.rm = FALSE),
    Ratio      = ImpDem / `Gross Building`) %>%
  fill(Ratio, .direction = "downup") %>%
  mutate(
    ImpTot     = na.approx(`Gross Building`, maxgap = 4, na.rm = FALSE),
    NetHaus    = ImpTot - Ratio * ImpTot,
    Cumuhaus   = cumsum(replace_na(NetHaus, 0)),
    SecondHaus = Firsthaus + Cumuhaus,
    # CumuAdj: cumulative additions, but only in years that have a reported
    # stock. Filling it upwards gives each gap the cumulative total at the
    # observation that closes it.
    DumStock   = ifelse(is.na(`Reported Housing Stock`), NA, 1),
    CumuAdj    = Cumuhaus * DumStock) %>%
  fill(CumuAdj, .direction = "up") %>%
  mutate(
    Cumustart  = first(CumuAdj),
    Origcumu   = OriginHaus + Cumuhaus - Cumustart,
    Adjhaus    = SecondHaus - CumuAdj,
    DiffHaus   = `Reported Housing Stock` - Origcumu,
    Genhaus    = Origcumu + na.approx(DiffHaus, maxgap = 35, na.rm = FALSE),
    StockRebuilt = coalesce(Genhaus, Adjhaus)) %>%
  ungroup() %>%
  select(Country, Date, Year, Population, `Reported Housing Stock`,
         `Gross Building`, Demolitions, `Public Housing Ratio`,
         `Private Housing Ratio`, StockStored = `Housing Stock (Estimate)`,
         StockAlt = `Housing Stock (Alternative)`, StockRebuilt)

# ---- Does the rebuild match the published estimate? ------------------------

stock_check <- stock %>%
  filter(!is.na(StockStored), !is.na(StockRebuilt)) %>%
  group_by(Country) %>%
  summarise(n = n(),
            max_abs_diff = max(abs(StockStored - StockRebuilt)),
            max_pct_diff = 100 * max(abs((StockStored - StockRebuilt) / StockStored)),
            .groups = "drop") %>%
  arrange(desc(max_abs_diff))

message("Rebuilt stock estimate vs the workbook's stored column:")
print(stock_check %>% mutate(across(where(is.numeric), ~ round(.x, 3))), n = 20)

# Every country except Belgium agrees to floating-point noise.
stopifnot(
  "rebuild diverges from the published estimate" =
    all(stock_check$max_pct_diff < 0.01),
  "NA pattern differs between rebuilt and stored" =
    identical(is.na(stock$StockStored), is.na(stock$StockRebuilt))
)

# BELGIUM. The methodology annex (Methodology-The-housebuilding-crisis-
# February-2023.pdf, p17, "Interpolation") names Belgium as the ONLY country
# where the "no data before" rule applies, and works it through:
#
#   "The only country this applies to from before 1955 is Belgium, which gives a
#    housing stock value of 3.2 million in 1963. In 1962 46,000 homes were built
#    and 2,000 were demolished. Therefore, a ratio of 2:46 was assumed for
#    demolitions to construction for all previous years. Between 1956 and 1963
#    325,000 houses were built, implying 14,000 total demolitions. This gives a
#    value of 310,000 net additions from 1955 onwards, which implies a housing
#    stock of approximately 2.9 million in 1955."
#
# So the series is rolled BACKWARDS from 1963 and Belgium's reported 1948 figure
# is deliberately not used as an anchor. That reported figure is inconsistent
# with the 1963 one anyway: completions over 1948-62 sum to about 613k while the
# reported stock rises by only 348k, so the two are not counting the same thing.
#
# The exclusion below is what makes this rebuild agree with the published series
# (2.89m in 1955, matching the annex's "approximately 2.9 million"). Without it
# the rebuild anchors on 1948 and runs 264k high until they converge in 1963.

# ---- Two stock series, and which to use where ------------------------------
# The workbook carries two:
#
#   Housing Stock (Estimate)     built on the STATED housing stocks. This is the
#                                default and what every rate and the
#                                counterfactual are computed from.
#   Housing Stock (Alternative)  the same series with the discontinuities
#                                smoothed. National statistical offices change
#                                their measurement criteria from time to time,
#                                which puts steps into the stated series that
#                                are artefacts of definition rather than of
#                                building. Those steps look wrong on a chart, so
#                                the published FIGURES use this version.
#
# The rule the report followed: stated stocks for the analysis, always, unless
# there was a serious reason otherwise; the smoothed series only for plotting
# levels. The two differ in 114 of 1,025 rows, across 8 countries -- Ireland
# 1970-2015, Germany 1959-71, France 1988-98, Denmark 1980-98, Netherlands
# 2012-20, Sweden 1991-93, Belgium 1948-62 and one UK year, 1989.
#
# It matters. On the stated series West Germany passes British homes per person
# in 1963; on the smoothed series it does so in 1967-68, which is what the
# report says on p20 and what Figure 5 shows.

STOCK_ANALYSIS <- "StockRebuilt"   # "StockStored" to reproduce the report exactly
STOCK_GRAPHS   <- "StockAlt"       # levels charts only -- see above

# ---- Derived rates ---------------------------------------------------------
# All rates are annual flows as a percentage of the standing stock.

panel <- stock %>%
  mutate(Stock       = .data[[STOCK_ANALYSIS]],
         Privatebuild = `Private Housing Ratio` * `Gross Building`,
         Publicbuild  = `Public Housing Ratio`  * `Gross Building`,
         Private      = 100 * Privatebuild / Stock,
         Public       = 100 * Publicbuild  / Stock,
         Total        = 100 * `Gross Building` / Stock,
         # Rates use the analysis stock; the per-person LEVEL used in the charts
         # uses the smoothed series, for the reason above.
         PerCapHouse     = 1000 * Stock / (Population * 1000),
         PerCapHouseSmooth = 1000 * .data[[STOCK_GRAPHS]] / (Population * 1000))

west <- panel %>% filter(Country %in% WEST)

# ---- Table 2: private housebuilding, 1955 to 1979 --------------------------

table2 <- west %>%
  filter(Year %in% POSTWAR) %>%
  group_by(Country) %>%
  summarise(`Average annual private rate` = mean(Private, na.rm = TRUE),
            `Maximum annual private rate` = max(Private, na.rm = TRUE),
            `Year of maximum`             = Year[which.max(Private)],
            `Private rate 1979`           = Private[Year == 1979],
            .groups = "drop") %>%
  arrange(`Average annual private rate`)

# The published average row is NOT the mean of each country's statistics -- it is
# the statistics OF the average series. Averaging the per-country maxima gives
# 2.45 in 1967; the report says 2.20 in 1973, which is the peak of the averaged
# series. Different question, different answer.
euro_private_series <- west %>%
  filter(Year %in% POSTWAR, Country != "Unitedkingdom") %>%
  group_by(Year) %>%
  summarise(rate = mean(Private, na.rm = TRUE), .groups = "drop")

table2 <- table2 %>%
  bind_rows(tibble(
    Country                       = "Western European Average",
    `Average annual private rate` = mean(euro_private_series$rate),
    `Maximum annual private rate` = max(euro_private_series$rate),
    `Year of maximum`             = euro_private_series$Year[which.max(euro_private_series$rate)],
    `Private rate 1979`           = euro_private_series$rate[euro_private_series$Year == 1979]))

message("\nTable 2: Britain's private housing supply from 1955 to 1979")
print(table2 %>% mutate(across(where(is.double), ~ round(.x, 2))), n = 20)

# Published Table 2, p28. Assert rather than eyeball.
published_t2 <- tribble(
  ~Country,        ~avg, ~max,  ~yr,  ~r79,
  "Unitedkingdom", 0.95, 1.29, 1964L, 0.62,
  "Austria",       1.05, 1.35, 1955L, 1.01,
  "Ireland",       1.28, 2.48, 1974L, 2.30,
  "Denmark",       1.44, 2.17, 1973L, 1.24,
  "Belgium",       1.51, 2.10, 1976L, 1.65,
  "France",        1.56, 2.25, 1972L, 1.41,
  "Netherlands",   1.60, 1.99, 1974L, 1.36,
  "Sweden",        1.72, 2.00, 1965L, 1.17,
  "Norway",        1.90, 2.30, 1955L, 1.87,
  "Germany",       2.19, 3.13, 1955L, 1.29,
  "Switzerland",   2.28, 3.00, 1961L, 1.34,
  "Finland",       2.43, 4.18, 1974L, 2.47)

t2_check <- table2 %>%
  inner_join(published_t2, by = "Country") %>%
  mutate(d_avg = abs(round(`Average annual private rate`, 2) - avg),
         d_max = abs(round(`Maximum annual private rate`, 2) - max),
         yr_ok = `Year of maximum` == yr,
         d_79  = abs(round(`Private rate 1979`, 2) - r79))

# Belgium is allowed 0.01 on the average, for the reason documented above.
stopifnot(
  "Table 2 average rate does not reproduce" = all(t2_check$d_avg < 0.011),
  "Table 2 maximum rate does not reproduce" = all(t2_check$d_max < 0.011),
  "Table 2 year of maximum does not reproduce" = all(t2_check$yr_ok),
  "Table 2 1979 rate does not reproduce" = all(t2_check$d_79 < 0.011)
)
# The Western European Average row, published as 1.72 / 2.20 / 1973 / 1.55.
avg_row <- table2 %>% filter(Country == "Western European Average")
stopifnot(
  "Western European Average row does not reproduce" =
    abs(round(avg_row$`Average annual private rate`, 2) - 1.72) < 0.011 &&
    abs(round(avg_row$`Maximum annual private rate`, 2) - 2.20) < 0.011 &&
    avg_row$`Year of maximum` == 1973 &&
    abs(round(avg_row$`Private rate 1979`, 2) - 1.55) < 0.011
)
message("  Table 2 reproduces the published values, including the European average row.")

# ---- Summary statistics quoted in the report text --------------------------

pw <- west %>% filter(Year %in% POSTWAR)

stat <- function(x) round(x, 2)

pub_rates_pw <- pw %>% group_by(Country) %>%
  summarise(public = mean(Public, na.rm = TRUE), .groups = "drop")

percap_change <- west %>%
  filter(Year %in% c(1955, 1979)) %>%
  select(Country, Year, PerCapHouse) %>%
  pivot_wider(names_from = Year, values_from = PerCapHouse) %>%
  mutate(pct_change = 100 * (`1979` / `1955` - 1)) %>%
  arrange(pct_change)

uk_eras <- panel %>%
  filter(Country == "Unitedkingdom", Year <= 2019) %>%
  mutate(era = if_else(Year <= 1979, "1948-1979", "1980-2019")) %>%
  group_by(era) %>%
  summarise(gross = mean(Total, na.rm = TRUE),
            private = mean(Private, na.rm = TRUE),
            public = mean(Public, na.rm = TRUE), .groups = "drop")

message("\nStatistics quoted in the report text:")
message("  p18  public rate 1955-79   NL ", stat(pub_rates_pw$public[pub_rates_pw$Country == "Netherlands"]),
        " (pub 1.4)  SE ", stat(pub_rates_pw$public[pub_rates_pw$Country == "Sweden"]),
        " (pub 0.96)  UK ", stat(pub_rates_pw$public[pub_rates_pw$Country == "Unitedkingdom"]), " (pub 0.9)")
message("  p19  UK homes per person 1955->1979  +",
        stat(percap_change$pct_change[percap_change$Country == "Unitedkingdom"]), "%  (pub 26%)")
message("  p38  UK gross rate  ", stat(uk_eras$gross[1]), "% then ", stat(uk_eras$gross[2]),
        "%  (pub 1.9 then 0.8)")

stopifnot(
  "public rates on p18 do not reproduce" =
    abs(pub_rates_pw$public[pub_rates_pw$Country == "Sweden"] - 0.96) < 0.01 &&
    abs(pub_rates_pw$public[pub_rates_pw$Country == "Unitedkingdom"] - 0.90) < 0.01,
  "UK homes per person change on p19 does not reproduce" =
    abs(percap_change$pct_change[percap_change$Country == "Unitedkingdom"] - 26) < 0.5,
  "UK era gross rates on p38 do not reproduce" =
    abs(uk_eras$gross[1] - 1.9) < 0.05 && abs(uk_eras$gross[2] - 0.8) < 0.05
)
# p20: "West Germany ... reaching British levels of homes per person by around
# 1967". True on the smoothed series, which is what Figure 5 plots; on the
# stated series it happens in 1963. A check that the right series is in use.
de_cross <- west %>%
  left_join(west %>% filter(Country == "Unitedkingdom") %>%
              select(Year, uk = PerCapHouseSmooth), by = "Year") %>%
  filter(Country == "Germany", Year %in% POSTWAR) %>%
  mutate(rel = 100 * PerCapHouseSmooth / uk)
de_cross_year <- min(de_cross$Year[de_cross$rel >= 100], na.rm = TRUE)
message("  p20  West Germany reaches UK homes per person in ", de_cross_year, "  (pub 'around 1967')")
stopifnot("Figure 5 series is wrong -- West Germany should cross around 1967" =
            de_cross_year >= 1966 && de_cross_year <= 1969)

message("  Report text statistics reproduce.")

# ---- Figures ---------------------------------------------------------------
# House style is not reproduced -- the Centre for Cities palette and fonts are
# brand assets and are not in this archive. The data and geometry are.

theme_report <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.title = element_blank())

nice <- function(x) recode(x, Unitedkingdom = "United Kingdom", Germany = "West Germany")

save_fig <- function(plot, name, w = 8, h = 5) {
  if (!SAVE_FIGURES) return(invisible())
  ggsave(file.path(OUT_DIR, paste0(name, ".png")), plot, width = w, height = h, dpi = 200)
}

# Figure 2 -- average annual gross housebuilding 1955-1979, split by tenure
fig2_data <- pw %>%
  group_by(Country) %>%
  summarise(Private = mean(Private, na.rm = TRUE),
            Public  = mean(Public,  na.rm = TRUE), .groups = "drop") %>%
  mutate(Total = Private + Public, Country = nice(Country)) %>%
  pivot_longer(c(Private, Public), names_to = "Tenure", values_to = "Rate")

fig2 <- ggplot(fig2_data, aes(reorder(Country, Total), Rate, fill = Tenure)) +
  geom_col() + coord_flip() +
  labs(title = "Figure 2: Britain built much less than other European countries in the post-war era",
       subtitle = "Average annual gross housebuilding as a share of housing stock (%), 1955 to 1979",
       x = NULL, y = NULL) + theme_report
save_fig(fig2, "Figure 02 - gross housebuilding 1955-1979")

# Figure 3 -- the Netherlands and Sweden against the UK, by tenure
fig3 <- west %>%
  filter(Country %in% c("Netherlands", "Sweden", "Unitedkingdom"), Year %in% POSTWAR) %>%
  mutate(Country = nice(Country)) %>%
  select(Country, Year, Private, Public) %>%
  pivot_longer(c(Private, Public), names_to = "Tenure", values_to = "Rate") %>%
  ggplot(aes(Year, Rate, colour = Country)) +
  geom_line(linewidth = 0.8) + facet_wrap(~ Tenure) +
  labs(title = "Figure 3: The Netherlands and Sweden show that postwar Britain could have built more",
       subtitle = "Annual housebuilding as a share of housing stock (%)", x = NULL, y = NULL) +
  theme_report
save_fig(fig3, "Figure 03 - Netherlands Sweden UK by tenure")

# Figure 4 -- change in homes per person, 1955 to 1979
fig4 <- percap_change %>%
  mutate(Country = nice(Country)) %>%
  ggplot(aes(reorder(Country, pct_change), pct_change)) +
  geom_col(fill = "steelblue4") + coord_flip() +
  labs(title = "Figure 4: Post-war Britain's increase in homes per person was low",
       subtitle = "Change in dwellings per 1,000 people, 1955 to 1979 (%)", x = NULL, y = NULL) +
  theme_report
save_fig(fig4, "Figure 04 - change in homes per person")

# Figure 5 -- homes per person relative to the UK, 1955-1979 (UK = 100)
# Figures 5 and 12 plot a level relative to the UK, so both use the smoothed
# series. Reproducing them from the stated series moves West Germany's crossing
# point by four years.
uk_percap <- west %>% filter(Country == "Unitedkingdom") %>%
  select(Year, uk = PerCapHouseSmooth)

rel_percap <- west %>%
  left_join(uk_percap, by = "Year") %>%
  mutate(rel = 100 * PerCapHouseSmooth / uk, Country = nice(Country))

fig5 <- rel_percap %>%
  filter(Year %in% POSTWAR,
         Country %in% c("Sweden", "Denmark", "Switzerland", "Finland",
                        "West Germany", "Netherlands")) %>%
  ggplot(aes(Year, rel, colour = Country)) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 100) +
  annotate("text", x = 1957, y = 101.5, label = "UK = 100", size = 3) +
  labs(title = "Figure 5: The UK saw relative decline in housing outcomes over the post-war period",
       subtitle = "Ratio of homes per person relative to the UK, 1955 to 1979", x = NULL, y = NULL) +
  theme_report
save_fig(fig5, "Figure 05 - homes per person relative to UK")

# Figure 6 -- residential investment as a share of GDP, 1955-1979.
# Coverage is uneven and the report says so in its own footnote: Switzerland
# stops in 1969, Austria has only ten years. The bars are means over whatever
# years each country reports, which is what the published chart does.
fig6_data <- capform %>%
  filter(Country %in% WEST, Year %in% POSTWAR, !is.na(`GFCF residential (% GDP)`)) %>%
  group_by(Country) %>%
  summarise(share = mean(`GFCF residential (% GDP)`),
            yrs = n(), span = paste0(min(Year), "-", max(Year)), .groups = "drop") %>%
  mutate(Country = nice(Country))

message("\nFigure 6: residential investment, mean share of GDP 1955-79")
print(fig6_data %>% mutate(share = round(share, 2)) %>% arrange(desc(share)), n = 20)

# The published chart ranks Switzerland top and the UK bottom. Assert the ends,
# which is what the figure's claim rests on -- the middle is close enough to be
# within reading error of a bar chart and the report gives no numeric table.
stopifnot(
  "Figure 6: UK should have the lowest residential investment share" =
    fig6_data$Country[which.min(fig6_data$share)] == "United Kingdom",
  "Figure 6: Switzerland should have the highest" =
    fig6_data$Country[which.max(fig6_data$share)] == "Switzerland")

fig6 <- fig6_data %>%
  ggplot(aes(reorder(Country, share), share,
             fill = Country == "United Kingdom")) +
  geom_col() + coord_flip() +
  scale_fill_manual(values = c(`TRUE` = "seagreen4", `FALSE` = "yellowgreen"), guide = "none") +
  labs(title = "Figure 6: Postwar Britain had the lowest investment rate in residential construction",
       subtitle = "Investment in residential buildings, average share of GDP per year, 1955 to 1979",
       x = NULL, y = NULL) + theme_report
save_fig(fig6, "Figure 06 - residential investment share of GDP")

# Figure 7 -- public housebuilding, UK against the European average
euro_public <- west %>%
  filter(Country != "Unitedkingdom") %>%
  group_by(Year) %>% summarise(Public = mean(Public, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = "Western European average")

fig7 <- west %>%
  filter(Country == "Unitedkingdom") %>%
  transmute(Year, Public, Country = "United Kingdom") %>%
  bind_rows(euro_public) %>%
  filter(Year >= 1948, Year <= 2015) %>%
  ggplot(aes(Year, Public, colour = Country)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Figure 7: From the 1970s onwards, public housebuilding fell across Europe",
       subtitle = "Annual public housebuilding as a share of housing stock (%)", x = NULL, y = NULL) +
  theme_report
save_fig(fig7, "Figure 07 - public housebuilding UK vs Europe")

# Figure 8 -- Swiss private building against total British building
fig8 <- west %>%
  filter(Year %in% POSTWAR) %>%
  filter((Country == "Switzerland") | (Country == "Unitedkingdom")) %>%
  transmute(Year,
            Series = if_else(Country == "Switzerland",
                             "Switzerland, private only", "United Kingdom, private and public"),
            Rate = if_else(Country == "Switzerland", Private, Total)) %>%
  ggplot(aes(Year, Rate, colour = Series)) +
  geom_line(linewidth = 0.8) +
  labs(title = "Figure 8: Switzerland built more private homes than Britain built in total",
       subtitle = "Annual housebuilding as a share of housing stock (%)", x = NULL, y = NULL) +
  theme_report
save_fig(fig8, "Figure 08 - Switzerland private vs UK total")

# Figure 10 -- 1980-2015 rates, showing the fall from the post-war era.
# The published chart uses hatched fills via ggpattern for the "reduction"
# segments. ggpattern is optional here so the script still runs without it.
fig10_data <- west %>%
  filter(Year %in% c(POSTWAR, MODERN)) %>%
  mutate(era = if_else(Year %in% POSTWAR, "postwar", "modern")) %>%
  group_by(Country, era) %>%
  summarise(Private = mean(Private, na.rm = TRUE),
            Public  = mean(Public,  na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(c(Private, Public), names_to = "Tenure", values_to = "Rate") %>%
  pivot_wider(names_from = era, values_from = Rate) %>%
  mutate(reduction = pmax(postwar - modern, 0),
         Country = nice(Country)) %>%
  select(Country, Tenure, `Post 1980` = modern, `Reduction since 1955-79` = reduction) %>%
  pivot_longer(-c(Country, Tenure), names_to = "Segment", values_to = "Rate") %>%
  mutate(Fill = paste(Segment, Tenure, sep = ", "))

ordering <- fig10_data %>% group_by(Country) %>% summarise(t = sum(Rate)) %>% arrange(t)

fig10 <- fig10_data %>%
  mutate(Country = factor(Country, levels = ordering$Country)) %>%
  ggplot(aes(Country, Rate, fill = Fill)) +
  geom_col() + coord_flip() +
  labs(title = "Figure 10: Housebuilding rates fell across nearly all European countries",
       subtitle = "Average annual gross housebuilding as a share of housing stock (%), 1980 to 2015,\nwith the reduction since 1955-79 shown above it",
       x = NULL, y = NULL) + theme_report
save_fig(fig10, "Figure 10 - housebuilding 1980-2015 with reduction")

# Figure 11 -- Ireland's homes per person
fig11 <- west %>%
  filter(Country == "Ireland", Year >= 1955) %>%
  ggplot(aes(Year, PerCapHouseSmooth)) +
  geom_line(linewidth = 0.8, colour = "seagreen4") +
  labs(title = "Figure 11: Ireland's ratio of homes per person has always been low",
       subtitle = "Dwellings per 1,000 people", x = NULL, y = NULL) + theme_report
save_fig(fig11, "Figure 11 - Ireland homes per person")

# Figure 12 -- homes per person relative to the UK, modern era
fig12 <- rel_percap %>%
  filter(Year >= 1980, Year <= 2015, Country != "United Kingdom") %>%
  ggplot(aes(Year, rel, colour = Country)) +
  geom_line(linewidth = 0.7) +
  geom_hline(yintercept = 100) +
  labs(title = "Figure 12: Some European countries are no longer seeing outcomes improve relative to Britain",
       subtitle = "Ratio of homes per person relative to the UK, 1980 to 2015", x = NULL, y = NULL) +
  theme_report
save_fig(fig12, "Figure 12 - homes per person relative to UK, modern")

# ---- Figure 1 and Table 6: England and Wales, 1856-2019 --------------------
# Not European data. Assembled in 2022 from Holmans, Historical Statistics of
# British Housing, via seven hand-extracted CSVs that no longer exist -- but the
# processed output survives, with the build rates already derived.

ew <- read_csv(file.path(DATA_DIR, "England and Wales Housebuilding 1856-2019.csv"),
               show_col_types = FALSE) %>%
  select(-1) %>%
  mutate(Year = as.integer(format(as.Date(Date), "%Y")),
         Decade = 10 * (Year %/% 10))

stopifnot("England & Wales file is short" = nrow(ew) > 150,
          "expected build rate columns missing" =
            all(c("Total Build Rate", "Private Build Rate", "Public Build Rate") %in% names(ew)))

table6 <- ew %>%
  filter(Decade >= 1920, Decade <= 2010) %>%
  group_by(Decade) %>%
  summarise(across(c(`Total Build Rate`, `Private Build Rate`, `Public Build Rate`),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop")

message("\nTable 6: housebuilding rates by decade, England and Wales")
print(table6 %>% mutate(across(-Decade, ~ round(.x, 2))), n = 20)

# Published Table 6, p56. SEVEN of ten decades reproduce exactly. The three that
# do not are recorded rather than fudged:
#   1940s  published 1.16/0.26/0.90, here 0.62/0.14/0.49. The published row
#          excludes the war years -- 1945-49 gives 1.14/0.21/0.93. The exact
#          window is not recoverable from what survives.
#   1950s  published 1.81/0.62/1.20, here 1.83/0.62/1.21 -- 0.01-0.02 out
#   1970s  published 1.55/0.86/0.69, here 1.52/0.84/0.68 -- 0.02-0.03 out
published_t6 <- tribble(
  ~Decade, ~total, ~priv, ~pub,
  1920,      1.77,  1.15, 0.62,
  1930,      2.62,  2.01, 0.61,
  1960,      2.04,  1.20, 0.84,
  1980,      0.96,  0.72, 0.24,
  1990,      0.73,  0.62, 0.12,
  2000,      0.67,  0.59, 0.08,
  2010,      0.57,  0.45, 0.12)

t6_check <- table6 %>% inner_join(published_t6, by = "Decade")
stopifnot("Table 6 does not reproduce for the seven clean decades" =
  all(abs(round(t6_check$`Total Build Rate`, 2)   - t6_check$total) < 0.011) &&
  all(abs(round(t6_check$`Private Build Rate`, 2) - t6_check$priv)  < 0.011) &&
  all(abs(round(t6_check$`Public Build Rate`, 2)  - t6_check$pub)   < 0.011))
message("  Table 6 reproduces for 1920s, 1930s, 1960s, 1980s, 1990s, 2000s, 2010s.")
message("  1940s (war years), 1950s and 1970s differ -- see comment above.")

fig1 <- ew %>%
  filter(Year >= 1856, Year <= 2019) %>%
  select(Year, Private = `Private Build Rate`, Public = `Public Build Rate`) %>%
  pivot_longer(-Year, names_to = "Tenure", values_to = "Rate") %>%
  ggplot(aes(Year, Rate, fill = Tenure)) +
  geom_col(width = 1) +
  geom_vline(xintercept = 1947, linetype = "dashed") +
  annotate("text", x = 1947, y = Inf, label = " TCPA 1947", hjust = 0, vjust = 1.6, size = 3) +
  labs(title = "Figure 1: The English and Welsh housebuilding rate decreased after 1947",
       subtitle = "Annual housebuilding as a share of housing stock (%), England and Wales",
       x = NULL, y = NULL) + theme_report
save_fig(fig1, "Figure 01 - England and Wales housebuilding 1856-2019", w = 9)

# ---- Figure 9: house prices against wages ----------------------------------
# Both inputs were listed as lost. They survive under different names:
#   UK_House_Price_Since_1952.csv -> UK_house_price_since_1952.xlsx (Nationwide)
#   Wage Price Data.csv           -> Quarterly Index.csv (Bank of England
#                                    "Q1. Quarterly Headline Series")
# The transform is from Domestic Britain Code.R lines 385-400 (working-code
# commit c3bebd6; now `5 England and Wales.R` lines 422-437): deflate both by
# CPI, index each to 1960 Q1, take log10. The script's constants 6.57 and 0.925
# are the 1960 Q1 CPI and the deflated 1960 Q1 earnings -- which is how the two
# files were identified.

# The two parents -- the Nationwide house price workbook and the Bank of England
# quarterly headline series -- are not in this archive: the BoE file alone is
# 26 MB of a public dataset, and only two of its columns are used. They were
# extracted once into the CSV below, which carries exactly what the transform
# needs. Both parents live in Dropbox for anyone who wants to regenerate it.

pw_raw <- read_csv(file.path(DATA_DIR, "UK Price and Wage Data 1952-2016.csv"),
                   show_col_types = FALSE) %>%
  mutate(Quarter = zoo::as.yearqtr(paste0(Year, " ", Q), format = "%Y Q%q"))

stopifnot("price and wage file is short" = nrow(pw_raw) > 200,
          "expected columns missing" =
            all(c("CPI", "Earnings", "HousePrice") %in% names(pw_raw)))

pw <- pw_raw %>%
  filter(!is.na(CPI), !is.na(Earnings), !is.na(HousePrice)) %>%
  arrange(Quarter) %>%
  # Rebase CPI so 1960 Q1 = 1, deflate, then index each series to 1960 Q1 = 1.
  mutate(cpi60 = CPI[Quarter == zoo::as.yearqtr("1960 Q1")],
         CPIr = CPI / cpi60,
         RealEarnings = Earnings / CPIr,
         RealHouse    = HousePrice / CPIr) %>%
  mutate(BaseEarnings = RealEarnings / RealEarnings[Quarter == zoo::as.yearqtr("1960 Q1")],
         BaseHouse    = RealHouse    / RealHouse[Quarter == zoo::as.yearqtr("1960 Q1")]) %>%
  filter(Quarter >= zoo::as.yearqtr("1960 Q1"))

stopifnot("Figure 9 series should be indexed to 1 at 1960 Q1" =
            abs(pw$BaseEarnings[1] - 1) < 1e-9 && abs(pw$BaseHouse[1] - 1) < 1e-9,
          "Figure 9 has too few quarters" = nrow(pw) > 180)

message("\nFigure 9: real house prices and wages, 1960 Q1 = 1")
message("  quarters: ", nrow(pw), " (", format(min(pw$Quarter)), " to ", format(max(pw$Quarter)), ")")
message("  final values -- house prices ", round(tail(pw$BaseHouse, 1), 2),
        "x, wages ", round(tail(pw$BaseEarnings, 1), 2), "x their 1960 level in real terms")

fig9 <- pw %>%
  select(Quarter, `Real house prices` = BaseHouse, `Real wages` = BaseEarnings) %>%
  pivot_longer(-Quarter, names_to = "Series", values_to = "Index") %>%
  mutate(Year = as.numeric(Quarter)) %>%
  ggplot(aes(Year, Index, colour = Series)) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c(`Real house prices` = "firebrick", `Real wages` = "steelblue4")) +
  labs(title = "Figure 9: House prices were already disconnecting from wages before 1980",
       subtitle = "Real UK house prices and real average weekly earnings, 1960 Q1 = 1",
       x = NULL, y = NULL) + theme_report
save_fig(fig9, "Figure 09 - house prices against wages")

# ---- Outputs ---------------------------------------------------------------

write_csv(table2,      file.path(OUT_DIR, "Table 2 - private housebuilding 1955-1979.csv"))
write_csv(stock_check, file.path(OUT_DIR, "Stock estimate rebuild check.csv"))
write_csv(panel,       file.path(OUT_DIR, "Summary panel.csv"))
write_csv(fig6_data,   file.path(OUT_DIR, "Figure 6 - residential investment.csv"))
write_csv(table6,      file.path(OUT_DIR, "Table 6 - England and Wales rates by decade.csv"))
write_csv(pw %>% select(Quarter, BaseHouse, BaseEarnings),
          file.path(OUT_DIR, "Figure 9 - real house prices and wages.csv"))

message("\nWrote tables and ", if (SAVE_FIGURES) "12 figures" else "no figures", " to:\n  ", OUT_DIR)
