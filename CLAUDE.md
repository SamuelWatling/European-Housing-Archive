# Claude Context: European Housing Archive

Code and data for Watling & Breach (2023), *The Housebuilding Crisis*, Centre for Cities.
This repo is an **archive of a finished 2022 project**, not live work. The value in it is the
counterfactual method and the assembled European panel; the 2022 working code is mostly
unrunnable and is best read as documentation.

*Written 10 September 2026 after a full audit of the repo, the Dropbox data and the report PDF.*

---

## 0. State of play — read this first

| Thing | Status |
|---|---|
| `Counterfactual Replication.R` | **Runs.** Reproduces published Table 3 totals exactly |
| `Summary Replication.R` | **New, 10 Sep 2026.** Runs. Reproduces Table 2 and the report's quoted statistics |
| `European-Housing-Working-Code` (6 theme files, from 13 scripts) | **Cannot run and never will** — 54 of 60 input files are gone |
| Published tenure split (Table 3) | **Narrowed to one step, not yet closed** — §7 |
| Historic England & Wales data | **Found 10 Sep 2026** in `Documents/Historical English Statistics` — §9 |
| 2022 working code in version control | **Yes.** Committed as `c3bebd6` in `European-Housing-Working-Code`, then grouped by theme — §10.6 |

The two replication scripts at the repo root are the whole live surface. Everything else is
provenance.

---

## 1. Where things live

| What | Path |
|---|---|
| This repo | `C:\Users\samue\Documents\GitHub\European-Housing-Archive` |
| The 2022 working scripts | `C:\Users\samue\Documents\GitHub\European-Housing-Working-Code` — own repo since 10 Sep 2026 |
| Data | `C:\Users\samue\Dropbox\Processed European Data` |
| The report PDF | `Dropbox\Processed European Data\The-housebuilding-crisis-February-2023.pdf` |
| Historic E&W source workbooks | `C:\Users\samue\Documents\Historical English Statistics` — §9 |
| Duplicate data folder, **diverged** | `Dropbox\Eurpean` (note the typo) — §10 |
| The technical annex | `Processed European Data\Methodology-...pdf` — §9c |
| Related but separate project | `Dropbox\English Housing Article` — has its own data, some of it better |
| Stray copy of the counterfactual | `Dropbox\European Replication Project` — byte-identical to the repo's |
| Graph output from 2022 | `Dropbox\European Graphs` |

**Consolidated 10 September 2026.** Everything needed now lives in `Processed European Data`:
the historic England & Wales files, the Nationwide house price workbook, the Bank of England
quarterly series and full millennium dataset, the Irish housing statistics, and the methodology
annex. They were **copied, not moved** — `Works in Progress`, `English Housing Article` and
`Historical English Statistics` are other projects and still need their own copies.

Data is readable — these folders are local, not Dropbox online-only placeholders. That is worth
checking after any Smart Sync change, because a placeholder read fails with `Input/output error`
and `read_csv` swallows it, returning a 0-row tibble rather than an error.

---

## 1a. Repository layout — restructured 10 September 2026

The archive is now **self-contained**: clone it, run either script, nothing else needed.

```
code/     Counterfactual Replication.R, Summary Replication.R -- both run
data/     the four files those scripts read, 400 KB total
output/   figures and tables, created on run, gitignored
docs/     methodology.tex -- the technical annex, typeset in LaTeX (§9c)
```

**`data/` holds only what the code reads.** Everything else — the UN source scans, Holmans, the
full Bank of England dataset, the report and methodology PDFs, the 1950s and 1990s intermediates —
is research material, not replication input, and stays in `Dropbox\Processed European Data`. That
was a deliberate call: an earlier version of this restructure pulled all 36 MB in, of which 26 MB
was a single public dataset with two columns in use.

Two extracts were made rather than carrying their parents:

| extract | replaces | why |
|---|---|---|
| `UK Price and Wage Data 1952-2016.csv` (12 KB) | `UK_house_price_since_1952.xlsx` + `Quarterly Index.csv` | the BoE parent is 26 MB and publicly re-downloadable; two of its columns are used |
| `England and Wales Housebuilding 1856-2019.csv` (27 KB) | Holmans + `HistoricEnglandandWales.csv` | the build rates are already derived in it |

The 2022 working scripts moved out to their own repository, `European-Housing-Working-Code`, so
this package contains only code that runs. There are **thirteen** of them, not fourteen — the
earlier count included a `.Rhistory`.

**Paths.** Neither script uses `setwd()`. Both resolve `ROOT` as `..` or `.` depending on whether
they are run from `code/` or the repo root. The original set `setwd()` twice, at lines 1 and 11,
which is why output from it landed in the data folder rather than beside it.


## 2. The report

66 pages, 12 figures, 7 tables, 8 boxes. The parts that matter for replication:

- **Box 8** (p46) — the counterfactual methodology, in prose
- **Table 3** (p44) — the missing-homes results, 13 rows
- **Table 2** (p28) — private housebuilding 1955-79, 12 countries x 4 statistics
- **Tables 5-7** (p55-57) — appendix, historic England & Wales and UK rates

The report refers to a **separate technical annex** with the full methodology. It is not in this
repo and was not found on the machine. It is the most likely home of the tenure-split step that
§7 cannot reproduce.

---

## 3. What runs

### `Counterfactual Replication.R` (340 lines) — verified 9 Sep, rewritten 10 Sep 2026

**Reorganised and documented 10 September 2026, arithmetic untouched.** The stage headings now
follow the annex's Stages 1-7, each explaining what is being controlled for and why; the duplicated
library block, both `setwd()` calls and every `view()` went; assertions against all 13 published
totals were added. Verified: output identical to the 2022 script, cell for cell, including the
tenure-ratio strings.

Reads `data/Combined.csv`, writes `output/Table 3 - missing homes 1955-2015.csv`. Reproduces the saved `Export3.csv` byte for byte, and
its totals match published Table 3 on all 13 rows: UK 12,230,000; Western European Average
4,254,000 — the "4.3 million missing homes" headline.

**It also runs from `Replication Data full.xlsx`** with a rename of 7 columns (§4). Doing so
changes 3 of 13 rows by 1,000 on ~8,000,000 — one unit in the last significant figure, caused
entirely by the workbook storing the stock estimate rounded to the nearest whole. Tenure ratios
are unaffected. **So the published workbook is sufficient to reproduce the projection.**

### `Summary Replication.R` (528 lines) — written 10 Sep 2026

The README promised this file from the start; it never existed. Rebuilt from
`Replication Data full.xlsx` alone. See §8.

---

## 4. `Replication Data full.xlsx` — the column dictionary

1,174 rows x 19 columns, 16 countries, 1948-2021. **The same data as `Combined.csv`** under
readable names — every value matches. Two traps:

- **Every number is stored as text.** Coerce with `as.numeric` before anything else or the
  columns silently fail type checks.
- `Date` is POSIXct in the workbook and a `dd/mm/yyyy` string in the CSV.

| workbook | `Combined.csv` | meaning |
|---|---|---|
| Population | `X50` | millions |
| Reported Housing Stock | `Y5` | thousands, **as reported** — sparse, often census-only |
| Gross Building | `Y7` | thousands completed per year |
| Demolitions | `Y2` | thousands per year, very sparse |
| Housing Stock (Estimate) | `X5 (Estimate)` | thousands, **modelled** — §6 |
| Housing Stock (Alternative) | `X5 (Alternative)` | the same, smoothed for charts — §5 |
| Public Housing Ratio | `` `1` `` | public share of gross building |
| Private Housing Ratio | `` `4` `` | private share of gross building |
| PerCapHouse | `PerCapHouse` | dwellings per 1,000 people |
| Publicbuild / Privatebuild | — | ratio x gross building |
| Public / Private / Total | — | rates, % of stock per year |

`Combined.csv` columns `2`, `3`, `5`, `6`, `7` are further UN tenure categories. **Nothing uses
them** and the workbook omits them. The counterfactual uses only 7 columns in total.

Greece, Italy, Spain and Portugal are in the data and excluded from every published figure.

---

## 5. Two stock series, and which to use where

**Sam's rule, stated 10 Sep 2026: stated housing stocks for the analysis, always, unless there is
a serious reason otherwise. The smoothed series is for plotting levels.**

National statistical offices change their measurement criteria from time to time, which puts steps
into the stated series that are artefacts of definition rather than of building. They look wrong on
a chart, so the published **figures** use `Housing Stock (Alternative)`; everything else uses
`Housing Stock (Estimate)`.

The two differ in **114 of 1,025 rows across 8 countries**: Ireland 1970-2015 (12.1% max),
Belgium 1948-62 (10.1%), Germany 1959-71 (6.7%), Sweden 1991-93, France 1988-98, Denmark 1980-98,
Netherlands 2012-20, and one UK year, 1989.

**It matters, and getting it wrong is silent.** On the stated series West Germany passes British
homes per person in **1963**; on the smoothed series in **1967-68**, which is what the report says
on p20 and what Figure 5 plots. `Summary Replication.R` asserts the crossing year for exactly this
reason.

---

## 6. Where `Housing Stock (Estimate)` comes from

The reported stock is sparse. The estimate fills the gaps by rolling the stock forward on net
completions and distributing the residual drift across each gap, so the series passes exactly
through every reported observation.

**The only surviving record of this is `90s Data Cleaning.R` lines 292-317** (working-code commit
`c3bebd6`; now `2 European panel.R` lines 325-350). If
that file is ever lost, the provenance of the denominator of every rate in the report goes with it.
It is now also reimplemented, with commentary, in `Summary Replication.R`.

Steps, within country and in date order: back-fill the first reported stock; interpolate a
demolitions/building ratio (demolitions being far sparser than building) and fill it; net
completions = gross x (1 - ratio); cumulate; roll forward from the first observation; take the
difference against reported stock where reported; interpolate that difference across gaps up to 35
years; add it back. A fallback series covers years no observation anchors.

**The rebuild was verified against the stored column: 14 of 15 countries agree to within 0.006%.**

### Belgium — resolved by the annex, 10 September 2026

The rebuild originally disagreed with the stored series for Belgium by up to 264k (10%). The
methodology annex (§9c) settles it, naming Belgium as the **only** country where the "no data
before" rule applies:

> *"The only country this applies to from before 1955 is Belgium, which gives a housing stock value
> of 3.2 million in 1963... This gives a value of 310,000 net additions from 1955 onwards, which
> implies a housing stock of approximately 2.9 million in 1955."*

So the series is rolled **backwards from 1963** and Belgium's reported 1948 figure is deliberately
not an anchor — reasonably, since completions over 1948-62 sum to ~613k while the reported stock
rises by only 348k, so the two figures are not counting the same thing.

`Summary Replication.R` now carries a one-row `STOCK_EXCLUSIONS` table dropping Belgium 1948, with
the annex quoted as justification. **With that, the rebuild agrees with the published estimate for
all 15 countries to within 0.006%** — Belgium included, at 0.002% — and Belgium's Table 2 average
returns to the published 1.51.

The lesson worth keeping: the rebuild was right about the arithmetic and wrong about the data. A
reported observation that contradicts the roll-forward is a candidate for exclusion, not proof the
method is broken.

---

## 7. The published tenure split — narrowed to one step

**Table 3's totals reproduce exactly. Its private/public split does not**, and the differences
cancel on every row, so it is a pure re-split rather than a different projection.

| | published Table 3 | `Counterfactual Replication.R` |
|---|---|---|
| UK | 7,875,000 / 4,358,000 | 8,054,000 / 4,179,000 |
| tenure mix | 64:36 | 66:34 |

**The annex rule is already implemented.** §9c gives it: the population-adjustment discrepancy is
allocated *"in the same tenure ratio as the total tenure ratio of the net additions"*, i.e.

    J_Private Additions = J_Net Private Building - (J_Private Building / J_Total Building) x J_Population Adjustments

and the script's `AdjCumuPriv = NewCumupriv + PrivRatio * DiffNum` is algebraically that same
expression. So the rule is not the problem.

**The problem is which ratio.** Both splits divide the same total of 12,233,000:

| ratio | value | gives UK private |
|---|---|---|
| script: stock-weighted cumulative building | 0.65841 | 8,054,000 |
| implied by published Table 3 | 0.64380 | 7,875,000 |
| **plain cumulative gross building, `sum(Privatebuild) / sum(Gross Building)`, 1955-2015** | **0.64442** | 7,881,000 |

The script weights each year's tenure ratio by the counterfactual stock path; the published table
appears to use the unweighted cumulative building ratio. That closes 1.46pp of a 1.46pp gap to a
residue of **0.06pp**, which is the remaining unknown — plausibly a slightly different year window,
or the stock-estimate rounding.

**One further clue, unexplained.** Published Switzerland and Belgium both show public =
**-4,358,000**, exactly minus the UK's published public figure, meaning those counterfactuals have
*zero* public additions. The script produces -3,822,000 for Switzerland, i.e. 357,000 of public
building rather than none. There may be a floor at zero that the script lacks.

## 8. `Summary Replication.R`

Reads **only** `Replication Data full.xlsx`. No `setwd`; one `DATA_DIR` at the top that detects
Windows or WSL. Rebuilds the stock estimate from reported stock rather than taking it on trust
(§6), checks the rebuild against the stored column, then derives rates and draws the figures.

**Assertions, so it fails loudly rather than drifting:**

- Table 2 in full — 12 countries x average / maximum / year of maximum / 1979 rate. Exact.
- The Western European Average row — 1.72 / 2.20 / 1973 / 1.55.
- p18 public rates: Netherlands 1.43, Sweden 0.96, UK 0.90.
- p19: UK homes per person +26.09% 1955-79 (published 26%).
- p38: UK gross rate 1.86% then 0.79% (published 1.9 and 0.8).
- p20: West Germany crosses UK homes per person in 1968 — the §5 tripwire.

**The average row caught a real error.** The published Western European Average is the peak *of the
averaged series* (2.20 in 1973), not the average of the per-country peaks (2.45 in 1967). Different
question, different answer.

**Produces figures 2, 3, 4, 5, 7, 8, 10, 11, 12**, plus Table 2, the rebuild check and the derived
panel, into `Summary Replication Output/` under the data folder. House style is not reproduced —
the Centre for Cities palette and fonts are brand assets and are not in this archive.

`ggpattern` is genuinely needed for Figure 10's hatched "reduction since 1955-79" segments and is
**not installed**. The script falls back to solid fills.

---

## 9. Historic England & Wales — found 10 September 2026

`Summary Replication.R` cannot draw **Figure 1** (English and Welsh housebuilding, 1856-2019) from
the European workbook. The data for it does exist, in two places, and neither is referenced by any
surviving script:

| File | What |
|---|---|
| `Documents\Historical English Statistics\HistoricEnglandandWales.csv` | 163 rows, 1856-2019, with all the intermediate estimation columns. **Three identical copies** (here, `Dropbox\Eurpean`, `Dropbox\Processed European Data`) |
| `Dropbox\English Housing Article\Data\England and Wales Housing Data from 1856.csv` | **The clean version** — Date, tenure splits, demolitions, stock estimate, and build rates already derived |

`Documents\Historical English Statistics` also holds `Holmans Historical Statistics of British
Housing.xlsx` (1 MB) and an Irish housing market workbook. It is **not** the
`Historical Housing Statistics` folder `Domestic Britain Code.R` wants: the seven CSVs that script
reads (`Households.csv`, `Pre-War-Completions.csv`, the tenure files, `Demolitions.csv`) are gone,
and were evidently hand-extracted from Holmans.

### It reproduces Table 6, mostly

From the clean file, average build rates by decade against published Table 6 (p56):

**Seven of ten decades reproduce exactly** — 1920s, 1930s, 1960s, 1980s, 1990s, 2000s, 2010s.

| Decade | rebuilt (T/Pr/Pu) | published | note |
|---|---|---|---|
| 1940s | 0.62 / 0.14 / 0.49 | 1.16 / 0.26 / 0.90 | **war years excluded in the published row.** 1945-49 gives 1.14 / 0.21 / 0.93 — close, exact window unresolved |
| 1950s | 1.83 / 0.62 / 1.21 | 1.81 / 0.62 / 1.20 | 0.01-0.02 out, unexplained |
| 1970s | 1.52 / 0.84 / 0.68 | 1.55 / 0.86 / 0.69 | 0.02-0.03 out, unexplained |

**Figure 1 and Table 6 are now in `Summary Replication.R`** (10 Sep 2026). The script asserts the
seven clean decades and records the three that differ in a comment rather than fudging them.

---

## 9a. Figure 9's price and wage data — found 10 September 2026

Both inputs to Figure 9 exist, under different names, in
`Dropbox\Works in Progress\Data`:

| `Domestic Britain Code.R` wants | actually on disk |
|---|---|
| `UK_House_Price_Since_1952.csv` | `UK_house_price_since_1952.xlsx` — Nationwide UK HPI, quarterly from 1952 Q4, All / New / Modern / Older houses, index + price + annual change |
| `Wage Price Data.csv` | `Quarterly Index.csv` — the Bank of England "Q1. Quarterly Headline Series" sheet, exported. Year in column 1, quarter in column 2, then a Wages and Prices block containing the spliced CPI and the **spliced Average Weekly Earnings series, 1919-2015** |

**The identification is certain, not a guess.** `Domestic Britain Code.R` lines 385-397 (working-code
commit `c3bebd6`; now `5 England and Wales.R` lines 422-434) divide
`CPI Index` by **6.57** and `Earnings` by **0.925**; the 1960 Q1 row of `Quarterly Index.csv` reads
`... 6.57 ... 9.25 ...` in exactly those columns. They are 1960 Q1 rebasings, which is what the
chart's "Log of Real 1960 Values = 1" axis means.

The same folder also holds **`PriceWageGraph.png`**, the raw ggplot output behind Figure 9 --
"English House Prices and Wages at 1975 Prices", real house prices in red against real wages in
blue, both indexed to 1960 -- and `BadPriceWageGraph.png`, presumably an earlier attempt. So that
folder is where the Figure 9 work was actually done.

`a-millennium-of-macroeconomic-data-for-the-uk.xlsx` (the full BoE dataset, 109 sheets) is there
too, so the quarterly export can be regenerated if `Quarterly Index.csv` is ever lost.

**Figure 9 is now in `Summary Replication.R`** (10 Sep 2026), indexed to 1960 Q1 = 1, and the shape
matches `PriceWageGraph.png` exactly -- tracking wages until 1971, spikes in 1973, 1980 and 1989,
sharp divergence from 2000. Real house prices end at 6.09x their 1960 level against 3.55x for wages.

**One trap.** The year in `Quarterly Index.csv` is written only against Q1 -- merged cells in the
original sheet -- so without a `fill()` you keep one row per year instead of four and the join
collapses to a quarter of the data without erroring. The 2022 script had that `fill()`; leaving it
out is how this was first got wrong. The assertion on quarter count is what caught it.


## 9b. Capital formation, and Figure 6 — 10 September 2026

**The capital formation data is now a second sheet in `Replication Data full.xlsx`**, named
`Capital Formation`: 941 rows, 33 countries, 1950-1988. The original
`Replication Data full` sheet is untouched — still 1,174 x 19 — so nothing built on it changes.
A backup of the pre-change workbook is at `Replication Data full.xlsx.backup-20260910`.

Assembled from `Complete West Capital Formation.csv` (23 countries) and
`Eastern European Capital Formation.csv` (10), which come from the **UN Annual Bulletin table 28**;
the raw OCR scan is `InvestScan.xlsx`. Seven series, and the `Out.of` column is what distinguishes
them — three are shares of GDP, two of total GFCF, two of construction GFCF:

| Item | series |
|---|---|
| A | GFCF total (% GDP) |
| B | GFCF construction (% GDP) |
| C | **GFCF residential (% GDP)** — this is Figure 6 |
| D | Construction (% of total GFCF) |
| E | Residential (% of total GFCF) |
| F | Residential (% of construction GFCF) |
| G | Non-residential (% of construction GFCF) |

Items `Y` and `Z` exist with one row each and no description; dropped.

### OCR decimal-point errors

The source has **decimal points in the wrong place** — Austria 1968 reads 57.4 for 5.74, Portugal
1982 reads 909 for 9.09. Confirmed by Sam, 10 Sep 2026. Repaired by dividing by 10 until the value
is plausible: **11 corrections, all in item C.** Same class of damage, and the same repair shape, as
`recover_thousands()` in the English housing build.

**A single threshold across items would have destroyed good data**, and my first attempt did exactly
that: total gross fixed capital formation really is 20-40% of GDP and a share of GFCF really is
50-60%, so a `> 15` ceiling flagged hundreds of correct values as broken. Ceilings are now per item,
and **repair is applied to item C only** — the one series where the error is confirmed. The other
six are **flagged, not altered**: 49 values exceed their ceiling, including Denmark 1957 and Germany
1963 reading 174 and 232 for total GFCF, which are plainly the same fault. They are left alone
because nobody has verified them.

### Figure 6

Reproduced in `Summary Replication.R` from the new sheet: residential GFCF as a share of GDP,
averaged 1955-1979. Coverage is uneven and the report says so in its own footnote — Switzerland
stops in 1969, Austria has ten years. The published rank order comes out right, Switzerland highest
and the UK lowest at 3.28% against 6.30%, and the script asserts both ends. The middle is within
reading error of a bar chart and the report publishes no numeric table for it.

---

## 9c. The technical annex — found 10 September 2026

`Methodology-The-housebuilding-crisis-February-2023.pdf`, 19 pages, now in
`Processed European Data`. This is the annex the report keeps pointing at, and **it contains both
of the things this archive could not otherwise explain.**

**It resolves §7.** The counterfactual's population adjustment creates a discrepancy between the
total net change in stock and the sum of gross building and demolitions, which compounds into
millions of homes over 1955-2015. The annex says the adjustment is applied so that *"these
adjustments occur in the same tenure ratio as the total tenure ratio of the net additions to the
housing stock"*. **That is the step `Counterfactual Replication.R` is missing** — which is exactly
why its totals match published Table 3 and its private/public split does not.

It also has an **Interpolation** section, which should independently document §6.

The equations are images in the PDF and do not extract as text. **`docs/methodology.tex` is a
corrected edition** (10 Sep 2026) -- read that rather than the PDF. Its equations and worked figures
were rewritten to match `Counterfactual Replication.R` and the stock rebuild in `Summary Replication.R`;
the Stage 7 formulas reproduce all 13 rows of `Export3.csv`. The published annex had a wrong Stage 4
demolition formula, wrong worked inputs, and a tenure split that is not the code's; the .tex's final
section lists every change. **Published Table 3's UK split (7,875,000 / 4,358,000) is total demolitions
split by the cumulative gross private share** (`BritRef` in the 2022 working code), not the year-by-year
split this script uses. The published country rows are not yet reconciled.


## 10. Hazards

1. ~~**`Dropbox\Eurpean` is a diverged duplicate.**~~ **Resolved 10 Sep 2026.** It was not
   redundant -- it uniquely held `1950HouseDataIV.csv` (one of the six surviving script inputs),
   `Rental Prices.csv`, and **`Scanned UN data/`: seven UN source workbooks, 1957-1988
   (*Housing and Construction Statistics for Europe*)** -- the primary source for the whole panel.
   All copied into `Processed European Data`, which now contains every item in `Eurpean` bar an
   empty folder. Of the two files that differed, `Processed European Data` holds the **corrected**
   copy: `CompleteData1958-1991.csv` differs by a single value, West Germany's 1963 floor space,
   which reads `4.2` in `Eurpean` against `74.2` in `Processed` -- and the series runs 73.5, 74.2,
   76.3. `Eurpean` is superseded; nothing was deleted.
2. **`setwd()` at line 1 of both replication scripts.** They point at
   `C:/Users/samue/Documents/Processed European Data`, which does not exist — the data is in
   Dropbox. The practical effect: anything the script writes lands in the *data* folder rather than
   where it was run. `Summary Replication.R` uses no `setwd` for this reason.
3. **`xts` masks `dplyr::first` and `last`** in `Counterfactual Replication.R` and prints alarming
   warnings. Tested: forcing `dplyr::first` changes nothing. Console noise, not a bug.
4. **The workbook's numbers are text.** Coerce first.
5. **`Combined.csv` and `CompleteData1958-1991.csv` also exist under
   `Dropbox\English Housing Article\Data` and `Dropbox\Works in Progress\Data`.** More copies, more
   chances to diverge.
6. ~~**`Project R Code/` IS NOT IN GIT.**~~ **Resolved 10 Sep 2026.** The working scripts are
   committed as `c3bebd6` in `European-Housing-Working-Code`, then grouped verbatim into six theme
   files. `c3bebd6` keeps the thirteen originals, so line citations to them stay valid.

---

## 11. What is lost

**54 of the 60 input files the 2022 scripts reference no longer exist.** Rechecked 10 Sep 2026
against every CSV and Excel file in Dropbox, by name plus a keyword search for renamed copies. The
six survivors are `Combined.csv`, `CompleteData1958-1991.csv`, `1950HouseDataIV.csv`,
`1950HouseDataV.csv`, `1950s Tenure Data.csv` and `Complete West Capital Formation.csv`.

The 2022 working scripts therefore cannot run and cannot be made to. They also point at
eight working directories under `C:/Users/S.Watling/`, a Centre for Cities account that is gone.
**Read them as documentation of method** — which is exactly how §6 was recovered.

Not reproducible from what survives:

| | needs |
|---|---|
| Figure 1, Tables 5-6 | **reproduced**, §9 |
| Figure 6 (residential investment) | **reproduced 10 Sep 2026** — §9b |
| Figure 9 (house prices vs wages) | **reproduced**, §9a |
| Table 1 (dwelling sizes) | **not European data at all.** Taken from a book (Sam, 10 Sep 2026), so there is nothing here to replicate and nothing missing |
| Table 3 tenure split | narrowed to 0.06pp, §7 — the one thing still open |

The **methodology** document the README promises is the annex, now in `Processed European Data` (§9c).

---

## 12. Conventions

- **Do not write Sam's code for him** unless he asks. Diagnose, explain why, name the function —
  he writes the line. Short fragments are fine; corrected blocks are not. See
  `C:\Users\samue\CLAUDE.md`. (He asked explicitly for `Summary Replication.R`; that was scoped.)
- **Assert against published numbers.** This archive's whole risk is silent drift from a report
  that is fixed in print. Every replication script should fail loudly when it stops reproducing.
- **Data stays out of git.** The repo holds `Replication Data full.xlsx` deliberately, as the
  published replication dataset; everything else lives in Dropbox.
- **No `setwd`.** See §10.2.

---

## 13. Next steps

Everything in the report is now reproduced except the Table 3 tenure split.

1. **Close the last 0.06pp on the tenure split** (§7). Try the unweighted cumulative building
   ratio in `Counterfactual Replication.R` in place of the stock-weighted one, and test whether a
   zero floor on counterfactual public additions produces the published -4,358,000 for Switzerland
   and Belgium. Success test: the UK row landing on 7,875,000 / 4,358,000.
2. **Pin down Table 6's 1940s, 1950s and 1970s** (§9). The 1940s is war-year exclusion; the other
   two are out by 0.02-0.03 for no reason yet found.
3. **Read the annex's Stages 1-7** (pp 9-16) against `Counterfactual Replication.R` line by line.
   Two things have already been found this way; there may be more.
4. Install `ggpattern` if Figure 10 needs to match the published styling.
5. Consider moving `Project R Code/` to a `docs/` subfolder so it stops looking runnable.
