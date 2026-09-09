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
| `Project R Code/` (14 scripts) | **Cannot run and never will** — 54 of 60 input files are gone |
| Published tenure split (Table 3) | **Does not reproduce.** Totals match, private/public does not — §7 |
| Historic England & Wales data | **Found 10 Sep 2026** in `Documents/Historical English Statistics` — §9 |
| `Project R Code/` in version control | **No.** 14 scripts, untracked, one copy on one disk — §10.6 |

The two replication scripts at the repo root are the whole live surface. Everything else is
provenance.

---

## 1. Where things live

| What | Path |
|---|---|
| This repo | `C:\Users\samue\Documents\GitHub\European-Housing-Archive` |
| Data | `C:\Users\samue\Dropbox\Processed European Data` |
| The report PDF | `Dropbox\Processed European Data\The-housebuilding-crisis-February-2023.pdf` |
| Historic E&W source workbooks | `C:\Users\samue\Documents\Historical English Statistics` — §9 |
| Duplicate data folder, **diverged** | `Dropbox\Eurpean` (note the typo) — §10 |
| Related but separate project | `Dropbox\English Housing Article` — has its own data, some of it better |
| Stray copy of the counterfactual | `Dropbox\European Replication Project` — byte-identical to the repo's |
| Graph output from 2022 | `Dropbox\European Graphs` |

Data is readable — these folders are local, not Dropbox online-only placeholders. That is worth
checking after any Smart Sync change, because a placeholder read fails with `Input/output error`
and `read_csv` swallows it, returning a 0-row tibble rather than an error.

---

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

### `Counterfactual Replication.R` (203 lines) — verified 9 Sep 2026

Reads `Combined.csv`, writes `Export3.csv`. Reproduces the saved `Export3.csv` byte for byte, and
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

**The only surviving record of this is `Project R Code/90s Data Cleaning.R` lines 292-317.** If
that file is ever lost, the provenance of the denominator of every rate in the report goes with it.
It is now also reimplemented, with commentary, in `Summary Replication.R`.

Steps, within country and in date order: back-fill the first reported stock; interpolate a
demolitions/building ratio (demolitions being far sparser than building) and fill it; net
completions = gross x (1 - ratio); cumulate; roll forward from the first observation; take the
difference against reported stock where reported; interpolate that difference across gaps up to 35
years; add it back. A fallback series covers years no observation anchors.

**The rebuild was verified against the stored column: 14 of 15 countries agree to within 0.006%.**

### Belgium is the exception, and it is a measurement break

Belgium reports a stock for 1948 (2,888k) and then nothing until 1963 (3,236k). Completions over
1948-62 sum to about 613k while the reported stock rises by only 348k — the two Belgian figures are
not counting the same thing. The workbook's estimate rolls 1963 backwards and **disregards the 1948
figure**; a faithful rebuild honours it and starts 264k higher, converging by 1963.

Worth one basis point on Belgium's average private rate (1.51 published, 1.50 rebuilt) and nothing
else. `Summary Replication.R` flags it rather than patching, because patching means silently
discarding a reported observation — but given §5, this is probably a deliberate discontinuity
exclusion and could be made explicit instead.

---

## 7. The published tenure split does not reproduce

**Table 3's totals reproduce exactly. Its private/public split does not.**

| | published Table 3 | `Counterfactual Replication.R` |
|---|---|---|
| UK | 7,875,000 / 4,358,000 | 8,054,000 / 4,179,000 |
| Western European Average | 5,859,255 / -1,604,855 | 5,301,000 / -1,047,000 |
| tenure mix | 64:36 -> **80:20** | 66:34 -> **81:19** |

The differences **cancel exactly** on every row — it is a pure re-split between the two tenure
columns, not a different projection. Ten of thirteen published tenure ratios still match.

One clue: published Switzerland and Belgium both show public = **-4,358,000**, precisely the
negative of the UK's published public figure, i.e. clamped to zero public building. The archived
script produces no such clamp. **The version that generated Table 3 had a tenure step this one does
not**, most likely documented in the missing technical annex.

Unresolved. Anyone rebuilding the counterfactual should decide whether to reproduce the published
split or to publish the script's own and explain the difference.

---

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

So Figure 1 and Tables 5-6 are recoverable, with a small unresolved discrepancy worth pinning down
before they are republished.

---

## 9a. Figure 9's price and wage data — found 10 September 2026

Both inputs to Figure 9 exist, under different names, in
`Dropbox\Works in Progress\Data`:

| `Domestic Britain Code.R` wants | actually on disk |
|---|---|
| `UK_House_Price_Since_1952.csv` | `UK_house_price_since_1952.xlsx` — Nationwide UK HPI, quarterly from 1952 Q4, All / New / Modern / Older houses, index + price + annual change |
| `Wage Price Data.csv` | `Quarterly Index.csv` — the Bank of England "Q1. Quarterly Headline Series" sheet, exported. Year in column 1, quarter in column 2, then a Wages and Prices block containing the spliced CPI and the **spliced Average Weekly Earnings series, 1919-2015** |

**The identification is certain, not a guess.** `Domestic Britain Code.R` lines 385-397 divide
`CPI Index` by **6.57** and `Earnings` by **0.925**; the 1960 Q1 row of `Quarterly Index.csv` reads
`... 6.57 ... 9.25 ...` in exactly those columns. They are 1960 Q1 rebasings, which is what the
chart's "Log of Real 1960 Values = 1" axis means.

The same folder also holds **`PriceWageGraph.png`**, the raw ggplot output behind Figure 9 --
"English House Prices and Wages at 1975 Prices", real house prices in red against real wages in
blue, both indexed to 1960 -- and `BadPriceWageGraph.png`, presumably an earlier attempt. So that
folder is where the Figure 9 work was actually done.

`a-millennium-of-macroeconomic-data-for-the-uk.xlsx` (the full BoE dataset, 109 sheets) is there
too, so the quarterly export can be regenerated if `Quarterly Index.csv` is ever lost.

**Consequence:** Figure 9 is reproducible. Note the source paths are outside this repo and outside
`Processed European Data` -- a fourth data location for this project.


## 10. Hazards

1. **`Dropbox\Eurpean` (typo) is a diverged duplicate.** Of four files compared,
   `CompleteData1958-1991.csv` and `Complete West Capital Formation.csv` **differ** from the
   `Processed European Data` copies; `Combined.csv` and `1950s Tenure Data.csv` are identical.
   Neither folder is authoritative. Resolve before trusting either.
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
6. **`Project R Code/` IS NOT IN GIT.** Only three files are tracked:
   `Counterfactual Replication.R`, `README.md` and `Replication Data full.xlsx`. All 14 working
   scripts are untracked and have never been committed — including `90s Data Cleaning.R`,
   **the sole surviving record of how the stock estimate is built** (§6). They exist in exactly one
   place, on one disk. Commit them.

---

## 11. What is lost

**54 of the 60 input files the 2022 scripts reference no longer exist.** The six survivors are
`Combined.csv`, `CompleteData1958-1991.csv`, `1950HouseDataIV.csv`, `1950HouseDataV.csv`,
`1950s Tenure Data.csv` and `Complete West Capital Formation.csv`.

The 14 scripts in `Project R Code/` therefore cannot run and cannot be made to. They also point at
eight working directories under `C:/Users/S.Watling/`, a Centre for Cities account that is gone.
**Read them as documentation of method** — which is exactly how §6 was recovered.

Not reproducible from what survives:

| | needs |
|---|---|
| Figure 1, Tables 5-6 | historic E&W data — **found**, see §9 |
| Figure 6 (residential investment) | the capital formation files — partly present |
| Figure 9 (house prices vs wages) | **both found 10 Sep 2026** — §9a |
| Table 1 (dwelling sizes) | the 1950s rooms data — gone |
| Table 3 tenure split | the technical annex — §7 |

Also missing but promised by the README: a **methodology** document.

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

1. **Commit `Project R Code/`** (§10.6). It is untracked, unique, and the only documentation of
   the method that survives. Cheapest and highest-value action here.
2. **Resolve the Table 3 tenure split** (§7) — find the technical annex, or publish the script's
   own split with an explanation. This is the only substantive gap between the archive and the
   report.
3. **Add Figure 1 and Tables 5-6** to `Summary Replication.R` from the historic E&W data (§9), and
   pin down the 1940s/1950s/1970s discrepancies first.
4. **Add Figure 9** from the price and wage data (§9a). The transform is fully specified in
   `Domestic Britain Code.R` lines 376-400 and both inputs are on disk, so this is the most
   straightforward of the remaining figures.
5. **Resolve `Dropbox\Eurpean` against `Processed European Data`** (§10.1) and delete the loser.
6. Install `ggpattern` if Figure 10 needs to match the published styling.
7. Consider whether `Project R Code/` should be marked read-only or moved to a `docs/` subfolder,
   so it stops looking like code that could be run.
