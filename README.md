# The Housebuilding Crisis — replication archive

Code and data to reproduce Watling, S. and Breach, A. (2023),
[*The Housebuilding Crisis*](https://www.centreforcities.org/publication/the-housebuilding-crisis/),
Centre for Cities.

**Self-contained.** Clone it, open R, run either script. Nothing else is needed.

```
code/    two scripts, both of which run
data/    the four files they read, 400 KB
output/  figures and tables (created on run, not tracked)
```

## Running it

```r
source("code/Summary Replication.R")        # Table 2, Table 6, and 12 figures
source("code/Counterfactual Replication.R") # Table 3, the missing-homes estimates
```

Both resolve their paths whether you run them from the repo root or from `code/`.
Requires `tidyverse`, `readxl`, `zoo`, `xts`, `lubridate`, `rlang`.

## What it reproduces

| | |
|---|---|
| **Table 2** | private housebuilding 1955–79 — 12 countries × 4 statistics, exact |
| **Table 3** | the missing homes, 1955–2015 — all 13 totals exact |
| **Table 6** | England and Wales by decade — 7 of 10 decades exact |
| **Figures 1–5, 7–12** | all reproduced |

Every published figure is asserted in the code, so the scripts **fail loudly** if they stop
reproducing rather than drifting quietly from a report fixed in print.

Two things are not reproduced. **Table 1** (dwelling sizes) came from a book, not this data, so
there is nothing here to replicate. And Table 3's **private/public split** differs from the
published version while its totals match exactly — the cause is narrowed to one step and documented
at the top of `Counterfactual Replication.R`.

## The data

| file | what |
|---|---|
| `Replication Data full.xlsx` | the European panel, 1948–2021, 16 countries. Sheet 2 is capital formation |
| `Combined.csv` | the same panel with the UN's own column codes, at full precision |
| `England and Wales Housebuilding 1856-2019.csv` | tenure splits, demolitions and build rates |
| `UK Price and Wage Data 1952-2016.csv` | Nationwide house prices with BoE CPI and average weekly earnings |

Sources are the UN *Annual Bulletin of Housing and Building Statistics for Europe*, national
statistical agencies after 2000, Holmans's *Historical Statistics of British Housing*, the
Nationwide house price index and the Bank of England's millennium dataset.



## Also

- `CLAUDE.md` — the full audit: column dictionaries, where each estimate comes from, what is
  open, and the traps.
