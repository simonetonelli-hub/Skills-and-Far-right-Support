------------------------------------------------------------------------
# Replication package

**Skills and Far-Right Voting: How Education and Training Systems Moderate Electoral Behaviour in the Knowledge Economy**

Two scripts reproduce every table and figure in the article and the appendix, from publicly available source data.

```         
01_build_data.R      builds the analysis files from the raw sources
02_analysis.do       estimates every model and writes every table and figure
data/                you place the downloaded source files here
output/              created by the scripts
```

**All the data used in this article are publicly available online.** None of the source files is redistributed here: the table below gives, for each input, what it is and where to obtain it. One small file that we constructed ourselves is included in `data/`.

------------------------------------------------------------------------

## Requirements

**R** (4.2 or later) with `tidyverse`, `haven`, `labelled`, `countrycode`, and `occupar`. The last is not on CRAN:

``` r
devtools::install_github("DiogoFerrari/occupar")
```

**Stata** 17 or later with `estout` and `grc1leg`:

``` stata
ssc install estout
ssc install grc1leg
```

------------------------------------------------------------------------

## How to run

**1. Obtain the source files.** Download each input from the links in the next section and put them in `data/`. Keep whatever file names the providers give you.

**2. Point the build script at them.** Open `01_build_data.R`. Set `PROJECT_DIR` in section 0.2 to the folder containing this README, then replace each placeholder in section 0.5 with the name of the file you downloaded. For example:

``` r
F_ESS       <- file.path(SOURCES, "[ESS]")          # before
F_ESS       <- file.path(SOURCES, "ESS1-11.dta")    # after
```

Nothing else in the script needs editing.

**3. Run the build.** Run `01_build_data.R` whole. Reading the ESS cumulative file takes several minutes. The script writes four files to `output/` and a timestamped log to `output/logs/`:

| File | Sample |
|------------------------------------|------------------------------------|
| `vet_rrp_analysis.dta` | dependent employees in paid work — the sample used in the article |
| `vet_rrp_analysis_empunemp.dta` | the above plus the unemployed (Table A12) |
| `vet_rrp_analysis_allactive.dta` | the whole economically active population (Table A12) |
| `vet_rrp_analysis_ches.dta` | the article's sample with the CHES alternative outcomes (Table A10) |

**4. Run the analysis.** Open `02_analysis.do`, set the `PROJECT` global in section 0.1 to the same folder, and run the file whole:

``` stata
do 02_analysis.do
```

Run it top to bottom, not block by block — later sections depend on variables and macros defined in section 1. The full run takes roughly two hours, most of it in the `margins` calculations behind the figures. Output goes to `output/tables` (one `.rtf` per table), `output/figures` (one `.gph` and one 300 dpi `.tif` per figure) and `output/logs`, which also carries every table as text. A completion banner at the end lists any missing file.

**Checks.** Section 1.5 prints the estimation sample before any model is fitted. It should report **28,088** observations, **18** countries and **42** elections. The vocational × dual VET interaction in Model 3 of Table 1 is **−0.015** (SE 0.005).

------------------------------------------------------------------------

## Data sources

| Placeholder | What it is | Where to get it |
|------------------------|------------------------|------------------------|
| `[ESS]` | European Social Survey, cumulative file, rounds 1–11. Individual-level survey data. | [ESS Data Portal](https://www.europeansocialsurvey.org/data-portal) or the [Sikt archive](https://ess.sikt.no/). Build a cumulative extract covering rounds 1–11 and export it in Stata format. Free registration is required; ESS data may not be redistributed by third parties, which is why the file is not included here. |
| `[PARLGOV]` | ParlGov: national elections, parties and cabinets. Supplies the election calendar and the far-right flag per election. | [parlgov.org](https://www.parlgov.org/); the data files themselves are released on [Harvard Dataverse](https://dataverse.harvard.edu/dataverse/parlgov). Any export works provided it carries the columns `country_name_short`, `election_date`, `election_type` and `party_id`. |
| `[POPULIST]` | The PopuList 3.0: classification of populist, far-left and far-right parties in Europe. | [popu-list.org](https://popu-list.org/) or OSF, [doi:10.17605/OSF.IO/2EWKQ](https://doi.org/10.17605/OSF.IO/2EWKQ). Semicolon-delimited CSV. |
| `[PARTYFACTS]` | Party Facts external-dataset mapping. Links CHES party ids to Party Facts ids. | [Party Facts downloads](https://partyfacts.herokuapp.com/download/) — the "external parties" table; stable versions are also archived on Harvard Dataverse. Columns used: `dataset_key`, `dataset_party_id`, `partyfacts_id`, `year_first`, `year_last`. |
| `[PARTYFACTS_ESS_HARMONISE]` | Party Facts ESS harmonisation table, linking ESS party codes across rounds. Columns used: `ess_id`, `first_ess_id`, `ess_variable`. | The ESS folder under `import/` in [github.com/hdigital/partyfactsdata](https://github.com/hdigital/partyfactsdata). |
| `[PARTYFACTS_ESS_PRTV]` | Party Facts crosswalk from ESS `prtv*` party codes to Party Facts ids. Columns used: `first_ess_id`, `partyfacts_id`. | [Party Facts · essprtv](https://partyfacts.herokuapp.com/data/essprtv/), which offers `essprtv.csv` as a direct download. |
| `[DUALVET]` | National share of upper-secondary pupils in combined school- and work-based ("dual") programmes, by country and year, 1996–2020. The article's institutional moderator. | Replication data for Emmenegger and Haslberger (2026), *Journal of European Social Policy* 36(2), 185–198, [doi:10.1177/09589287251370494](https://doi.org/10.1177/09589287251370494). The underlying enrolment series can also be rebuilt from the [UNESCO Institute for Statistics data browser](https://databrowser.uis.unesco.org/), [Eurostat](https://ec.europa.eu/eurostat/web/education-and-training/database) or the [OECD Data Explorer](https://data-explorer.oecd.org/). Wide format, semicolon-delimited; first column `code2` holds ISO2 country codes, one column per year. |
| `[CPDS]` | Comparative Political Data Set. Supplies the unemployment rate, employment protection legislation and the electoral-system indicator. | [cpds-data.org](https://cpds-data.org/). Stata format. The current release runs to 2023, so its file name differs from the 1960–2022 update we used; either works, since the script reads `iso`, `year`, `unemp`, `emprot_reg` and `prop` by name. |
| `[OECD_GDP]` | OECD National Accounts, Table 1: GDP per capita. The models use the 2011 cross-section. | [OECD Data Explorer](https://data-explorer.oecd.org/). Columns used: `LOCATION`, `Year`, `Value`. |
| `[OECD_INEQUALITY]` | Gini coefficients and the 50/10, 90/10 and 90/50 gross-earnings decile ratios, by country and year (Table A5). | **Included in `data/`.** A frozen extract of the OECD Income Distribution Database and the OECD decile ratios of gross earnings, both available from the [OECD Data Explorer](https://data-explorer.oecd.org/). It is frozen deliberately: the OECD revises these back-series, so a fresh download no longer reproduces the published Table A5. Columns: `cntry`, `year`, `gini`, `wage_p50_p10`, `wage_p90_p10`, `wage_p90_p50`. |
| `[CHES]` | Chapel Hill Expert Survey trend file, party positions. Used only for the five alternative far-right codings in Table A10. | [chesdata.eu](https://www.chesdata.eu/). The means-by-party trend file. |

Please cite the original providers as well as this article when you use these data. Access conditions are theirs, not ours, and are set out on the pages above.

------------------------------------------------------------------------

## Notes on the analysis

- ESS rounds 1–4 drop out without an explicit filter: the education variable `edulvlb`, from which the three-way educational track is built, exists only from round 5.
- Table A7, column 3 (job satisfaction) does not converge. This is reported as it stands; the article draws no positive conclusion from that table.
- Table A10, column 5 (welfare-chauvinist coding) is estimated **unweighted**, because the weighted model does not converge on that outcome.
