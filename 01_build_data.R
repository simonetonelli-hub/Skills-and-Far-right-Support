################################################################################
##  Skills and Far-right Support: How Education and Training Systems Moderate  
##  Voting Behaviour in The Knowledge Economy.
##  Authors: Simone Tonelli, Niccolo Durazzi, Philip Rathgeb
##  Journal of European Public Policy
##  Replication part 1 of 2 - data construction
##  Input : raw ESS, ParlGov, PopuList, OECD, CPDS and dual-VET source files
##  Output: vet_rrp_analysis.dta, vet_rrp_analysis_empunemp.dta,
##          vet_rrp_analysis_allactive.dta, vet_rrp_analysis_ches.dta
##  Run this before 02_analysis.do
################################################################################


## =============================================================================
## 0.1  Packages
## =============================================================================

library(tidyverse)
library(haven)
library(labelled)
library(countrycode)
library(occupar)


## =============================================================================
## 0.2  Paths (edit PROJECT_DIR)
## =============================================================================

PROJECT_DIR <- getwd()
SOURCES <- file.path(PROJECT_DIR, "data")
OUT     <- file.path(PROJECT_DIR, "output")
LOGS    <- file.path(OUT, "logs")

dir.create(OUT,  showWarnings = FALSE, recursive = TRUE)
dir.create(LOGS, showWarnings = FALSE, recursive = TRUE)


## =============================================================================
## 0.3  Run log
## =============================================================================

RUN_STARTED <- Sys.time()
LOGFILE <- file.path(
  LOGS, paste0("01_build_data_", format(RUN_STARTED, "%Y%m%d_%H%M%S"), ".log"))

.LOGLINES <- character(0)

logmsg <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n", sep = "")
  utils::flush.console()
  .LOGLINES <<- c(.LOGLINES, txt)
  invisible(txt)
}

logflush <- function() writeLines(.LOGLINES, LOGFILE)

logmsg("================================================================")
logmsg("01_build_data.R")
logmsg("Started : ", format(RUN_STARTED, "%Y-%m-%d %H:%M:%S"))
logmsg("Project : ", PROJECT_DIR)
logmsg("R       : ", R.version.string)
logmsg("================================================================")
logflush()


## =============================================================================
## 0.4  Country scope
## =============================================================================

country_codes <- c("AT","BE","CH","CZ","DE","DK","ES","FI","FR","GB",
                   "GR","HU","IE","IL","IT","LU","NL","NO","PL","PT",
                   "SE","SI","EE","IS","SK","TR","UA","BG","CY","RU",
                   "HR","LV","RO","LT","AL","XK","ME","RS")


## =============================================================================
## 0.5  Source files (replace each placeholder with the name of your download)
## =============================================================================

F_ESS          <- file.path(SOURCES, "[ESS]")
F_PARLGOV      <- file.path(SOURCES, "[PARLGOV]")
F_POPULIST     <- file.path(SOURCES, "[POPULIST]")
F_PARTYFACTS   <- file.path(SOURCES, "[PARTYFACTS]")
F_PF_HARMONISE <- file.path(SOURCES, "[PARTYFACTS_ESS_HARMONISE]")
F_PF_ESSPRTV   <- file.path(SOURCES, "[PARTYFACTS_ESS_PRTV]")
F_DUALVET      <- file.path(SOURCES, "[DUALVET]")
F_CPDS         <- file.path(SOURCES, "[CPDS]")
F_OECD_GDP     <- file.path(SOURCES, "[OECD_GDP]")
F_OECD_INEQ    <- file.path(SOURCES, "[OECD_INEQUALITY]")
F_CHES         <- file.path(SOURCES, "[CHES]")



## =============================================================================
## 1.1  Read the cumulative ESS file
## =============================================================================

logmsg("\n[1/8] Reading the ESS cumulative file ..."); logflush()

ess_raw <- read_dta(F_ESS)

logmsg("      done: ", format(nrow(ess_raw), big.mark = ","), " rows"); logflush()


## =============================================================================
## 1.2  Extract the party-choice variables
## =============================================================================

prtv <- ess_raw %>%
  select(
    cntry, essround, idno,
    inwmms, inwmme, inwmm, inwdds, inwdde, inwdd,
    inwyys, inwyye, inwyr, inwde, inwds,
    vote,
    starts_with("prtv")
  ) %>%
  select(-matches("^prtv[a-z]?de1$"), -matches("^prtv[a-z]?lt[23]$"))


## =============================================================================
## 1.3  Extract the analysis variables
## =============================================================================

depvar <- ess_raw %>%
  select(
    cntry, region, idno, essround, pspwght, pweight, anweight,
    vote, gndr, agea, domicil, blgetmg, rlgdgr, hinctnta,
    edulvla, edulvlb, eduyrs,
    edulvlfa, edulvlfb, edulvlma, edulvlmb,
    eduade1, eduade2, eduade3, edlvbch, edlvcch, edlvdch, edlveat, eduat1,
    pdwrk, uempla, uempli, edctn, emplrel, crpdwk, wrkctra, mbtru,
    isco08, iscoco, nacer1, nacer11, nacer2,
    uemp3m, uemp12m, uemp5yr, uempnyr, lkuemp,
    jbspv, njbspv, wkdcorga, iorgact, jbscr,
    imwbcnt, lrscale, stfgov, stflife, stfjb, stfeco, stfdem,
    trstplt, trstprl, gincdif, plinsoc, atchctr, hincfel,
    lknemny, lsintjb, allbpe, allbpne, grsplet
  )

rm(ess_raw); gc()


## =============================================================================
## 2.1  Far-right party list from PopuList 3.0, borderline cases excluded
## =============================================================================

logmsg("\n[2/8] Party-level far-right coding ..."); logflush()

populist <- read_delim(F_POPULIST,
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(partyfacts_id, farright, farright_bl,
         farright_startnobl, farright_endnobl) %>%
  filter(farright == 1, farright_bl == 0, !is.na(partyfacts_id)) %>%
  select(partyfacts_id, starts_with("farr"))


## =============================================================================
## 2.2  ESS party variables to Party Facts identifiers
## =============================================================================

harmonise_ess <- read_csv(F_PF_HARMONISE) %>%
  filter(grepl("prtv", ess_variable)) %>%
  select(ess_id, first_ess_id)

essprtv <- read_csv(F_PF_ESSPRTV) %>%
  select(first_ess_id, partyfacts_id)


## =============================================================================
## 2.3  Readable party label per respondent
## =============================================================================

partynames <- prtv %>%
  mutate(id = paste(cntry, idno, essround, sep = "-")) %>%
  select(id, starts_with("prtv")) %>%
  mutate(across(starts_with("prtv"), ~ to_character(.x, nolabel_to_na = TRUE))) %>%
  pivot_longer(cols = starts_with("prtv"),
               names_to = "column_name", values_to = "label") %>%
  filter(!is.na(label)) %>%
  select(id, label)


## =============================================================================
## 3.1  Map every calendar date to the preceding national election
## =============================================================================

logmsg("\n[3/8] Election calendar ..."); logflush()

edates <- read_csv(F_PARLGOV) %>%
  filter(election_type == "parliament") %>%
  select(country_name_short, election_date) %>%
  distinct() %>%
  mutate(cntry = countrycode(country_name_short, "iso3c", "iso2c")) %>%
  select(cntry, election_date) %>%
  mutate(year = format(as.Date(election_date, format = "%Y-%m-%d"), "%Y")) %>%
  filter(year > 1990) %>%
  mutate(election = election_date) %>%
  select(-year)

date_seq <- seq(as.Date("1990-01-01"), as.Date("2023-12-31"), by = "day")

elections <- expand.grid(date = date_seq, country = country_codes) %>%
  as_tibble() %>%
  left_join(edates, by = c("country" = "cntry", "date" = "election_date")) %>%
  group_by(country) %>%
  fill(election, .direction = "down") %>%
  ungroup() %>%
  na.omit() %>%
  rename(cntry = country, sdate = date)


## =============================================================================
## 3.2  Flag elections contested by a far-right party
## =============================================================================

pop_elec <- read_delim(F_POPULIST,
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(farright == 1, farright_bl == 0) %>%
  select(farright, parlgov_id, farright_startnobl, farright_endnobl) %>%
  distinct()

isrrp <- read_csv(F_PARLGOV) %>%
  left_join(pop_elec, by = c("party_id" = "parlgov_id")) %>%
  select(country_name_short, election_date, starts_with("far")) %>%
  mutate(eyear = format(as.Date(election_date), "%Y")) %>%
  mutate(farright = ifelse(farright_startnobl != 1900 & eyear < farright_startnobl, 0,
                    ifelse(farright_endnobl  != 2100 & eyear > farright_endnobl,  0,
                           farright))) %>%
  mutate(farright = ifelse(is.na(farright), 0, farright)) %>%
  select(country_name_short, election_date, farright) %>%
  distinct() %>%
  group_by(country_name_short, election_date) %>%
  filter(n() == 1 | farright == 1) %>%
  ungroup() %>%
  mutate(cntry = countrycode(country_name_short, "iso3c", "iso2c")) %>%
  rename(election = election_date, isthereanrrp = farright) %>%
  select(cntry, election, isthereanrrp)


## =============================================================================
## 3.3  Interview date, party voted, and far-right coding per respondent
## =============================================================================

partyvoted <- prtv %>%
  mutate(id = paste(cntry, idno, essround, sep = "-")) %>%
  mutate(date = case_when(!is.na(inwds)                 ~ format(inwds, "%Y-%m-%d"),
                          is.na(inwds) & !is.na(inwde)  ~ format(inwde, "%Y-%m-%d"))) %>%
  mutate(month = case_when(
    is.na(inwmms) & !is.na(inwmme) & str_count(inwmme) == 1 & essround >= 3 & essround < 10 ~ paste0("0", inwmme),
    is.na(inwmms) & !is.na(inwmme) & str_count(inwmme) != 1 & essround >= 3 & essround < 10 ~ as.character(inwmme),
    !is.na(inwmms) & str_count(inwmms) == 1 & essround >= 3 & essround < 10 ~ paste0("0", inwmms),
    !is.na(inwmms) & str_count(inwmms) != 1 & essround >= 3 & essround < 10 ~ as.character(inwmms),
    !is.na(inwmm)  & str_count(inwmm)  == 1 & essround < 3 ~ paste0("0", inwmm),
    !is.na(inwmm)  & str_count(inwmm)  != 1 & essround < 3 ~ as.character(inwmm))) %>%
  mutate(day = case_when(
    is.na(inwdds) & !is.na(inwdde) & str_count(inwdde) == 1 & essround >= 3 & essround < 10 ~ paste0("0", inwdde),
    is.na(inwdds) & !is.na(inwdde) & str_count(inwdde) != 1 & essround >= 3 & essround < 10 ~ as.character(inwdde),
    !is.na(inwdds) & str_count(inwdds) == 1 & essround >= 3 & essround < 10 ~ paste0("0", inwdds),
    !is.na(inwdds) & str_count(inwdds) != 1 & essround >= 3 & essround < 10 ~ as.character(inwdds),
    !is.na(inwdd)  & str_count(inwdd)  == 1 & essround < 3 ~ paste0("0", inwdd),
    !is.na(inwdd)  & str_count(inwdd)  != 1 & essround < 3 ~ as.character(inwdd))) %>%
  mutate(year = case_when(
    is.na(inwyys)  & essround >= 3 & essround < 10 ~ inwyye,
    !is.na(inwyys) & essround >= 3 & essround < 10 ~ inwyys,
    !is.na(inwyr)  & essround < 3                  ~ inwyr)) %>%
  mutate(sdate = case_when(!is.na(year) & !is.na(month) & !is.na(day) ~
                             paste(year, month, day, sep = "-"))) %>%
  mutate(sdate = as.Date(ifelse(is.na(sdate), date, sdate))) %>%
  filter(vote == 1) %>%
  select(cntry, essround, id, sdate, starts_with("prt")) %>%
  pivot_longer(cols = starts_with("prtv")) %>%
  na.omit() %>%
  mutate(ess_id = case_when(
    cntry %in% c("DE", "LT") ~ paste(cntry, essround, value, substr(name, 4, 4),
                                     str_sub(name, -3, -1), sep = "-"),
    TRUE                     ~ paste(cntry, essround, value, substr(name, 4, 4), sep = "-"))) %>%
  left_join(harmonise_ess, by = "ess_id") %>%
  left_join(essprtv,       by = "first_ess_id") %>%
  left_join(partynames,    by = "id") %>%
  mutate(partyfacts_id = as.numeric(partyfacts_id)) %>%
  mutate(value = as.numeric(value)) %>%
  rename(partyname = label, partyvalue = value) %>%
  filter(!is.na(partyfacts_id) | essround == 11) %>%
  left_join(populist, by = "partyfacts_id") %>%
  left_join(elections, by = c("cntry", "sdate")) %>%
  mutate(eyear = as.numeric(format(as.Date(election), "%Y"))) %>%
  mutate(farright = ifelse(farright_startnobl != 1900 & eyear < farright_startnobl, 0,
                    ifelse(farright_endnobl  != 2100 & eyear > farright_endnobl,  0,
                           farright)))


## =============================================================================
## 4.1  Dual VET share, matched on election year and on survey year
## =============================================================================

logmsg("\n[4/8] Country-level series ..."); logflush()

vet_long <- read_delim(F_DUALVET,
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  pivot_longer(-code2, names_to = "year", values_to = "share") %>%
  rename(cntry = code2) %>%
  mutate(year = as.numeric(year))

vet_shares       <- vet_long %>% rename(dualvetPatrick = share)
vet_shares_round <- vet_long %>% rename(dualvet_round  = share)


## =============================================================================
## 4.2  CPDS: unemployment, employment protection, electoral system
## =============================================================================

cpds <- read_dta(F_CPDS) %>%
  mutate(cntry = countrycode(iso, "iso3c", "iso2c")) %>%
  select(cntry, year, unemp, emprot_reg, prop)


## =============================================================================
## 4.3  GDP per capita, 2011 cross-section
## =============================================================================

GDPpc <- read_csv(F_OECD_GDP) %>%
  mutate(cntry = countrycode(LOCATION, "iso3c", "iso2c")) %>%
  rename(year = Year, GDPpc = Value) %>%
  select(cntry, year, GDPpc) %>%
  mutate(logGDPpc = log(GDPpc)) %>%
  filter(year == 2011) %>%
  select(-year)


## =============================================================================
## 4.4  OECD Gini and wage-ratio series used in Table A5
## =============================================================================

oecd_ineq <- read_csv(F_OECD_INEQ, show_col_types = FALSE) %>%
  select(cntry, year, gini, wage_p50_p10, wage_p90_p10, wage_p90_p50)


## =============================================================================
## 5.  Individual-level recodes, party match and macro merges
## =============================================================================

logmsg("\n[5/8] Individual recodes and merges ..."); logflush()

ess <- depvar %>%

  mutate(sector = case_when(
    nacer1 >= 15 & nacer1 <= 36 | nacer11 >= 15 & nacer11 <= 36 |
      nacer2 >= 10 & nacer2 <= 32                                  ~ "Manufacturing",
    nacer1 < 15 | nacer11 < 15 | nacer2 < 10                       ~ "Agriculture & Mining",
    nacer1  >= 37 & nacer1  <= 41 | nacer1  >= 50 & nacer1  <= 64 | nacer1  == 71 | nacer1  >= 90 & nacer1  <= 99 |
      nacer11 >= 37 & nacer11 <= 41 | nacer11 >= 50 & nacer11 <= 64 | nacer11 == 71 | nacer11 >= 90 & nacer11 <= 99 |
      nacer2  >= 33 & nacer2  <= 39 | nacer2  >= 45 & nacer2  <= 61 | nacer2  >= 77 & nacer2  <= 81 | nacer2 >= 90 & nacer2 <= 99 ~ "Low-end service",
    nacer1  >= 65 & nacer1  <= 70 | nacer1  >= 72 & nacer1  <= 85 |
      nacer11 >= 65 & nacer11 <= 70 | nacer11 >= 72 & nacer11 <= 85 |
      nacer2  >= 62 & nacer2  <= 75 | nacer2  >= 82 & nacer2  <= 88 ~ "High-end service",
    nacer1 == 45 | nacer11 == 45 | nacer2 >= 41 & nacer2 <= 43     ~ "Construction")) %>%

  mutate(id = paste(cntry, idno, essround, sep = "-")) %>%
  left_join(partyvoted, by = c("cntry", "essround", "id")) %>%
  filter(agea != 999) %>%

  mutate(isco_mainjob = isco08to88(isco08 = isco08),
         isco_mainjob = ifelse(is.na(isco_mainjob), iscoco, isco_mainjob),
         isco_main1   = as.numeric(substr(as.character(isco_mainjob), 1, 1))) %>%
  mutate(occupation = case_when(
    isco_mainjob >= 1000 & isco_mainjob < 4000                                    ~ 1,
    isco_mainjob >= 4000 & isco_mainjob < 5000 |
      isco_mainjob >= 6000 & isco_mainjob < 9000                                  ~ 2,
    isco_mainjob >= 5000 & isco_mainjob < 6000 |
      isco_mainjob >= 9000 & isco_mainjob < 10000                                 ~ 3)) %>%
  mutate(occupation = factor(occupation, levels = 1:3,
                             labels = c("NRC", "Routine", "NRM"))) %>%

  mutate(eduvet2 = case_when(
    edulvlb %in% c(0, 113, 212, 213, 129, 221, 222, 223, 229)   ~ 1,
    edulvlb %in% c(311, 312, 313, 412, 413)                     ~ 2,
    edulvlb %in% c(321, 322, 323, 421, 422, 423)                ~ 3,
    edulvlb %in% c(510, 520, 610, 620, 710, 720, 800)           ~ 4)) %>%
  mutate(eduvet2 = factor(eduvet2, levels = 1:4,
                          labels = c("Low", "General", "Vocational", "Tertiary"))) %>%
  mutate(eduvet1 = case_when(
    edulvlb %in% c(0, 113, 212, 213, 129, 221, 222, 223, 229)   ~ 1,
    edulvlb %in% c(311, 312, 313)                               ~ 2,
    edulvlb %in% c(321, 322, 323)                               ~ 3,
    edulvlb %in% c(412, 413, 421, 422, 423,
                   510, 520, 610, 620, 710, 720, 800)           ~ 4)) %>%
  mutate(eduvet1 = factor(eduvet1, levels = 1:4,
                          labels = c("Low", "General", "Vocational",
                                     "Post-secondary and higher"))) %>%

  mutate(urban    = case_when(domicil %in% 1:2 ~ 1, domicil %in% 3:5 ~ 0),
         minority = factor(blgetmg, levels = 1:2, labels = c("yes", "no")),
         status   = as.numeric(abs(plinsoc - 10)),
         male     = case_when(gndr == 1 ~ 1, gndr == 2 ~ 0),
         age2     = agea * agea,
         permanent = case_when(wrkctra == 1 ~ 1, !is.na(wrkctra) ~ 0)) %>%
  mutate(degreemom = as.integer(edulvlmb >= 500 | edulvlma == 5),
         degreedad = as.integer(edulvlfb >= 500 | edulvlfa == 5),
         degreemom = replace_na(degreemom, 0L),
         degreedad = replace_na(degreedad, 0L),
         atleastadegree = as.integer(degreemom == 1 | degreedad == 1)) %>%
  mutate(unemplkl = case_when(lkuemp %in% 1:4 ~ as.numeric(lkuemp),
                              uempnyr == 4 ~ 1, uempnyr == 3 ~ 2,
                              uempnyr == 2 ~ 3, uempnyr == 1 ~ 4)) %>%
  mutate(unemplkl = factor(unemplkl, levels = 1:4,
                           labels = c("Not at all likely", "Not very likely",
                                      "Likely", "Very likely"))) %>%
  mutate(anweight = pspwght * pweight) %>%

  filter(agea > 17, vote == 1) %>%
  mutate(year  = as.numeric(format(sdate, "%Y")),
         eyear = as.numeric(format(as.Date(election), "%Y")),
         cntryyear = paste(cntry, eyear)) %>%

  mutate(farright = if_else(
    essround == 11 & partyname %in% c(
      "Swiss People's Party", "Ticino League", "Federal Democratic Union",
      "Le Rassemblement national (ex Front National", "UK Independence Party",
      "Brexit Party", "Ελληνική Λύση",
      "Lega", "Fratelli d'Italia", "Party 'Freedom and Justice' (LT)",
      "JA21", "Party for Freedom", "Forum for Democracy", "CHEGA",
      "SDS - Slovenska demokratska stranka",
      "NSI - Nova Slovenija – Kršcanski demokrati",
      "SNS - Slovenska nacionalna stranka", "SME Rodina",
      "ĽS Naše Slovensko",
      "6 FIDESZ-KDNP (Fidesz Magyar Polgári Szövetség-Kereszténydemokrata Néppárt"),
    1, farright)) %>%

  group_by(cntry, election) %>%
  mutate(nrrpvotes = sum(farright, na.rm = TRUE),
         atleastone = ifelse(nrrpvotes > 0, 1, nrrpvotes)) %>%
  ungroup() %>%
  mutate(election = as.Date(election)) %>%
  left_join(isrrp, by = c("cntry", "election")) %>%
  mutate(farright = ifelse(is.na(farright), 0, farright)) %>%

  left_join(vet_shares,       by = c("cntry", "eyear" = "year")) %>%
  left_join(vet_shares_round, by = c("cntry", "year")) %>%
  left_join(GDPpc,            by = "cntry") %>%
  left_join(cpds,             by = c("cntry", "year")) %>%
  left_join(oecd_ineq,        by = c("cntry", "year")) %>%
  mutate(dualsystem = ifelse(cntry == "DE" | cntry == "AT" | cntry == "CH" |
                             cntry == "NL" | cntry == "DK", 1, 0))


## =============================================================================
## 6.1  Keep elections contested by, and with observed votes for, a far-right party
## =============================================================================

logmsg("\n[6/8] Building the three analytic samples ..."); logflush()

ess <- ess %>% filter(isthereanrrp == 1, atleastone == 1)


## =============================================================================
## 6.2  Published sample: dependent employees in paid work
## =============================================================================

ess_employees <- ess %>%
  filter(emplrel == 1, uempla == 0, uempli == 0, edctn == 0, pdwrk == 1)


## =============================================================================
## 6.3  Labour-force flags
## =============================================================================

ess_lf <- ess %>%
  mutate(
    lf_inwork   = coalesce(as.integer(pdwrk),   0L),
    lf_employee = coalesce(as.integer(emplrel), 0L),
    lf_unemp    = as.integer(coalesce(as.integer(uempla), 0L) == 1L |
                             coalesce(as.integer(uempli), 0L) == 1L))


## =============================================================================
## 6.4  Employees plus the unemployed, and the whole economically active population
## =============================================================================

ess_empunemp <- ess_lf %>%
  filter(edctn == 0) %>%
  filter((lf_employee == 1L & lf_inwork == 1L) | lf_unemp == 1L) %>%
  mutate(unemployed = lf_unemp) %>%
  select(-lf_inwork, -lf_employee, -lf_unemp)

ess_allactive <- ess_lf %>%
  filter(edctn == 0) %>%
  filter(lf_inwork == 1L | lf_unemp == 1L) %>%
  mutate(unemployed = lf_unemp,
         selfemp    = as.integer(lf_inwork == 1L & lf_employee %in% c(2L, 3L))) %>%
  select(-lf_inwork, -lf_employee, -lf_unemp)


## =============================================================================
## 6.5  Write the three analytic files
## =============================================================================

write_dta(ess_employees, file.path(OUT, "vet_rrp_analysis.dta"))
write_dta(ess_empunemp,  file.path(OUT, "vet_rrp_analysis_empunemp.dta"))
write_dta(ess_allactive, file.path(OUT, "vet_rrp_analysis_allactive.dta"))

logmsg("      employees only          : ", format(nrow(ess_employees), big.mark = ","), " rows")
logmsg("      employees + unemployed  : ", format(nrow(ess_empunemp),  big.mark = ","), " rows")
logmsg("      all economically active : ", format(nrow(ess_allactive), big.mark = ","), " rows")
logflush()

ess <- ess_employees


## =============================================================================
## 7.1  Read CHES and the Party Facts crosswalk
## =============================================================================

logmsg("\n[7/8] CHES alternative dependent variables ..."); logflush()

ches <- read_csv(F_CHES,
                 show_col_types = FALSE)

pf_ches <- read_csv(F_PARTYFACTS, show_col_types = FALSE) %>%
  filter(dataset_key == "ches", !is.na(partyfacts_id), !is.na(dataset_party_id)) %>%
  transmute(
    partyfacts_id = as.numeric(partyfacts_id),
    party_id      = as.numeric(dataset_party_id),
    pf_year_first = suppressWarnings(as.numeric(year_first)),
    pf_year_last  = suppressWarnings(as.numeric(year_last))
  ) %>%
  distinct()


## =============================================================================
## 7.2  Helpers: NaN handling and election year to CHES wave
## =============================================================================

nan_to_na <- function(x) ifelse(is.nan(x), NA_real_, x)

map_election_to_ches_year <- function(x) {
  case_when(
    x <= 2008              ~ 2006,
    x >= 2009 & x <= 2012  ~ 2010,
    x >= 2013 & x <= 2016  ~ 2014,
    x >= 2017 & x <= 2020  ~ 2019,
    x >= 2021              ~ 2024,
    TRUE                   ~ NA_real_
  )
}


## =============================================================================
## 7.3  Build the five alternative far-right codings
## =============================================================================

ches_lookup <- ches %>%
  rename(ches_year = year) %>%
  mutate(party_id = as.numeric(party_id),
         family   = as.numeric(family)) %>%
  left_join(pf_ches, by = "party_id") %>%
  filter(!is.na(partyfacts_id)) %>%
  filter((is.na(pf_year_first) | ches_year >= pf_year_first),
         (is.na(pf_year_last)  | ches_year <= pf_year_last)) %>%
  arrange(partyfacts_id, ches_year, desc(pf_year_first), desc(pf_year_last)) %>%
  distinct(partyfacts_id, ches_year, .keep_all = TRUE) %>%
  mutate(
    nativism_index = nan_to_na(rowMeans(
      cbind(immigrate_policy, multiculturalism, nationalism), na.rm = TRUE)),
    econ_protection_score = nan_to_na(rowMeans(
      cbind(10 - lrecon,
            10 - spendvtax,
            10 - redistribution,
            if_else(!is.na(econ_interven), 10 - econ_interven, NA_real_),
            protectionism),
      na.rm = TRUE)),
    farright_ches_family = if_else(family == 1, 1, 0, missing = 0),
    farright_ches_strict = if_else(
      family == 1 &
        !is.na(lrgen)          & lrgen          >= 7 &
        !is.na(galtan)         & galtan         >= 7 &
        !is.na(nativism_index) & nativism_index >= 7,
      1, 0, missing = 0),
    farright_ches_broad = if_else(
      family == 1 |
        (!is.na(lrgen)          & lrgen          >= 7 &
         !is.na(galtan)         & galtan         >= 7 &
         !is.na(nativism_index) & nativism_index >= 7),
      1, 0, missing = 0)
  ) %>%
  group_by(ches_year) %>%
  mutate(rr_wave_median_econ = median(
    econ_protection_score[farright_ches_family == 1], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    rr_ches_type = case_when(
      farright_ches_family != 1                            ~ "not_far_right",
      is.na(econ_protection_score)                         ~ "not_classified",
      econ_protection_score >= rr_wave_median_econ         ~ "welfare_chauvinist",
      econ_protection_score <  rr_wave_median_econ         ~ "primarily_nativist",
      TRUE                                                 ~ "not_classified"),
    farright_ches_welfare  = if_else(rr_ches_type == "welfare_chauvinist",  1, 0, missing = 0),
    farright_ches_nativist = if_else(rr_ches_type == "primarily_nativist", 1, 0, missing = 0)
  ) %>%
  select(partyfacts_id, ches_year, family, lrgen, galtan,
         nativism_index, econ_protection_score, rr_wave_median_econ, rr_ches_type,
         farright_ches_family, farright_ches_strict, farright_ches_broad,
         farright_ches_welfare, farright_ches_nativist)

stopifnot(!any(duplicated(ches_lookup[c("partyfacts_id", "ches_year")])))


## =============================================================================
## 7.4  Attach the codings to respondents and write the CHES file
## =============================================================================

ess_ches <- ess %>%
  mutate(ches_year = map_election_to_ches_year(eyear)) %>%
  left_join(ches_lookup, by = c("partyfacts_id", "ches_year"))

n_before  <- nrow(ess_ches)
n_matched <- sum(!is.na(ess_ches$farright_ches_family))

ess_alt_dv_ches_finalsample <- ess_ches %>%
  mutate(
    farright_ches_family   = replace_na(farright_ches_family,   0),
    farright_ches_strict   = replace_na(farright_ches_strict,   0),
    farright_ches_broad    = replace_na(farright_ches_broad,    0),
    farright_ches_welfare  = replace_na(farright_ches_welfare,  0),
    farright_ches_nativist = replace_na(farright_ches_nativist, 0))

write_dta(ess_alt_dv_ches_finalsample, file.path(OUT, "vet_rrp_analysis_ches.dta"))

logmsg("  respondents in build         : ", n_before)
logmsg("  matched to a CHES party-wave : ", n_matched,
       sprintf("  (%.1f%%)", 100 * n_matched / n_before))
logmsg("  unmatched, coded 0           : ", n_before - n_matched)
for (v in c("farright_ches_family", "farright_ches_strict", "farright_ches_broad",
            "farright_ches_nativist", "farright_ches_welfare")) {
  logmsg(sprintf("    %-24s %7d",
                 v, sum(ess_alt_dv_ches_finalsample[[v]] == 1, na.rm = TRUE)))
}
logflush()


## =============================================================================
## 8.  Completion log
## =============================================================================

logmsg("\n[8/8] Finishing ...")

RUN_ENDED <- Sys.time()
.elapsed <- round(as.numeric(difftime(RUN_ENDED, RUN_STARTED, units = "mins")), 1)

logmsg("\n================================================================")
logmsg("RUN COMPLETED")
logmsg("Started : ", format(RUN_STARTED, "%Y-%m-%d %H:%M:%S"))
logmsg("Ended   : ", format(RUN_ENDED,   "%Y-%m-%d %H:%M:%S"))
logmsg("Elapsed : ", .elapsed, " minutes")
logmsg("\nFiles written to ", OUT, ":")

for (f in c("vet_rrp_analysis.dta", "vet_rrp_analysis_empunemp.dta",
            "vet_rrp_analysis_allactive.dta", "vet_rrp_analysis_ches.dta")) {
  fp <- file.path(OUT, f)
  if (file.exists(fp)) {
    logmsg(sprintf("  %-38s %9.1f MB", f, file.size(fp) / 1024^2))
  } else {
    logmsg(sprintf("  %-38s MISSING", f))
  }
}

logmsg("\n================================================================")
logmsg("END OF 01_build_data.R -- now run 02_analysis.do")
logmsg("================================================================")

writeLines(c(.LOGLINES, "", "sessionInfo():",
             capture.output(sessionInfo())), LOGFILE)

writeLines(
  c(paste0("01_build_data.R completed ", format(RUN_ENDED, "%Y-%m-%d %H:%M:%S")),
    paste0("elapsed_minutes: ", .elapsed),
    paste0("log: ", LOGFILE),
    paste0("rows_analysis: ", nrow(ess)),
    paste0("rows_ches: ", nrow(ess_alt_dv_ches_finalsample))),
  file.path(OUT, "_BUILD_COMPLETED.txt"))

################################################################################
##  END OF PART 1
################################################################################
