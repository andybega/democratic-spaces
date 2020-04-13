#
#   Split raw V-Dem data into CY set, DV, IV pieces
#
#   Andreas Beger
#   2020-03-23
#
#   Adapated from `VDem_data_build.r`, which Rick Morgan originally wrote.
#
#   Output:
#
#     - trafo-data/dv_data_1968_on.csv
#     - trafo-data/vdem_data_1968_on.csv
#     - trafo-data/country_year_set_1968_on.csv
#     - dashboard/Data/dv_data_1968-2019.csv
#
#   NOTE FOR UPDATES:
#     For every vdem version update, we need to comment out any
#     "country_name == """ mutate function in the "vdem_clean_data"
#     construction.
#     It is likely that VDem fixed the NAs these mutate functions are
#     addressing...
#     Also, it is likely that the are other different NAs from version to
#     version...
#

library(tidyverse)
library(states)
library(tidyr)
library(here)

# needs devel version of states
if (packageVersion("states") < "0.2.2.9007") {
  stop("Please re-install the latest dev version of states:\n  remotes::install_github(\"andybega/states\")")
}

setwd(here::here("create-data"))

naCountFun <- function(dat, exclude_year){
  dat %>%
    filter(year < exclude_year) %>%
    sapply(function(x) sum(is.na(x))) %>%
    sort()
}

# The end year of observed data. Usually should be the year prior to the
# current year.
END_YEAR   <- 2019L
START_YEAR <- 1968L

vdem_raw <- readRDS("input/V-Dem-CY-Full+Others-v10.rds")

## Remove countries that have a lot of missingness in the VDem data... and make adjustments to merge with GW country-year set
vdem_complete <- vdem_raw %>%
  mutate(country_name = ifelse(country_id == 196, "Sao Tome and Principe", country_name)) %>%
  filter(year >= START_YEAR &
           country_name != "Palestine/West Bank" & country_name != "Hong Kong" & country_name != "Bahrain" & country_name != "Malta" &
           country_name != "Zanzibar" & country_name != "Somaliland" & country_name != "Palestine/Gaza") %>%
  filter(!(country_name == "Timor-Leste" & year < 2002)) %>%
  mutate(gwcode = COWcode,
         gwcode = case_when(gwcode == 255 ~ 260,
                            gwcode == 679 ~ 678,
                            gwcode == 345 & year >= 2006 ~ 340,
                            TRUE ~ gwcode)) %>%
  select(gwcode, everything())
dim(vdem_complete) ## 8430 4109

vdem_country_year0 <- vdem_complete %>%
  select(c(country_name, country_text_id, country_id, gwcode, year, v2x_pubcorr))
summary(vdem_country_year0)
# no_gwcode <- vdem_country_year0[is.na(vdem_country_year0$gwcode), c("country_name", "country_id", "year")]

vdem_country_year <- vdem_country_year0 %>%
  filter(!is.na(gwcode)) %>%
  group_by(gwcode) %>%
  complete(country_name, country_id, country_text_id, year = min(year):END_YEAR) %>%
  # fill(country_name) %>%
  ungroup()
dim(vdem_country_year) ## 8753  6

## GW_template is a balanced gwcode yearly data frame from 1968 to 2019. Need to drop microstates.
data(gwstates)

keep <- gwstates$gwcode[gwstates$microstate == FALSE]

GW_template <- state_panel(START_YEAR, END_YEAR, partial = "any", useGW = TRUE) %>%
  filter(gwcode %in% keep)
dim(GW_template) ## 8344 2

country_year_set <- left_join(GW_template, vdem_country_year) %>%
  filter(!is.na(country_id)) %>%
  mutate(keep = ifelse(is.na(v2x_pubcorr) & year != END_YEAR, 0, 1)) %>%
  filter(keep == 1) %>%
  select(-c(keep, v2x_pubcorr))

naCountFun(country_year_set, END_YEAR)
dim(country_year_set) ## 8120    5

write_csv(country_year_set, "trafo-data/country_year_set_1968_on.csv")

## Separate df for dvs...

vdem_dvs <- vdem_complete %>%
  select(c(country_name, country_text_id, country_id, gwcode, year,
           v2x_veracc_osp, v2xcs_ccsi, v2xcl_rol, v2x_freexp_altinf, v2x_horacc_osp, v2x_pubcorr, e_regionpol_6C)) %>%
  mutate(v2x_pubcorr = 1 - v2x_pubcorr)

dvs <- left_join(country_year_set, vdem_dvs)
naCountFun(dvs, END_YEAR)  # no NAs

write_csv(dvs, "../dashboard/Data/dv_data_1968_on.csv")

dvs <- dvs %>% 
  select(-e_regionpol_6C)

write_csv(dvs, "trafo-data/dv_data_1968_on.csv")

### Getting the V-Dem data in order...

vdem_ivs <- vdem_complete %>%
    select(country_name, country_text_id, gwcode, country_id, year, v2x_polyarchy, v2x_liberal, v2xdl_delib, v2x_jucon,
           v2x_frassoc_thick, v2xel_frefair, v2x_elecoff, v2xlg_legcon, v2x_partip, v2x_cspart, v2x_egal, v2xeg_eqprotec,
           v2xeg_eqaccess, v2xeg_eqdr, v2x_diagacc, v2xex_elecleg, v2x_civlib, v2x_clphy, v2x_clpol, v2x_clpriv, v2x_corr,
           v2x_EDcomp_thick, v2x_elecreg, v2x_freexp, v2x_gencl, v2x_gencs, v2x_hosabort, v2x_hosinter, v2x_rule, v2xcl_acjst,
           v2xcl_disc, v2xcl_dmove, v2xcl_prpty, v2xcl_slave, v2xel_elecparl, v2xel_elecpres, v2xex_elecreg, v2xlg_elecreg,
           v2ex_legconhog, v2ex_legconhos, v2x_ex_confidence, v2x_ex_direlect, v2x_ex_hereditary, v2x_ex_military, v2x_ex_party,
           v2x_execorr, v2x_legabort, v2xlg_leginter, v2x_neopat, v2xnp_client, v2xnp_pres, v2xnp_regcorr, v2elvotbuy, v2elfrcamp,
           v2elpdcamp, v2elpaidig, v2elmonref, v2elmonden, v2elrgstry, v2elirreg, v2elintim, v2elpeace, v2elfrfair, v2elmulpar,
           v2elboycot, v2elaccept, v2elasmoff, v2eldonate, v2elpubfin, v2ellocumul, v2elprescons, v2elprescumul, v2elembaut,
           v2elembcap, v2elreggov, v2ellocgov, v2ellocons, v2elrsthos, v2elrstrct, v2psparban, v2psbars, v2psoppaut, v2psorgs,
           v2psprbrch, v2psprlnks, v2psplats, v2pscnslnl, v2pscohesv, v2pscomprg, v2psnatpar, v2pssunpar, v2exremhsp, v2exdfdshs,
           v2exdfcbhs, v2exdfvths, v2exdfdmhs, v2exdfpphs, v2exhoshog, v2exrescon, v2exbribe, v2exembez, v2excrptps, v2exthftps,
           v2ex_elechos, v2ex_hogw, v2expathhs, v2lgbicam, v2lgqstexp, v2lginvstp, v2lgotovst, v2lgcrrpt, v2lgoppart, v2lgfunds,
           v2lgdsadlobin, v2lglegplo, v2lgcomslo, v2lgsrvlo, v2ex_hosw, v2dlreason, v2dlcommon, v2dlcountr, v2dlconslt,
           v2dlengage, v2dlencmps, v2dlunivl, v2jureform, v2jupurge, v2jupoatck, v2jupack, v2juaccnt, v2jucorrdc, v2juhcind,
           v2juncind, v2juhccomp, v2jucomp, v2jureview, v2clacfree, v2clrelig, v2cltort, v2clkill, v2cltrnslw, v2clrspct, v2clfmove,
           v2cldmovem, v2cldmovew, v2cldiscm, v2cldiscw, v2clslavem, v2clslavef, v2clstown, v2clprptym, v2clprptyw, v2clacjstm,
           v2clacjstw, v2clacjust, v2clsocgrp, v2clrgunev, v2svdomaut, v2svinlaut, v2svstterr, v2cseeorgs, v2csreprss, v2cscnsult,
           v2csprtcpt, v2csgender, v2csantimv, v2csrlgrep, v2csrlgcon, v2mecenefm, v2mecrit, v2merange, v2meharjrn, v2meslfcen, v2mebias,
           v2mecorrpt, v2pepwrses, v2pepwrsoc, v2pepwrgen, v2pepwrort, v2peedueq, v2pehealth)
dim(vdem_ivs) ## 8430  186

vdem_clean_data <- vdem_ivs %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate(is_jud = ifelse(is.na(v2x_jucon), 0, 1),
         is_leg = ifelse(v2lgbicam > 0, 1, 0),
         is_elec = ifelse(v2x_elecreg == 0, 0, 1),
         is_election_year = ifelse(!is.na(v2elirreg), 1, 0)) %>%
         fill(v2elrgstry) %>%
           fill(v2elvotbuy) %>%
           fill(v2elirreg) %>%
           fill(v2elintim) %>%
           fill(v2elpeace) %>%
           fill(v2elfrfair) %>%
           fill(v2elmulpar) %>%
           fill(v2elboycot) %>%
           fill(v2elaccept) %>%
           fill(v2elasmoff) %>%
           fill(v2elfrcamp) %>%
           fill(v2elpdcamp) %>%
           fill(v2elpaidig) %>%
           fill(v2elmonref) %>%
           fill(v2elmonden) %>%
           mutate(v2elrgstry = ifelse(is.na(v2elrgstry) & v2x_elecreg == 0, 0, v2elrgstry),
                  v2elvotbuy = ifelse(is.na(v2elvotbuy) & v2x_elecreg == 0, 0, v2elvotbuy),
                  v2elirreg = ifelse(is.na(v2elirreg) & v2x_elecreg == 0, 0, v2elirreg),
                  v2elintim = ifelse(is.na(v2elintim) & v2x_elecreg == 0, 0, v2elintim),
                  v2elpeace = ifelse(is.na(v2elpeace) & v2x_elecreg == 0, 0, v2elpeace),
                  v2elfrfair = ifelse(is.na(v2elfrfair) & v2x_elecreg == 0, 0, v2elfrfair),
                  v2elmulpar = ifelse(is.na(v2elmulpar) & v2x_elecreg == 0, 0, v2elmulpar),
                  v2elboycot = ifelse(is.na(v2elboycot) & v2x_elecreg == 0, 0, v2elboycot),
                  v2elaccept = ifelse(is.na(v2elaccept) & v2x_elecreg == 0, 0, v2elaccept),
                  v2elasmoff = ifelse(is.na(v2elasmoff) & v2x_elecreg == 0, 0, v2elasmoff),
                  v2elpaidig = ifelse(is.na(v2elpaidig) & v2x_elecreg == 0, 0, v2elpaidig),
                  v2elfrcamp = ifelse(is.na(v2elfrcamp) & v2x_elecreg == 0, 0, v2elfrcamp),
                  v2elpdcamp = ifelse(is.na(v2elpdcamp) & v2x_elecreg == 0, 0, v2elpdcamp),
                  v2elpdcamp = ifelse(is.na(v2elpdcamp) & v2x_elecreg == 0, 0, v2elpdcamp),
                  v2elmonref = ifelse(is.na(v2elmonref) & v2x_elecreg == 0, 0, v2elmonref),
                  v2elmonden = ifelse(is.na(v2elmonden) & v2x_elecreg == 0, 0, v2elmonden)) %>%#,
           ungroup() %>%
           mutate(v2x_jucon = ifelse(is_jud == 0, 0, v2x_jucon),
                  v2xlg_legcon = ifelse(is_leg == 0, 0, v2xlg_legcon),
                  v2elmonref = ifelse(is.na(v2elmonref) & is_elec == 1, 0, v2elmonref),
                  v2elmonden = ifelse(is.na(v2elmonden) & is_elec == 1, 0, v2elmonden),
                  v2ellocgov = ifelse(is.na(v2ellocgov) & country_name == "Singapore" & year == 2019, 0, v2ellocgov), ## Not sure why it's NA. All other years are 0
                  v2elrsthos = ifelse(is.na(v2elrsthos) & country_name == "Jamaica" & year == 2019, 0, v2elrsthos), ## Not sure why this is NA. All other years are 0
                  v2elrstrct = ifelse(is.na(v2elrstrct) & country_name == "Jamaica" & year == 2019, 1, v2elrstrct), ## Not sure why this is NA. All other years are 1
                  v2exhoshog = ifelse(is.na(v2exhoshog) & country_name == "Jamaica" & year == 2019, 0, v2exhoshog), ## Not sure why this is NA. All other years are 0
                  v2exhoshog = ifelse(is.na(v2exhoshog) & country_name == "Kazakhstan" & year == 2019, 0, v2exhoshog), ## Not sure why this is NA. All other years are 0
                  v2svstterr = ifelse(is.na(v2svstterr) & country_name == "South Yemen" & year == 1990, 97.4, v2svstterr), ## Not sure why this is NA. last year in the series, carry forward
                  v2svstterr = ifelse(is.na(v2svstterr) & country_name == "Republic of Vietnam" & year == 1975, 99, v2svstterr), ## Not sure why this is NA. last year in the series, carry forward
                  v2svstterr = ifelse(is.na(v2svstterr) & country_name == "German Democratic Republic" & year == 1990, 48, v2svstterr), ## Not sure why this is NA. last year in the series, carry forward
                  v2psoppaut = ifelse(is.na(v2psoppaut) & country_name == "Saudi Arabia" & between(year, 1970, 2019), -3.527593, v2psoppaut), ## Opposition parties are banned in Saudi Arabia. Going with the min score in the data (1970-2017)
                  v2psoppaut = ifelse(is.na(v2psoppaut) & country_name == "Kuwait" & between(year, 1970, 2019), -2.250289, v2psoppaut), ## Carry forward. has the same score 1970-2016
                  v2psoppaut = ifelse(is.na(v2psoppaut) & country_name == "Qatar" & between(year, 1971, 2019), -3.527593, v2psoppaut), ## Opposition parties are banned in Qatar. Going with the min score in the data (1970-2017)
                  v2psoppaut = ifelse(is.na(v2psoppaut) & country_name == "United Arab Emirates" & between(year, 1971, 2019), -3.527593, v2psoppaut), ## Opposition parties are banned in UAE. Going with the min score in the data (1970-2017)
                  v2psoppaut = ifelse(is.na(v2psoppaut) & country_name == "Oman" & between(year, 2000, 2019), -2.46780629, v2psoppaut), ## Carry forward. There are a handful of nominal opposition parties, but they are co-opted. No much changed after 1999...
                  v2lgqstexp = ifelse(is_leg == 0, 0, v2lgqstexp),
                  v2lginvstp = ifelse(is_leg == 0, 0, v2lginvstp),
                  v2lgotovst = ifelse(is_leg == 0, 0, v2lgotovst),
                  v2lgcrrpt = ifelse(is_leg == 0, 0, v2lgcrrpt),
                  v2lgoppart = ifelse(is_leg == 0, 0, v2lgoppart),
                  v2lgfunds = ifelse(is_leg == 0, 0, v2lgfunds),
                  v2lgdsadlobin = ifelse(is_leg == 0, 0, v2lgdsadlobin),
                  v2lglegplo = ifelse(is_leg == 0, 0, v2lglegplo),
                  v2lgcomslo = ifelse(is_leg == 0, 0, v2lgcomslo),
                  v2lgsrvlo =  ifelse(is_leg == 0, 0, v2lgsrvlo)) %>%
  select(country_name, country_text_id, gwcode, country_id, year, is_jud, is_leg, is_elec, is_election_year, everything())
dim(vdem_clean_data) ## 8430  190

vdem_clean_data_diff <- vdem_clean_data %>%
  group_by(country_id) %>%
  arrange(year) %>%
  mutate_at(vars(-c(country_name, country_text_id, gwcode, country_id, year, is_jud, is_leg, is_elec, is_election_year)), ~c(NA, diff(.))) %>%
  ungroup() %>%
  arrange(country_id, year) %>%
  select(country_name, country_text_id, gwcode, country_id, year, is_jud, is_leg, is_elec, is_election_year, everything())

names(vdem_clean_data_diff)[-c(1:9)] <- paste0("diff_year_prior_", names(vdem_clean_data)[-c(1:9)])
# naCountFun(vdem_clean_data_diff, END_YEAR)

vdem_data <- vdem_clean_data %>%
  left_join(vdem_clean_data_diff)
dim(vdem_data) ## 8430  371
# naCountFun(vdem_data, END_YEAR)

vDem_GW_data <- country_year_set %>%
  left_join(vdem_data) %>%
  group_by(gwcode) %>%
  arrange(year) %>%
  fill(2:length(.), .direction = "up") %>% # fills-in the NAs in the lagged vars for states created between 1970 and 2019...
  ungroup() %>%
  arrange(country_id, year)
dim(vDem_GW_data) ## 8120  371
# summary(vDem_GW_data)

# naCountFun(vDem_GW_data, END_YEAR + 1)
naCountFun(vDem_GW_data, END_YEAR)

write_csv(vDem_GW_data, "trafo-data/vdem_data_1968_on.csv")
