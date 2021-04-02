### Outputs the Mertens-Ravn quarterly tax rates as annual rates and gets Mertens_Ravn rates with GDP as denominator

data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'

source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))

shhh(library(dplyr))
shhh(library(data.table))

#quarterly nipa data
nipa_q <- fread(file.path(data_dir,"macro_invt_elasticities/Data/nipa_q_vals.csv"))
#annual NIPA data
nipa_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_fa_vals.csv"))
#nipa reference table
nipa_ref <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_fa_meta.csv"))

#romer romer data (from replication file, mertens-ravn shocks already added)
romerdata <- fread(file.path(data_dir, "macro_invt_elasticities/Data/romerromer_q.csv"))

#read in annual mertens-ravn (only contans PI shocks)
mertens_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/mertens_ravn_annual.csv"))
mertens_a <- mertens_a %>% rename(Year = DATES) %>% dplyr::select(c(Year, T_PI, m_PI, APITR, MPITR))

# dplyr::select nominal GDP and real GDP from BEA to annualize Romer-Romer series
nipa_a_gdp <- nipa_a %>% dplyr::select(c(date, T10103_1_A, T10105_1_A)) %>% rename(Year = date)

## Read in NIPA data c. 2008q1
nipa_2008_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/annual_nipa_circa_2008q1.csv"))

#romer vars to annualize
romer_vars <- c("SPENDNR", "COUNTNR","DEFICNR", "LONGRNR", "ENDOGNR", "EXOGENR", "SUMMANR", 
                "SPENDRETRO", "COUNTRETRO", "DEFICRETRO", "LONGRRETRO", "ENDOGRETRO", "EXOGERETRO", "SUMMARETRO",
                "SPENDPDV", "COUNTPDV", "DEFICPDV", "LONGRPDV", "ENDOGPDV", "EXOGEPDV", "SUMMAPDV")


### Annualize Romer-Romer tax variables

romer_ann <- romerdata %>% 
  dplyr::select(c(Date, c(all_of(romer_vars)))) %>% 
  mutate(Year = as.numeric(substr(as.character(Date), start = 1, stop = 4))) %>% dplyr::select(-c(Date)) %>% #create year variable from quarterly dates
  group_by(Year) %>%
  summarise(across(all_of(romer_vars), sum, na.rm = T)) %>% ungroup() %>%  # add up all tax shocks for each year
  left_join(nipa_a_gdp) %>%
  mutate_at(vars(romer_vars), funs(RATIO = ./(T10105_1_A*10^(-5)))) %>% dplyr::select(c(names(nipa_a_gdp),contains("RATIO"))) %>% #scale taxes by nominal GDP (after scaling nominal GDP to account for difference in magnitude)
  rename_with(~gsub("_", "", .x, fixed = TRUE), c(names(nipa_a_gdp),contains("RATIO"))) %>%
  dplyr::select(-c(T101031A, T101051A)) %>% left_join(nipa_2008_a, by = c("Year"))

##annualize mertens-ravn
#get corporate profits (1.12 line 13) less table 6.16 BCD 
nipa_q_corp <- nipa_q %>% dplyr::select(c(date, T11200_13_Q, T61600B_11_Q, T61600C_11_Q, T61600D_11_Q)) %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(fed_profits = ifelse(!is.na(T61600B_11_Q), T61600B_11_Q, ifelse(!is.na(T61600C_11_Q), T61600C_11_Q, T61600D_11_Q)),
         corp_profits = T11200_13_Q - fed_profits) %>% 
  #join with R-R and get nominal value of corporate tax shock by multipling rate by corporate tax base
  rename(Date = date) %>% left_join(romerdata, by = c("Date")) %>%
  mutate(corp_tax = (T_CI/100)*(corp_profits*10^-3),date = as.numeric(substr(as.character(Date), start = 1, stop = 4))) %>%
  #sum corporate tax shocks by year
  dplyr::select(c(date, corp_tax)) %>% group_by(date) %>% summarise(corp_tax = sum(corp_tax, na.rm = T)) %>% ungroup()

#now get M-R disaggregated corp tax shock in annual value
nipa_a_corp <- nipa_a %>% dplyr::select(c(date, T11200_13_A, T61600B_11_A, T61600C_11_A, T61600D_11_A)) %>%
  mutate(fed_profits = ifelse(!is.na(T61600B_11_A), T61600B_11_A, ifelse(!is.na(T61600C_11_A), T61600C_11_A, T61600D_11_A)),
         corp_profits = T11200_13_A - fed_profits) %>% left_join(nipa_q_corp) %>%
  mutate(T_CI = (10^5)*corp_tax/corp_profits) %>% # scale by 10^5 to get align magnitudes (10^3) and then get rate in percent
  rename(Year = date) %>%
  left_join(romer_ann) %>% #combine with R-R aggregate shocks
  left_join(mertens_a) # combine with personal income tax shock

rameypop <- fread(file.path(data_dir, "macro_invt_elasticities/Data/ramey_pop16.csv"))
rameypop <- rameypop %>% rename(Year = Date)

nipa_a_corp <- nipa_a_corp %>% left_join(rameypop, by = c("Year"))

write.csv(nipa_a_corp, file.path(data_dir, "macro_invt_elasticities/Data/romerromer_ann.csv"))

## Get Mertens-Ravn tax rates with GDP as denominator (for fair comparison with Romer-Romer)
#Corporate
nipa_q_gdp <- nipa_q %>% dplyr::select(c(date, T11200_13_Q, T61600B_11_Q, T61600C_11_Q, T61600D_11_Q,
                                     T20100_1_Q, T20100_17_Q, T30200_11_Q, T10105_1_Q))  %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(fed_profits = ifelse(!is.na(T61600B_11_Q), T61600B_11_Q, ifelse(!is.na(T61600C_11_Q), T61600C_11_Q, T61600D_11_Q)),
         corp_profits = T11200_13_Q - fed_profits,
         pitb = T20100_1_Q - T20100_17_Q + T30200_11_Q) %>% 
  #join with R-R and get nominal value of corporate tax shock by multipling rate by corporate tax base
  rename(Date = date) %>% left_join(romerdata, by = c("Date")) %>%
  mutate(corp_tax = (T_CI/100)*(corp_profits*10^-3),
         personal_tax = (T_PI/100)*(pitb*10^(-3)),
         T_CI_GDP = 100*corp_tax/(T10105_1_Q*10^-3),
         T_PI_GDP = 100*personal_tax/(T10105_1_Q*10^-3)) %>%
  dplyr::select(c(names(romerdata), T_CI_GDP, T_PI_GDP))

write.csv(nipa_q_gdp, file.path(data_dir, "macro_invt_elasticities/Data/romer_romer_placeholder.csv"))