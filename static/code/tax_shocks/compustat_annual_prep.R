###Prepare compustat annual data for regressions
data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'
source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))

## Define Vectors
company_var <- c("fyear",  "tic","conm", "gvkey", "cusip", "indfmt", "consol", "fic", "cik", "sic", "naicsh", "ipodate")

balance_sheet_var <- c("at", "lt", "seq","act","che", "rect", "ppegt","invt","aco","ppent", "ivaeq", "ivao", "intan", "ao","lct","dlc","ap","txp","lco","dltt","lo","txditc","mib","pstk","ceq","cstk","caps","re","tstk", "txdb")

income_statement_var <- c("ni", "sale", "cogs", "xsga", "oibdp", "dp", "xint", "nopi", "spi", "pi", "pidom", "pifo", "txt", "mii", "ib", "xido")

cash_flow_var <- c("ibc", "dpc", "xidoc", "txdc", "esubc", "sppiv", "fopo", "fopt", "capx", "aqc", "sppe", "ivch", "siv", "dlcch", "dltis", "dltr", "sstk", "prstkc", "dv", "fsrco", "fuseo", "wcapc")

misc_var <- c("xad", "xrd", "csho", "prcc_f", "pstkrv", "dvc", "dvp", "am")

#nipa reference table
nipa_ref <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_fa_meta.csv"))

## Read in dfs
#Raw compustat data downloaded from WRDS
compustat <- fread(file.path(data_dir, "macro_invt_elasticities/Data/compustat_annual.csv"), select = c(balance_sheet_var, company_var, income_statement_var, misc_var, cash_flow_var, "txpd"))

# BEA data
nipa_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_fa_vals.csv"))
# shocks from Romer-Romer and Mertens-Ravn
shocks <- fread(file.path(data_dir, "macro_invt_elasticities/Data/romerromer_ann.csv"))

#Firm-age data from Jay Ritter
firm_age <- fread(file.path(data_dir, "macro_invt_elasticities/Data/RITTER_AGE.csv"))

# Intan Q data from Peters and Taylor
intanq <- fread(file.path(data_dir,"macro_invt_elasticities/Data/peters_taylor_intan_q.csv"))

##Set up deflators and shocks
shocks <- shocks %>% dplyr::select(c(Year, T_CI:MPITR)) %>% rename(fyear = Year)
#base year 2012, deflator is for quantity GDP index. 37 corresponds to all assets, 40 to intangibles, 2 corresponds to stocks, 8 to flows
nipa_deflators <- nipa_a %>% dplyr::select(c(date, T10103_1_A, FAAt402_37_A, FAAt402_40_A, FAAt408_37_A,
                                             FAAt408_40_A)) %>% rename(fyear = date) %>%
  mutate(across(-c("fyear"), ~./100)) %>% # scale by 100
  left_join(shocks)

##Set up compustat
compustat <- compustat %>%
  #Remove financial services, only look at domestic firms, remove firms without tickers or
  #non-positive sales
  filter(indfmt != "FS" & fic == "USA" & !is.na(tic) & !is.na(sale) & sale >0 & tic != "" & (at > 0 | is.na(at)))

#Getting firm age
comp_age <- compustat %>%
  # Remove firms without a final stock price at the end of the year
  filter(!is.na(prcc_f) & !is.na(tic) & !is.na(fyear) & tic != "") %>%
  dplyr::select(tic, fyear, prcc_f) %>%
  #get the first year a firm had a final year stock price
  group_by(tic) %>% summarise(
    "first_yr" = min(fyear)
  ) %>% ungroup()



# convert Ritter IPO date to numeric (only want year so get rid of month/date)
firm_age <- firm_age %>% dplyr::select(c(CUSIP, Founding)) %>% rename(cusip = CUSIP) %>% filter(Founding > 1800)

# bring in firm birth year possibilities, then calculate firm age in order of precedence and clean NAICS codes
compustat <- compustat %>% 
  left_join(comp_age, by = "tic") %>% # bring in first year firm appears in compustat with a final stock price
  left_join(firm_age, by = "cusip") %>% # bring in Ritter age data based on IPO
  mutate(ipo_2 = as.numeric(substr(ipodate, start = 1, stop = 4)), # look at IPO date from Compustat
         age_priority = clean_ipo_date(Founding, first_yr, ipo_2), # calculate firm-age using the formula that if Ritter IPO exists, use that, otherwise first year firm appears in Compustat with final stock price, otherwise comp IPO date, otherwise first time in Compustat
         age = ifelse(fyear - age_priority >= 0, fyear - age_priority, NA)) %>% #age in each year given as the year minus firm birth year
  mutate(NAICS1 = substr(naicsh, start = 1, stop = 2), 
         NAICS = (clean_naics(NAICS1)) # group NAICS codes 31-32 into 33, etc (see code for function above)
         ) %>%
  dplyr::select(-c(Founding, first_yr, ipo_2, NAICS1)) %>%
  filter(!is.na(age)) %>%
  left_join(intanq)  


#Now get NAICS from SIC codes and create inflation-adjust variables. Deflate investment/capital
#variables using fixed asset deflator for non-res corp assets
compustat_sic <- compustat %>%
  dplyr::select(fyear, sic, tic, cik)
#sic-naics crosswalk
sic_naics_cw <- fread(file.path(data_dir,"macro_invt_elasticities/Data/sic_naics_crosswalk.csv"))

# vectorize variables accordig to deflator category
unadj_var <- c("tic", "fyear", "NAICS", "conm", "gvkey", "cusip", "cik", "indfmt", "fic", "cik", "age", "csho", "consol", "ipodate")
adjust_var_fa_stock <- c("ppent", "ppegt") #tan stock deflator
adjust_var_intan_stock <- c("intan", "K_int_Know", "K_int_Org", "K_int") #intan stock deflator
adjust_var_invt <- c("capx") #invt deflator (physical invt)
adjust_var_invt_intan <- c("xrd") #intan int deflator
adjust_var_gdp <- c(names(dplyr::select(compustat, -c(all_of(unadj_var)), -c(all_of(adjust_var_fa_stock)),
                                        -c(all_of(adjust_var_intan_stock)), -c(all_of(adjust_var_invt)),
                                        -c(all_of(adjust_var_invt_intan))
                                        ))) #gdp quantity deflator (1.3.1)

#now get rid of NA values in either sic or naics.  We do not care about a NAICS/SIC code if there is
#no correspondence. Also get rid of extraneous columns and convert SIC to 3 digit codes (that's the
#level of sensitivity I care about)
sic_naics_cw <- sic_naics_cw %>%
  filter(!is.na(SIC) & !is.na(NAICS)) %>%
  dplyr::select(NAICS, SIC) %>%
  mutate(SIC = substr(SIC, 1, 3), SIC = as.numeric(SIC))

#now join compustat_sic w/ sic_naics_cw. we only care about years prior to 1985 since there are no NAICS
#values before then. 
compustat_sic <- compustat_sic %>%
  mutate(SIC = substr(sic, 1, 3), SIC = as.numeric(SIC)) %>%
  filter(fyear < 1985) %>%
  left_join(., sic_naics_cw, by = c("SIC")) %>% 
  mutate(NAICS2 = substr(NAICS, 1, 2), NAICS2 = as.numeric(NAICS2), NAICS_s = (clean_naics(NAICS2)), id = paste(tic, fyear, sep = "-")) %>%
  dplyr::select(-c(SIC, NAICS, NAICS2)) %>%
  distinct(id, .keep_all = TRUE) %>%
  dplyr::select(-c(id,cik, sic))

#bring secondary NAICS values into Compustat 
compustat <- compustat %>% left_join(compustat_sic, by = c("fyear", "tic")) %>%
  mutate(NAICS = naic_all(NAICS, NAICS_s))

#extract ages and NAICS to move to quarterly data so we don't have to repeat process
comp_age_naics <- compustat %>% dplyr::select(c(fyear, gvkey, age, NAICS))
write.csv(comp_age_naics, file.path(data_dir,"macro_invt_elasticities/Data/comp_age_naics.csv"))

compustat <- compustat %>%
  filter(!is.na(NAICS) | NAICS != 52 | NAICS != 53 | NAICS != 22) %>% 
  left_join(nipa_deflators) %>%
  mutate(
    K_int = ifelse(is.na(K_int), 0, K_int),
    K_int_Know = ifelse(is.na(K_int_Know), 0, K_int_Know),
    K_int = ifelse(is.na(K_int), 0, K_int)
  ) %>%
  group_by(fyear) %>% mutate(
    K_int = Winsorize(K_int, probs = c(0, .99), na.rm = TRUE),
    K_int_Know = Winsorize(K_int_Know,probs = c( 0, .99), na.rm = TRUE),
    K_int_Org = Winsorize(K_int_Org, probs = c(0, .99), na.rm = TRUE),
  ) %>% ungroup() %>%
  mutate(across(c(adjust_var_gdp), list(adj =  ~./T10103_1_A))) %>% # gdp deflator
  mutate(across(c(adjust_var_fa_stock), list(adj =  ~./FAAt402_37_A))) %>% # physical stock deflator
  mutate(across(c(adjust_var_intan_stock), list(adj =  ~./FAAt402_40_A))) %>% # intan stock deflator
  mutate(across(c(adjust_var_invt), list(adj =  ~./FAAt408_37_A))) %>% #physical invt deflator
  mutate(across(c(adjust_var_invt_intan), list(adj =  ~./FAAt408_40_A))) %>%  # intan invt deflator
  mutate(id = paste(tic, fyear, sep = "-"),
         leverage = ifelse(seq_adj != 0 & !is.na(seq_adj), (dltt_adj + dlc_adj)/seq_adj, NA),  # calculate leverage
         tobins_q = (at_adj + csho*prcc_f - ceq_adj)/at_adj, # calculate tobin's q 
         mnc_exp = ifelse(pifo/pi >= .02139, 1, 0), # cutoff for MNC
         mktcap = prcc_f_adj*csho, #calculate mktcap
         liquidity = che_adj/at_adj,
         net_leverage = (dlc_adj+ dltt_adj - (act - lct))/at, # net leverage
         net_current_assets = (act - lct)/at, #net current assets
         org_delta_rate = .2, # set organizational capital depreciation rate 
         rd_delta_rate = .15, # set Know capital depreciation rate
         Know_intensity = K_int_Know/(ppent + K_int),
         org_intensity = K_int_Org/(ppent + K_int),
         intangible_intensity = K_int/(ppent+K_int),
         tangible_intensity = 1-intangible_intensity,
         effective_rate = eff_rate(pi, txt), 
         roa = pi/at * 100 #Return on assets
  ) %>%
  constrained_index(frequency = "A") 

#get list of firms with MNC status
list_tic_mnc <- compustat %>% filter(mnc_exp == 1) %>% 
  dplyr::select(tic) %>% distinct() %>% 
  mutate(mnc_imp = 1)


#set to MNC status for firms implicitly (if they have it in year t, they also have it in t-1, etc)
#set MNC status to zero otherwise
#big firm defined by mktcap > 100m. Later, define this as greater than median of marketcap for given year
# use this chunk to add heterogeneity (examine profitability, leverage, etc)
compustat <- compustat %>% 
  left_join(list_tic_mnc, by = c("tic")) %>%
  mutate(mnc = ifelse(!is.na(mnc_exp), mnc_exp, ifelse(!is.na(mnc_imp), mnc_imp, 0))) %>%
  mutate(big_firm_mktcap = ifelse(mktcap > 100, 1, 0)) %>%
  group_by(fyear) %>%
  mutate(big_firm_yearly_mkt <- ifelse(mktcap > median(mktcap, na.rm = T), 1, 0)) %>%
  ungroup()


#write mnc to csv so that it can be used in quarterly data
comp_mnc <- compustat %>% dplyr::select(c(mnc, gvkey,fyear, kz_threshold, Know_intensity, org_intensity, intangible_intensity, kz_index, kz_con, hp_index, hp_con)) 
write.csv(comp_mnc, file.path(data_dir,"macro_invt_elasticities/Data/mnc.csv"))

#Linear Interpolation
max_gap <- 1 # set maximum number of value to interpolate between values
interp_vars <- c("tobins_q", "leverage", "at", "dp", "dp_adj", "am_adj", "xrd", "am", "liquidity", "net_current_assets") # set variables to interpolate

yrs_invest <- 8  #filter at least yrs_invest of investment
na_vars <- c("n_invt_phy", "sale", "at_interp") #necessary variables that cannot be NA

log_vars <- c("sale", "sale_adj", "ppent", "ppent_adj", "dp_adj_interp", "g_invt_org", "g_invt_org_adj", "g_invt_phy", "g_invt_phy_adj", "g_invt_know", "g_invt_know_adj", "g_invt_int", "g_invt_int_adj", "g_invt_total", "g_invt_total_adj") # set variables to log
growth_vars <- c("sale_log", "sale_adj_log") # set variables to grow

compustat <- compustat %>% 
  interp_df(interp_vars) %>%
  filter(at > 0 & ppent > 0 & leverage >= 0 & leverage < 10 & liquidity > 0 & abs(net_current_assets) <= 10) %>%
  group_by(fyear) %>%
  mutate(
    ppent = Winsorize(ppent, probs = c(.005, .995), na.rm = TRUE),
    ppent_adj = Winsorize(ppent_adj, probs = c(.005, .995), na.rm = TRUE),
    dp = Winsorize(dp_interp, probs = c(0.005, .995), na.rm = TRUE),
    dp_adj = Winsorize(dp_adj_interp, probs = c(0.005, .995), na.rm = TRUE),
    
    am_adj_interp = Winsorize(am_adj_interp, probs = c(0.005, .995), na.rm = TRUE),
    am = Winsorize(am_interp, probs = c(0.005, .995), na.rm = TRUE)
    
  ) %>%
  ungroup() %>%
  group_by(gvkey) %>%
  mutate(
    am = ifelse(is.na(am_interp), 0, am),
    
    n_invt_phy = ppent - dplyr::lag(ppent, n = 1L), # implicitly net investment in physical capital
    n_invt_phy_adj = ppent_adj - dplyr::lag(ppent_adj, n = 1L), # implicitly net investment in physical capital
    
    delta_phy = dp - am, # physical capital depreciation
    delta_phy_adj = dp_adj_interp - am_adj_interp,
    g_invt_phy = n_invt_phy + dp - am, # physical gross invt
    g_invt_phy_adj = n_invt_phy_adj + delta_phy_adj,
    n_invt_know = K_int_Know - dplyr::lag(K_int_Know, n = 1L), # net investment for Know capital
    n_invt_know_adj = K_int_Know_adj - dplyr::lag(K_int_Know_adj, n = 1L),
    dep_know = rd_delta_rate*dplyr::lag(K_int_Know, n = 1L), # total depreciation for Know capital
    dep_know_adj = rd_delta_rate*dplyr::lag(K_int_Know_adj, n = 1L), # total depreciation for Know capital
    g_invt_know = n_invt_know + dep_know, # gross investment Know capital
    g_invt_know_adj = n_invt_know_adj + dep_know_adj, # gross investment Know capital
    n_invt_org = K_int_Org - dplyr::lag(K_int_Org, n = 1L), # net investment for organizational capital
    n_invt_org_adj = K_int_Org_adj - dplyr::lag(K_int_Org_adj, n = 1L), # net investment for organizational capital
    dep_org = org_delta_rate*dplyr::lag(K_int_Org, n = 1L), # total depreciation for organizational capital
    dep_org_adj = org_delta_rate*dplyr::lag(K_int_Org_adj, n = 1L), # total depreciation for organizational capital
    g_invt_org = n_invt_org + dep_org, # gross investment organizational capital
    g_invt_org_adj = n_invt_org_adj + dep_org_adj, # gross investment organizational capital
    n_invt_int = K_int - dplyr::lag(K_int, n = 1L), # net investment in intangible capital
    n_invt_int_adj = K_int_adj - dplyr::lag(K_int_adj, n = 1L), # net investment in intangible capital
    dep_int = dep_know + dep_org, # total intangible depreciation
    dep_int_adj = dep_know_adj + dep_org_adj, # total intangible depreciation
    g_invt_int = n_invt_int + dep_int, #gross investment intangible capital
    g_invt_int_adj = n_invt_int_adj + dep_int_adj, #gross investment intangible capital
    k_total = ppent + K_int, # total capital
    k_total_adj = ppent_adj + K_int_adj, # total capital
    n_invt_total = k_total - dplyr::lag(k_total, n = 1L), # net investment
    n_invt_total_adj = k_total_adj - dplyr::lag(k_total_adj, n = 1L), 
    dep_total = dp - am + dep_int, #total depreciation spend
    dep_total_adj = dp_adj - am_adj + dep_int_adj,
    g_invt_total = n_invt_total + dep_total, # gross investment,
    g_invt_total_adj = n_invt_total_adj + dep_total_adj
  ) %>%
  ungroup() %>%
  log_growth_df("fyear", log_vars, growth_vars) %>%
  filter(abs(sale_log_growth < 1) | is.na(sale_log_growth)) %>%
  na_filter_a(na_vars) %>%
  group_by(gvkey) %>%
  mutate(
    n_invt_rate_phy = n_invt_phy/dplyr::lag(ppent, n = 1L), # net physical investment rate
    n_invt_rate_phy = Winsorize(n_invt_rate_phy, probs = c(.005, .995), na.rm = TRUE),
    n_invt_rate_phy_adj = n_invt_phy_adj/dplyr::lag(ppent_adj, n = 1L), # net physical investment rate
    n_invt_rate_phy_adj = Winsorize(n_invt_rate_phy_adj, probs = c(.005, .995), na.rm = TRUE),    
    delta_rate_phy = delta_phy/dplyr::lag(ppent, n = 1L), # physical capital dep rate
    delta_rate_phy = Winsorize(delta_rate_phy, probs = c(.005, .995), na.rm = TRUE),
    delta_rate_phy_adj = delta_phy_adj/dplyr::lag(ppent_adj, n = 1L), # physical capital dep rate
    delta_rate_phy_adj = Winsorize(delta_rate_phy_adj, probs = c(.005, .995), na.rm = TRUE),
    
    g_invt_rate_phy = n_invt_rate_phy + delta_rate_phy, # gross investment  rate physical
    g_invt_rate_phy = Winsorize(g_invt_rate_phy, probs = c(.005, .995), na.rm = TRUE),
    g_invt_rate_phy_adj = n_invt_rate_phy_adj + delta_rate_phy_adj, # gross investment  rate physical
    g_invt_rate_phy_adj = Winsorize(g_invt_rate_phy_adj, probs = c(.005, .995), na.rm = TRUE),
    
    delta_total = dep_total/dplyr::lag(k_total, n = 1L), # total depreciation rate
    delta_total = Winsorize(delta_total, probs = c(.005, .995), na.rm = TRUE),
    delta_total_adj = dep_total_adj/dplyr::lag(k_total_adj, n = 1L), # total depreciation rate
    delta_total_adj = Winsorize(delta_total_adj, probs = c(.005, .995), na.rm = TRUE),
    
    
    n_invt_rate_total = n_invt_total/dplyr::lag(k_total, n = 1L), # net investment rate
    n_invt_rate_total = Winsorize(n_invt_rate_total, probs = c(.005, .995), na.rm = TRUE),
    n_invt_rate_total_adj = n_invt_total_adj/dplyr::lag(k_total_adj, n = 1L), # net investment rate
    n_invt_rate_total_adj = Winsorize(n_invt_rate_total_adj, probs = c(.005, .995), na.rm = TRUE),
    
    g_invt_rate_total = n_invt_rate_total + delta_total, # gross investment rate
    g_invt_rate_total = Winsorize(g_invt_rate_total, probs = c(.005, .995), na.rm = TRUE),
    g_invt_rate_total_adj = n_invt_rate_total_adj + delta_total_adj, # gross investment rate
    g_invt_rate_total_adj = Winsorize(g_invt_rate_total_adj, probs = c(.005, .995), na.rm = TRUE),
    
    n_invt_rate_know = ifelse(dplyr::lag(K_int_Know, n = 1L) == 0, 0, n_invt_know/dplyr::lag(K_int_Know, n = 1L)), # net investment rate for Know capital
    n_invt_rate_know = Winsorize(n_invt_rate_know, probs = c(0, .99), na.rm = TRUE),
    n_invt_rate_know_adj = ifelse(dplyr::lag(K_int_Know_adj, n = 1L) == 0, 0, n_invt_know_adj/dplyr::lag(K_int_Know_adj, n = 1L)), # net investment rate for Know capital
    n_invt_rate_know_adj = Winsorize(n_invt_rate_know_adj, probs = c(0, .99), na.rm = TRUE),
    
    
    g_invt_rate_know = n_invt_rate_know + rd_delta_rate, # gross investment rate for Know capital
    g_invt_rate_know = Winsorize(g_invt_rate_know, probs = c(0, .99), na.rm = TRUE),
    g_invt_rate_know_adj = n_invt_rate_know_adj + rd_delta_rate, # gross investment rate for Know capital
    g_invt_rate_know_adj = Winsorize(g_invt_rate_know_adj, probs = c(0, .99), na.rm = TRUE),
    
    n_invt_rate_org = ifelse(dplyr::lag(K_int_Org, n = 1L) == 0, 0, n_invt_org/dplyr::lag(K_int_Org, n = 1L)),  # net investment rate for organizational capital
    n_invt_rate_org = Winsorize(n_invt_rate_org, probs = c(.005, .995), na.rm = TRUE),
    n_invt_rate_org_adj = ifelse(dplyr::lag(K_int_Org_adj, n = 1L) == 0, 0, n_invt_org_adj/dplyr::lag(K_int_Org_adj, n = 1L)),  # net investment rate for organizational capital
    n_invt_rate_org_adj = Winsorize(n_invt_rate_org_adj, probs = c(.005, .995), na.rm = TRUE),
    
    
    g_invt_rate_org = n_invt_rate_org + org_delta_rate, #gross investment rate for organizational capital
    g_invt_rate_org = Winsorize(g_invt_rate_org, probs = c(.005, .995), na.rm = TRUE),
    g_invt_rate_org_adj = n_invt_rate_org_adj + org_delta_rate, #gross investment rate for organizational capital
    g_invt_rate_org_adj = Winsorize(g_invt_rate_org_adj, probs = c(.005, .995), na.rm = TRUE),
    
    
    n_invt_rate_int = ifelse(dplyr::lag(K_int, n = 1L)== 0 , 0,  n_invt_int/dplyr::lag(K_int, n = 1L)), # net investment rate intangible capital
    n_invt_rate_int = Winsorize(n_invt_rate_int, probs = c(0, .99), na.rm = TRUE),
    n_invt_rate_int_adj = ifelse(dplyr::lag(K_int_adj, n = 1L)== 0 , 0,  n_invt_int_adj/dplyr::lag(K_int_adj, n = 1L)), # net investment rate intangible capital
    n_invt_rate_int_adj = Winsorize(n_invt_rate_int_adj, probs = c(0, .99), na.rm = TRUE),
    
    
    dep_int_rate = dep_int/dplyr::lag(K_int, n = 1L), # depreciation rate for intangible capital
    dep_int_rate = Winsorize(dep_int_rate, probs = c(.005, .995), na.rm = TRUE),
    dep_int_rate_adj = dep_int_adj/dplyr::lag(K_int_adj, n = 1L), # depreciation rate for intangible capital
    dep_int_rate_adj = Winsorize(dep_int_rate_adj, probs = c(.005, .995), na.rm = TRUE),
    
    g_invt_rate_int = n_invt_rate_int + dep_int_rate, # gross investment rate intangible capital
    g_invt_rate_int = Winsorize(g_invt_rate_int, probs = c(.005, .995), na.rm = TRUE),
    g_invt_rate_int_adj = n_invt_rate_int_adj + dep_int_rate_adj, # gross investment rate intangible capital
    g_invt_rate_int_adj = Winsorize(g_invt_rate_int_adj, probs = c(.005, .995), na.rm = TRUE),
    
    
    div_payer = ifelse(dvc > 0, 1, 0),
    high_intan = ifelse(intangible_intensity > 0.75, 1, 0),
    high_know = ifelse(Know_intensity > 0.5, 1, 0),
    high_org = ifelse(org_intensity > 0.5, 1, 0),
    highleverage = ifelse(leverage > 1, 1, 0),
  ) %>% ungroup() %>%
  filter(!is.na(n_invt_rate_phy) & !is.na(g_invt_rate_phy) & !is.na(g_invt_rate_total) & !is.na(n_invt_rate_total)) %>%
  mutate(
    cont_id = paste(gvkey, grp, sep = "-"), #create new ID variable because not all periods are continuous, e.g., Apple could have a 1980-1990 and a 1990.5-2005 period
  ) %>%
  high_roa(roa = roa, datevar = fyear, threshold = .75) %>%
  group_by(fyear) %>%
  mutate(
    big_firm = ifelse(at > quantile(at, .75, na.rm = T), 1, 0)
  ) %>%
  ungroup()

write.csv(compustat, file.path(data_dir,"macro_invt_elasticities/Data/compustat_annual_prepped.csv"))
