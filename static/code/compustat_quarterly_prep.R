### Prepares quarterly compustat data for panel regressions and Jorda local projections
data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'

source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))

## Define Vectors
company_var <- c("fyearq", "fqtr", "datadate", "datafqtr", "datacqtr" , "tic","conm", "gvkey", "cusip", "indfmt", "consol", "fic", "cik","ipodate")

other_var <- c("dvy", "dvpy", "atq", "ibq", "txdiq", "xsgaq", "oancfy", "cogsq", "dpq", "xintq", "rectq", "invtq", "saleq", "ppegtq", "ppentq", "capxy", "piq", "piy", "txtq", "txty", "prccq", "cshoq", "ibcy", "actq", "dlttq", "dlcq", "seqq", "xrdq", "dvpq", "xrdy", "ceqq", "ltq", "lltq", "lctq", "cheq")

ytd_vars <- c("ibcy", "capxy", "dvy", "dvpy", "oancfy")

## Read in dfs

#bring in Compustat annual to get firm age and deflator
compustat_ann <- fread(file.path(data_dir, "macro_invt_elasticities/Data/compustat_annual_prepped.csv"), select = c("fyear", "am", "gvkey"))
comp_age_naics <- fread(file.path(data_dir, "macro_invt_elasticities/Data/comp_age_naics.csv"), select = c("fyear", "gvkey", "age", "NAICS"))
comp_mnc <- fread(file.path(data_dir, "macro_invt_elasticities/Data/mnc.csv"))
compustat_ann <- compustat_ann %>% left_join(comp_age_naics) %>% left_join(comp_mnc) %>% rename(fyearq = fyear)

#main compustat quarterly file.
compustat <- fread(file.path(data_dir, "macro_invt_elasticities/Data/compustat_quarterly.csv"), select = c(company_var, other_var))

#bring in shocks from mertens-ravn and romer-romer
shocks <- fread(file.path(data_dir,"macro_invt_elasticities/Data/romerromer_q.csv"))

# Bring in NIPA deflators and scale by 10^-2 and merge with shocks
nipa_q <- fread(file.path(data_dir,"macro_invt_elasticities/Data/nipa_q_vals.csv"), select = c("date", "T10103_1_Q", "T11400_17_Q", "T11400_41_Q"))
nipa_q <- nipa_q %>% mutate(T10103_1_Q = T10103_1_Q/100, gva_deflator = T11400_17_Q/T11400_41_Q) %>% rename(Date = date) %>%
  left_join(shocks) %>% rename(date = Date)
#annual deflators
nipa_a <- fread(file.path(data_dir,"macro_invt_elasticities/Data/nipa_fa_vals.csv"), select = c("date", "FAAt402_37_A", "FAAt402_40_A", "FAAt408_37_A","FAAt408_40_A"))
nipa_a <- nipa_a %>% dplyr::select(c(date, FAAt402_37_A, FAAt402_40_A, FAAt408_37_A,
                                                       FAAt408_40_A)) %>% rename(fyearq = date) %>%
  mutate(across(-c("fyearq"), ~./100))  # scale by 100


compustat <- compustat %>%
  filter(indfmt != "FS" & fic == "USA" & !is.na(tic) & !is.na(saleq) & saleq > 0 & tic != "" & atq > 0 ) %>%
  left_join(compustat_ann, by = c("gvkey", "fyearq")) %>%
  mutate(am = ifelse(!is.na(am), am/4, 0), # amortization variable unavailable quarterly, so divide annual by 4
         date = quarterly(datafqtr)) %>% # create quarterly format in the form of 1950.00 = 1950Q1
  left_join(nipa_q, by = c("date")) %>% # bring in quarterly deflator and shocks
  left_join(nipa_a, by = c("fyearq")) %>% # join with annual deflators
  left_join(comp_age_naics, by = c("gvkey", "age", "NAICS")) # join with age var


# set variables to adjust
unadj_var <- c(company_var, "age", "NAICS")
adjust_var_fa_stock <- c("ppentq", "ppegtq") #tan stock deflator
adjust_var_intan_stock <- c() #intan stock deflator
adjust_var_invt <- c("capxy") #invt deflator (physical invt)
adjust_var_invt_intan <- c("xrdq") #intan int deflator
adjust_var_gdp <- c(names(dplyr::select(compustat, -c(all_of(unadj_var)), -c(all_of(adjust_var_fa_stock)),
                                        -c(all_of(adjust_var_intan_stock)), -c(all_of(adjust_var_invt)),
                                        -c(all_of(adjust_var_invt_intan)))), "oancfy_q") #gdp quantity deflator (1.3.1)
  
#Linear Interpolation
max_gap <- 1 # set maximum number of value to interpolate between values
interp_vars <- c("ppentq", "ppentq_adj", "leverage", "dpq", "dpq_adj", "xrdq_adj",
                 "atq", "atq_adj", "liquidity", "net_current_assets", "tobins_q",
                 "cogsq_adj", "xsgaq_adj", "invtq_adj", "rectq_adj", "oancfy_q_adj", "xintq_adj") # set variables to interpolate

yrs_invest <- 20  #filter at least quarters of of investment
na_vars <- c("n_invt_phy", "dpq_interp", "g_invt_phy_adj_log", "tobins_q_interp") #necessary variables that cannot be NA
#n_observ <- 20 #set minimum number of observations for a firm

#vars to log-transform and get in terms of growth
log_vars <- c("saleq", "saleq_adj","atq", "atq_adj", "g_invt_phy", "g_invt_phy_adj")
growth_vars <- c("saleq_log", "saleq_adj_log")

compustat <- compustat %>% 
  filter(date > 1974.5 & date < 2007) %>%
  ytd(ytd_vars) %>%
  filter(!is.na(NAICS) & NAICS != 52 & NAICS != 53 & NAICS != 22) %>% 
  mutate(across(c(all_of(adjust_var_gdp)), list(adj =  ~./gva_deflator))) %>% # gdp deflator
  mutate(across(c(all_of(adjust_var_fa_stock)), list(adj =  ~./gva_deflator))) %>% # physical stock deflator
  mutate(across(c(all_of(adjust_var_intan_stock)), list(adj =  ~./gva_deflator))) %>% # intan stock deflator
  mutate(across(c(all_of(adjust_var_invt)), list(adj =  ~./gva_deflator))) %>% #physical invt deflator
  mutate(across(c(all_of(adjust_var_invt_intan)), list(adj =  ~./gva_deflator))) %>%  # intan invt deflator
  ungroup() %>%
  mutate(leverage = ifelse(seqq_adj != 0, (dlttq_adj + dlcq_adj) / (seqq_adj), NA), 
         tobins_q = (atq_adj + cshoq*prccq_adj - ceqq_adj)/atq_adj,
         mktcap = prccq_adj*cshoq,
         xrd_sale = xrdq_adj/saleq_adj,
         xrd_sale_imp = xrd_select(xrdq_adj, saleq_adj, xrd_sale),
         xrd_dum = ifelse(xrd_sale_imp > .05, 1, 0), # if ratio of R&D spend to sales > .05, 1
         big_firm = ifelse(mktcap > 100, 1, 0), # if mktcap > 100m, big firm
         liquidity = cheq_adj/atq_adj,
         net_leverage = (dlcq_adj+ dlttq_adj - (actq_adj - lctq_adj))/atq_adj,
         net_current_assets = (actq_adj - lctq_adj)/atq_adj, #net current assets
         highleverage = ifelse(leverage > 1, 1, 0),
         effective_rate = eff_rate(piq, txtq)
         ) %>% 
  interp_df(interp_vars) %>%
  filter(atq > 0 & ppentq > 0 & leverage >= 0 & leverage < 10 & liquidity > 0 & abs(net_current_assets) <= 10)  %>%
  group_by(fyearq) %>%
  mutate(
    ppentq_interp = Winsorize(ppentq_interp, probs = c(.005, .995), na.rm = TRUE),
    dpq_interp = Winsorize(dpq_interp, probs = c(.005, .995), na.rm = TRUE),
    am = Winsorize(am, probs = c(.005, .995), na.rm = TRUE),
    ppentq_adj_interp = Winsorize(ppentq_adj_interp, probs = c(.005, .995), na.rm = TRUE),
    dpq_adj_interp = Winsorize(dpq_adj_interp, probs = c(.005, .995), na.rm = TRUE),
    am_adj = Winsorize(am_adj, probs = c(.005, .995), na.rm = TRUE)
  ) %>%
  ungroup() %>% group_by(gvkey) %>%
  mutate(
    n_invt_phy = ppentq_interp - dplyr::lag(ppentq_interp, n = 1L), # same as above but using nominal variables
    n_invt_phy_adj = ppentq_adj_interp - dplyr::lag(ppentq_adj_interp, n = 1L), # same as above but using nominal variables
    delta_phy_adj = dpq_adj_interp - am_adj,
    delta_phy = dpq_interp - am,
    n_invt_phy = Winsorize(n_invt_phy, probs =c(0.005,0.995), na.rm = TRUE), # same as above but using nominal variables
    n_invt_phy_adj = Winsorize(n_invt_phy_adj, probs =c(0.005,0.995), na.rm = TRUE), # same as above but using nominal variables
    delta_phy = Winsorize(delta_phy, probs =c(0.005,0.995), na.rm = TRUE), # same as above but using nominal variables
    delta_phy_adj = Winsorize(delta_phy_adj, probs =c(0.005,0.995), na.rm = TRUE), # same as above but using nominal variables
    g_invt_phy = n_invt_phy + delta_phy,
    g_invt_phy_adj = n_invt_phy_adj + delta_phy_adj,
    xrdq_adj_interp = ifelse(is.na(xrdq_adj_interp), 0, xrdq_adj_interp),
    xrd_intensity = xrdq_adj_interp/saleq_adj
  ) %>% ungroup() %>%
  log_growth_df(datevar = "date", all_of(log_vars), all_of(growth_vars)) %>%
  filter(abs(saleq_log_growth) < 1 | is.na(saleq_log_growth)) %>%# %>% #only positive sales, sales growth between -100 and 100% and net current assets as a share of total assets between -10 and 10
  na_filter_q(c(all_of(na_vars))) %>%
  mutate(
    cont_id = paste(gvkey, grp, sep = "-"), #create new ID variable because not all periods are continuous, e.g., Apple could have a 1980-1990 and a 1990.5-2005 period
  ) %>%
  group_by(cont_id) %>%
  mutate(
    n_invt_rate_phy = n_invt_phy/dplyr::lag(ppentq_interp, n = 1L),
    n_invt_rate_phy = Winsorize(n_invt_rate_phy, probs = c(.005,.995), na.rm = TRUE),
    n_invt_rate_phy_adj = n_invt_phy_adj/dplyr::lag(ppentq_adj_interp, n = 1L),
    n_invt_rate_phy_adj = Winsorize(n_invt_rate_phy_adj, probs = c(.005,.995), na.rm = TRUE),
    delta_rate_phy = delta_phy/dplyr::lag(ppentq_interp, n = 1L),
    delta_rate_phy = Winsorize(delta_rate_phy, probs = c(.005,.995), na.rm = TRUE),
    delta_rate_phy_adj = delta_phy_adj/dplyr::lag(ppentq_adj_interp, n = 1L),
    delta_rate_phy_adj = Winsorize(delta_rate_phy_adj, probs = c(.005,.995), na.rm = TRUE),
    g_invt_rate_phy = n_invt_rate_phy + delta_rate_phy,
    g_invt_rate_phy = Winsorize(g_invt_rate_phy, probs = c(.005, .995), na.rm = T),
    g_invt_rate_phy_adj = n_invt_rate_phy_adj + delta_rate_phy_adj,
    g_invt_rate_phy_adj = Winsorize(g_invt_rate_phy_adj, probs = c(.005, .995), na.rm = T),
    g_invt_phy = Winsorize(g_invt_phy, probs = c(.005, .995), na.rm = T),
    g_invt_phy_log = Winsorize(g_invt_phy_log, probs = c(.005, .995), na.rm = T),
    g_invt_phy_adj = Winsorize(g_invt_phy_adj, probs = c(.005, .995), na.rm = T),
    g_invt_phy_adj_log = Winsorize(g_invt_phy_adj_log, probs = c(.005, .995), na.rm = T),
    roa = piq/atq * 100,
    roa = Winsorize(roa, probs = c(0.005, .995), na.rm = TRUE),
    div_payer = ifelse(dvy - dvpy > 0 & !is.na(dvy - dvpy), 1, 0), # Sikes 2017
    div = dvy - dvpy
    ) %>%
  ungroup() %>%
  filter(!is.na(n_invt_rate_phy) & !is.na(g_invt_rate_phy)) %>%
  constrained_index(frequency = "Q") %>%
  high_roa(roa = roa, datevar = date, threshold = .9) %>%
  group_by(date) %>%
  mutate(
    big_firm = ifelse(atq >= quantile(atq, .9, na.rm = T), 1, 0)
  ) %>% ungroup() %>%
  group_by(fyearq) %>%
  mutate(
    high_intan = ifelse(intangible_intensity >  0.9, 1, 0),
    high_know = ifelse(Know_intensity >  0.66, 1, 0),
    high_org = ifelse(org_intensity > 0.66, 1, 0)
  ) %>% ungroup() %>%
  mutate(
    T_CI_NEG = ifelse(T_CI  < 0, T_CI, 0),
    T_CI_POS = ifelse(T_CI > 0, T_CI, 0),
    young_firm = ifelse(age < 15, 1, 0),
    debt = dlcq_adj +dlttq_adj,
    ebitda = saleq_adj - cogsq_adj_interp - xsgaq_adj_interp,
    cash_flow = (oancfy_q_adj_interp + xintq_adj_interp )/ atq_adj_interp,
    collateral = ppentq_adj_interp + invtq_adj_interp + rectq_adj_interp
  )# %>%
  #dplyr::select(c(date, T_CI, T_CI_NEG, T_CI_POS, EXOGENRRATIO, T_PI,  g_invt_phy_adj_log, cont_id, fqtr, leverage, liquidity, NAICS, high_intan, high_org, high_roa, high_know, 
     #             saleq_adj_log_growth, ppentq_adj_interp, atq_adj, highleverage, tobins_q, hp_con, kz_con, mnc, div_payer, young_firm, big_firm, debt, collateral, cash_flow, ebitda,
          #        oancfy_q_adj_interp, xintq_adj_interp, invtq_adj_interp, rectq_adj_interp, cogsq_adj_interp, xsgaq_adj_interp, saleq, atq_interp, atq, seqq, seqq_adj))

write.csv(compustat, file.path(data_dir, "macro_invt_elasticities/Data/compustat_quarterly_prepped.csv"))



## Summary Stats