## Panel Regressions. This script runs different specifications of panel regressions. 

data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'

source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))

### Quarterly CI

compustat <- fread(file.path(data_dir,"macro_invt_elasticities/Data/compustat_quarterly_prepped.csv"))

## Baseline Specification
coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI, n = 1L) + as.factor(fqtr) ,
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI, n = 1L)*mnc + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*high_roa  + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI, n = 1L)*hp_con  + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*kz_con+ as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*big_firm + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*div_payer + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*as.factor(NAICS) + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*highleverage + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*high_intan + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*high_know + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI, n = 1L)*high_org + as.factor(fqtr), 
            model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI, n = 1L)*young_firm + as.factor(fqtr) ,
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")





## Positive

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_POS, n = 1L) + as.factor(fqtr) ,
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_POS, n = 1L)*mnc + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*high_roa  + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_POS, n = 1L)*hp_con  + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*kz_con+ as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*big_firm + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*div_payer + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*as.factor(NAICS) + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*highleverage + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*high_intan + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*high_know + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_POS, n = 1L)*high_org + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_POS, n = 1L)*young_firm + as.factor(fqtr) ,
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

## Neg

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_NEG, n = 1L) + as.factor(fqtr) ,
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_NEG, n = 1L)*mnc + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*high_roa  + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_NEG, n = 1L)*hp_con  + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*kz_con+ as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*big_firm + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
##POTENTIAL HERE
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*div_payer + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*as.factor(NAICS) + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*highleverage + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*high_intan + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*high_know + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")

coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_NEG, n = 1L)*high_org + as.factor(fqtr), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
##HERE
coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_NEG, n = 1L)*young_firm + as.factor(fqtr) ,
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")


## With Firm Controls

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI, n = 1L) + dplyr::lag(g_invt_phy_adj_log, n = 1L)  + as.factor(fqtr) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) + dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI, n = 1L)*mnc + as.factor(fqtr)
             + dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) + dplyr::lag(tobins_q_interp, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*high_roa  + as.factor(fqtr)  + dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L)  +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI, n = 1L)*hp_con + as.factor(fqtr)+ 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*kz_con + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*big_firm + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*div_payer + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*as.factor(NAICS) + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*highleverage + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*high_intan + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI, n = 1L)*high_know + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI, n = 1L)*high_org + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI, n = 1L)*young_firm + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")





## Positive

## With Firm Controls

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_POS, n = 1L) + dplyr::lag(g_invt_phy_adj_log, n = 1L)  + as.factor(fqtr) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_POS, n = 1L)*mnc + as.factor(fqtr)
             + dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) + dplyr::lag(tobins_q_interp, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*high_roa  + as.factor(fqtr)  + dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L)  +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_POS, n = 1L)*hp_con + as.factor(fqtr)+ 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*kz_con + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*big_firm + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*div_payer + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*as.factor(NAICS) + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*highleverage + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*high_intan + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_POS, n = 1L)*high_know + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_POS, n = 1L)*high_org + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_POS, n = 1L) + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compusta), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")


## NEGATIVE

## With Firm Controls

coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_NEG, n = 1L) + dplyr::lag(g_invt_phy_adj_log, n = 1L)  + as.factor(fqtr) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~ dplyr::lag(T_CI_NEG, n = 1L)*mnc + as.factor(fqtr)
             + dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) + dplyr::lag(tobins_q_interp, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*high_roa  + as.factor(fqtr)  + dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L)  +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_NEG, n = 1L)*hp_con + as.factor(fqtr)+ 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*kz_con + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*big_firm + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
#POTENTIAL
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*div_payer + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*as.factor(NAICS) + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*highleverage + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*high_intan + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~   dplyr::lag(T_CI_NEG, n = 1L)*high_know + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
#HERE
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_NEG, n = 1L)*high_org + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")
coeftest(plm(g_invt_phy_adj_log ~  dplyr::lag(T_CI_NEG, n = 1L)*young_firm + as.factor(fqtr) + 
               dplyr::lag(g_invt_phy_adj_log, n = 1L) + dplyr::lag(liquidity, n = 1L) +
               dplyr::lag(log(atq_adj), n = 1L) + dplyr::lag(saleq_adj_log_growth, n = 1L) +
               dplyr::lag(log(ppentq_adj_interp), n = 1L) + dplyr::lag(leverage, n = 1L) +dplyr::lag(tobins_q_interp, n = 1L), 
             model = c("within"), index = c("cont_id"), data = compustat), vcov. = vcovHC, type = "HC1", method = "white1", cluster = "group")



lev <- compustat %>% dplyr::select(-c(V1)) %>% group_by(age) %>% summarise("Leverage" = mean(leverage, na.rm = T)) %>% filter(age<=50)
ggplot(data = lev) + geom_line(aes(x = age, y = Leverage), size = 1.1)  +  theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Age")

compuslev <- compustatlev %>% left_join(lev, by = c("age"))

ggplot(data = compuslev) + 
    geom_line(aes(x = age, y = Leverage.x, color = "Full Compustat"), size = 1.1) +
    geom_line(aes(x = age, y = Leverage.y, color = "Filtered Sample"), size = 1.1) +  theme_minimal() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.border = element_blank(), axis.line = element_line()) + 
    xlab("Age") + ylab("Leverage") + theme(legend.title=element_blank()) + theme(legend.position=c(.25, .85))
