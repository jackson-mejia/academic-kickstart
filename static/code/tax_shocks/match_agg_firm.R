## Matching Aggregate and Firm-Level Net Investment Rates

data_dir <- "C:/Users/Jackson_Mejia/Documents/ERM_Practice"
source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))

### Read in data
nipa_q <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_q_vals_kalman.csv"))
#annual NIPA data
nipa_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_fa_vals.csv"))
#nipa reference table
nipa_ref <- fread(file.path(data_dir,"macro_invt_elasticities/Data/nipa_fa_meta.csv"))

romerdata_q <- fread(file.path(data_dir, "macro_invt_elasticities/Data/romerromer_q.csv"))
romerdata_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/romerromer_ann.csv"))

## Filter relevant vars from quarterly and annual NIPA data
nipa_kalm <- colnames(dplyr::select(nipa_q, contains("KQ")))
nipa_q <- nipa_q %>% dplyr::select(c("date", all_of(nipa_kalm))) %>% rename(Date = date)

# select relevant annual vars
nipa_a_vars <- c(bea_table("FAAt407", "A"), bea_table("FAAt403", "A"), bea_table("FAAt406", "A"))
nipa_a_rates <- colnames(dplyr::select(nipa_a, contains("rate")))
nipa_a <- nipa_a %>% dplyr::select(c("date", all_of(nipa_a_rates), all_of(nipa_a_vars))) %>% rename(Year = date)

## Next we find gross and net investment rates using Fixed Asset Tables 4.3, 4.6, and 4.7. First,
## we do this using periodic differences between stocks (to get net invt) and second, using 4.7 
## (gross invt). Do this both annually and quarterly. This is done sequentially for total investment,
## physical investment (nonres structures + nonres equip), structures, equipment, and R&D. Named using the
## convention hist_TYPE_M_K_frequency, k in {P, NP}, M in {T, P, S, E, R}

##Annually
romerdata_a <- romerdata_a %>% left_join(nipa_a, by = "Year") %>% rename(Date = Year)

## Quarterly
romerdata_q <- romerdata_q %>% left_join(nipa_q, by = "Date")

compustat_a <- fread(file.path(data_dir,"macro_invt_elasticities/Data/compustat_annual_prepped.csv"))
compustat_q <- fread(file.path(data_dir,"macro_invt_elasticities/Data/compustat_quarterly_prepped.csv"))

sum_compustat_a <- compustat_a %>% filter(fyear > 1964 & fyear < 2010 & !is.infinite(g_invt_rate_total))  %>% group_by(fyear) %>%
  summarise(mean_ni_rate = 100*mean(n_invt_rate_total, na.rm = TRUE),
            median_n_invt_rate_tot = 100*median(n_invt_rate_total, na.rm = TRUE),
            median_n_invt_rate_phy = 100*median(n_invt_rate_phy, na.rm = TRUE),
            median_n_invt_rate_int = 100*median(n_invt_rate_int, na.rm = TRUE),
            median_g_invt_rate_tot = 100*median(g_invt_rate_total, na.rm = TRUE),
            median_g_invt_rate_phy = 100*median(g_invt_rate_phy, na.rm = TRUE),
            median_g_invt_rate_int = 100*median(g_invt_rate_int, na.rm = TRUE),
            w_median_n_invt_rate_tot = 100*weighted_median(n_invt_rate_total, w = sale, na.rm = TRUE),
            w_median_n_invt_rate_phy = 100*weighted_median(n_invt_rate_phy, w = sale, na.rm = TRUE),
            w_median_n_invt_rate_int = 100*weighted_median(n_invt_rate_int, w = sale, na.rm = TRUE),
            w_median_g_invt_rate_tot = 100*weighted_median(g_invt_rate_total, w = sale, na.rm = TRUE),
            w_median_g_invt_rate_phy = 100*weighted_median(g_invt_rate_phy,w  = sale,  na.rm = TRUE),
            w_median_g_invt_rate_int = 100*weighted_median(g_invt_rate_int, w = sale, na.rm = TRUE)
            ) %>%
  ungroup %>% rename(Date = fyear) %>%
  left_join(romerdata_a, by = c("Date"))


cor(sum_compustat_a$median_n_invt_rate_tot, sum_compustat_a$nirate_A_37)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_t_ni_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a, median_n_invt_rate_tot, 100*nirate_A_37, "Net")
dev.off()

cor(sum_compustat_a$w_median_n_invt_rate_tot, sum_compustat_a$nirate_A_37)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_t_ni_w_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a, w_median_n_invt_rate_tot, 100*nirate_A_37, "Net")
dev.off()

cor(sum_compustat_a$median_n_invt_rate_phy, sum_compustat_a$nirate_A_3839)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_p_ni_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a, median_n_invt_rate_phy, 100*nirate_A_3839, "Net")
dev.off()

cor(sum_compustat_a$w_median_n_invt_rate_phy, sum_compustat_a$nirate_A_3839)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_p_ni_w_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a, w_median_n_invt_rate_phy, 100*nirate_A_3839, "Net")
dev.off()

sum_compustat_a_intan <- sum_compustat_a %>% filter(Date > 1964)

cor(sum_compustat_a_intan$median_n_invt_rate_int, sum_compustat_a_intan$nirate_A_40)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_r_ni_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a_intan, median_n_invt_rate_int, 100*nirate_A_40, "Net")
dev.off()

cor(sum_compustat_a_intan$w_median_n_invt_rate_int, sum_compustat_a_intan$nirate_A_40)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_r_ni_w_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a_intan, w_median_n_invt_rate_int, 100*nirate_A_40, "Net")
dev.off()

## Gross rates
cor(sum_compustat_a$median_g_invt_rate_tot, sum_compustat_a$girate_A_37)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_t_gi_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a, median_g_invt_rate_tot, 100*girate_A_37, "Gross")
dev.off()

cor(sum_compustat_a$w_median_g_invt_rate_tot, sum_compustat_a$girate_A_37)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_t_gi_w_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a, w_median_g_invt_rate_tot, 100*girate_A_37, "Gross")
dev.off()

cor(sum_compustat_a$median_g_invt_rate_phy, sum_compustat_a$girate_A_3839)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_p_gi_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a, median_g_invt_rate_phy, 100*girate_A_3839, "Gross")
dev.off()

cor(sum_compustat_a$w_median_g_invt_rate_phy, sum_compustat_a$nirate_A_3839)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_p_gi_w_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a, w_median_g_invt_rate_phy, 100*girate_A_3839, "Gross")
dev.off()

cor(sum_compustat_a_intan$median_g_invt_rate_int, sum_compustat_a_intan$girate_A_40)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_r_gi_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a_intan, median_g_invt_rate_int, 100*girate_A_40, "Gross")
dev.off()

cor(sum_compustat_a_intan$w_median_g_invt_rate_int, sum_compustat_a_intan$girate_A_40)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_r_gi_w_a.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_a_intan, w_median_g_invt_rate_int, 100*girate_A_40, "Gross")
dev.off()


sum_compustat_q <- compustat_q %>% dplyr::select(c(date, n_invt_rate_phy, g_invt_rate_phy, saleq, n_invt_rate_phy_adj)) %>%
  filter(date > 1974.75 & date < 2008) %>% group_by(date) %>% 
  summarise(mean_ni = 100*mean(n_invt_rate_phy, na.rm = T),
            mean_gi = 100*mean(g_invt_rate_phy, na.rm = TRUE),
            median_n_invt_rate_phy = 100*median(n_invt_rate_phy_adj, na.rm = TRUE),
            w_median_n_invt_rate_phy = 100*weighted_median(n_invt_rate_phy, w = saleq, na.rm = TRUE),
            median_g_invt_rate_phy = 100*median(g_invt_rate_phy, na.rm = TRUE),
            w_median_g_invt_rate_phy = 100*weighted_median(g_invt_rate_phy, w = saleq, na.rm = TRUE)) %>%
  ungroup %>% rename(Date = date)  %>%
  left_join(romerdata_q, by = c("Date"))

cor(sum_compustat_q$mean_ni, sum_compustat_q$nirate_KQ_3839)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_p_ni_q.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_q, median_n_invt_rate_phy, 100*nirate_KQ_3839, "Net")
dev.off()

cor(sum_compustat_q$w_median_n_invt_rate_phy, sum_compustat_q$nirate_KQ_3839)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_p_ni_w_q.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_q, w_median_n_invt_rate_phy, 100*nirate_KQ_3839, "Net")
dev.off()


cor(sum_compustat_q$median_g_invt_rate_phy, sum_compustat_q$girate_KQ_3839)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_p_gi_q.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_q, median_g_invt_rate_phy, 100*girate_KQ_3839, "Gross")
dev.off()

cor(sum_compustat_q$w_median_g_invt_rate_phy, sum_compustat_q$girate_KQ_3839)
png(file = file.path(data_dir, "macro_invt_elasticities/Figures/match_agg_firm/comp_nipa_p_gi_w_q.png"),
    width = 500,
    height = 300
)
invt_plot(sum_compustat_q, w_median_g_invt_rate_phy, 100*girate_KQ_3839, "Gross")
dev.off()
