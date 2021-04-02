### Generate VARS using Romer-Romer specification
data_dir <- "C:/Users/Jackson_Mejia/Documents/ERM_Practice/"
source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))

### Read in data
##FOF data
fof <- fread(file.path(data_dir, "macro_invt_elasticities/Data/FOF_data.csv"))

nipa_q <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_q_vals_kalman.csv"))
nipa_q <- nipa_q %>% dplyr::select(-c(V1)) %>%left_join(fof)
#annual NIPA data
nipa_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_fa_vals.csv"))
#nipa reference table
nipa_ref <- fread(file.path(data_dir,"macro_invt_elasticities/Data/nipa_fa_meta.csv"))

romerdata_q <- fread(file.path(data_dir, "macro_invt_elasticities/Data/romerromer_q.csv"))
romerdata_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/romerromer_ann.csv"))

## Filter relevant vars from quarterly and annual NIPA data
#nipa_q_vars <- c(bea_table("T10103", "Q"), bea_table("T10505", "Q"), bea_table("T50305", "Q"))
#nipa_kalm <- colnames(dplyr::select(nipa_q, contains("nirate")))
#nipa_q <- nipa_q %>% dplyr::select(c("date", all_of(nipa_q_vars), all_of(nipa_kalm))) %>% rename(Date = date)
nipa_q <- nipa_q %>%rename(Date = date) %>%
  mutate(across(contains("nirate"), ~.*100)) %>%
  mutate(gva_deflator = T11400_17_Q/T11400_41_Q) %>%
  mutate(across(contains("KQ"), ~./gva_deflator)) %>%
  mutate(fof_nc_nonres_gfcf = fof_nc_nonres_gfcf/gva_deflator,
         fof_gfcf = fof_gfcf/gva_deflator)
# select relevant annual vars
nipa_a_vars <- c(bea_table("FAAt407", "A"), bea_table("FAAt403", "A"), bea_table("FAAt406", "A"), 
                 bea_table("T10103", "A"),
                 bea_table("T10105", "A"),
                 bea_table("T10503", "A"),
                 bea_table("T10505", "A"),
                 bea_table("T50305", "A"),
                 colnames(dplyr::select(nipa_a, contains("nirate"))))
nipa_a <- nipa_a %>% dplyr::select(c("date", all_of(nipa_a_vars))) %>% rename(Year = date)

##Annually
romerdata_a <- romerdata_a %>% left_join(nipa_a, by = "Year")%>%  rename(Date = Year) %>%
  mutate(across(contains("nirate"), ~.*100)) %>% filter(Date > 1946.75 & Date < 2007.25)

## Quarterly
romerdata_q <- romerdata_q %>% left_join(nipa_q, by = "Date") %>%  filter(Date > 1946.75 & Date < 2007.25) %>%
  mutate(FAAT = FAAt407_3839_A_KQ + FAAt407_40_A_KQ,
         EXOGENRRATIO_80_B = ifelse(Date < 1980, EXOGENRRATIO, 0),
         EXOGENRRATIO_80_A = ifelse(Date >= 1980, EXOGENRRATIO, 0),
         T_CI_POS = ifelse(T_CI > 0, T_CI, 0),
         T_PI_POS = ifelse(T_PI > 0, T_PI, 0),
         T_CI_NEG = ifelse(T_CI < 0, T_CI, 0),
         T_PI_NEG = ifelse(T_PI < 0, T_PI, 0),
         EXOGENRRATIO_NEG = ifelse(EXOGENRRATIO < 0, -1*EXOGENRRATIO, 0),
         EXOGENRRATIO_POS = ifelse(EXOGENRRATIO > 0, EXOGENRRATIO, 0)
  )


constr_nipa <- c(colnames(romerdata_a %>% dplyr::select(contains("nirate"))),
                 colnames(dplyr::select(romerdata_q, contains("nirate") )))

romer_rep_vars <- c(bea_table("T10103", "Q", c(1,7,8,9,13)), bea_table("T10103", "A", c(1,7,8,9,13))) 

romer_extend_vars <- c(bea_table("T10503",  "Q",26:41), 
                       bea_table("T50303","Q", 1:26))

romer_extend_vars_08 <- c(sprintf("RQ_T10503_%d", 21:33), sprintf("RQ_T50303_%d", 1:28))
#  structures, equipment, ip
romer_disagg <- c("T10503_29_Q", "T10503_30_Q", "T10503_37_Q")

romer_nipa_vars <- c("GDP", "T10103_1_Q", "GPDI", "T10103_7_Q", "FI", "T10103_8_Q", "NONRES", "T10103_9_Q", "RES", "T10103_13_Q") 
# vars to replicate R-R from 2008Q1 NIPA
romer_rep_vars_08 <- c("GDP", "GPDI", "FI", "NONRES", "RES") 
# Start date of 1960 for comparability
legal_form_vars <- c("FAAt407_37_A_KQ", "FAAt407_38_A_KQ", "FAAt407_39_A_KQ", "FAAt407_40_A_KQ",
                     "FAAt407_41_A_KQ", "FAAt407_42_A_KQ", "FAAt407_43_A_KQ", "FAAt407_44_A_KQ")
legal_form_vars_gfcf <- c("fof_gfcf", "fof_nc_nonres_gfcf")
#FOF vars for legal type. Start date 1952
legal_form_fof <- c("fof_net_equip_cc", "fof_net_struct_cc",  "fof_net_intan_cc", 
                    "fof_net_all_cc", "fof_nc_net_equip_cc", "fof_nc_net_struct_cc", 
                    "fof_nc_net_ip_cc", "fof_nc_net_all_cc")

#Set vector of variables we want for impulse response functions
invt_vars <-  c(romer_rep_vars_08)
#initialize list to contain IRF plots
irf_plots <- list()
romerdata_q <- romerdata_q %>% filter(Date >1974.75)
romer_tax_split <- c("EXOGENRRATIO_80_B", "EXOGENRRATIO_80_A")
tax_shocks <- c("EXOGENRRATIO_POS", "EXOGENRRATIO_NEG")
# initialize NA vector to put peak (minimum) values in
peak_v <- rep(NA, length(invt_vars)*length(tax_shocks))
#initialize NA vector to put peak period in
quarters_v <- rep(NA, length(invt_vars)*length(tax_shocks))
dep_vars <-rep(invt_vars, length(tax_shocks))
#initialize df to contain a column of IRF vars, the period in which the peak occurred, and the peak value
peak_df <- data.frame(dep_vars, quarters_v, peak_v)

# initialize vector to contain i*j names of impulse responses, with each name taking the form i_j, i \in {invt_vars}, j \in {tax_shocks}
invt_vars_exp <- rep(NA, nrow(peak_df))
#loop over IRF vars to create TS object, run VAR, get IRF, extract min value, and plot IRF
for(j in 1:length(tax_shocks)){
  for(i in 1:length(invt_vars)){
    if(invt_vars[i] %in% names(romerdata_q)){
      if(grepl("rate", invt_vars[i])){
        v1 <- var_ts(invt_vars[i], tax_shocks[j], 0, 4, GDP = FALSE) # create time series for IRF response var
      } else {
        v1 <- var_ts(invt_vars[i], tax_shocks[j], 1, 4, GDP = TRUE, trans_negs = FALSE) 
      }
      
      colnames(v1)[colnames(v1) == "ts_invt"] <- invt_vars[i] # rename TS column names
      
      irf <- run_var_irf(v1, invt_vars[i], 20) # run VAR and get IRF
      irf <- extract_varirf(irf, tax_shocks[j], "Q") # get IRF in readable format
      plot <- plot_irf(irf, invt_vars[i], tax_shocks[j]) # plot IRF
      
      peak_df[(j-1)*length(invt_vars) + i,2:5] <- peak_finder(irf) # extract min value and period
      
      irf_plots[[(j-1)*length(invt_vars) + i]] <- plot # save plot
      invt_vars_exp[(j-1)*length(invt_vars) + i] <- paste(invt_vars[i], j, sep=  "_")
      
    } 
    else{
      if(grepl("rate", invt_vars[i])){
        v1 <- var_ts(invt_vars[i], tax_shocks[j], 0, 1, GDP = FALSE) # create time series for IRF response var
      } else {
        v1 <- var_ts(invt_vars[i], tax_shocks[j], 1, 1, GDP = FALSE) 
      }
      
      colnames(v1)[colnames(v1) == "ts_invt"] <- invt_vars[i] # rename TS column names
      
      irf <- run_var_irf(v1, invt_vars[i], 5) # run VAR and get IRF
      irf <- extract_varirf(irf, tax_shocks[j], "A") # get IRF in readable format
      plot <- plot_irf(irf, invt_vars[i], tax_shocks[j]) # plot IRF
      
      peak_df[(j-1)*length(invt_vars) + i,2:5] <- peak_finder(irf) # extract min value and period
      
      irf_plots[[(j-1)*length(invt_vars)+ i]] <- plot # save plot
      invt_vars_exp[(j-1)*length(invt_vars) + i] <- paste(invt_vars[i], j, sep = "_")
    }
  }
}

bigly <- romerdata_a %>% mutate(intanr = T10105_12_A/T10105_9_A)
ggplot(data = bigly) + geom_line(aes(x = Date, y = intanr), size = 1.1)  + ylab("Intangible Intensity") + theme_minimal() +
  theme(legend.position="bottom") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line())

#To recreate results for Figure ### (impulse responses from NIPA 1.1.3 10-12 from exogenrratio). 
irf_plots[[1]] + ggtitle("Structures") + theme(legend.position="bottom") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead")
irf_plots[[2]] + ggtitle("Equipment") + theme(legend.position="bottom") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead")
irf_plots[[3]] + ggtitle("Intellectual Property") + theme(legend.position="bottom") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead")

##Heterogeneity by type of investment
multi_irf_plots(all_of(invt_vars_exp), Conf_Int = FALSE, Title = "") + 
  theme(legend.position="none")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead")
multi_irf_plots(c("T50303_2_Q_1", "T50303_20_Q_1"), Conf_Int = TRUE, Title = "By Major Type") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Nonresidential", "Residential")) + guides(fill = FALSE)
multi_irf_plots(c("T50303_3_Q_1", "T50303_9_Q_1", "T50303_16_Q_1"), Conf_Int = TRUE, Title = "By Nonresidential Type") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Intellectual Property", "Structures", "Equipment")) + guides(fill = FALSE)
multi_irf_plots(c("T50303_4_Q_1", "T50303_5_Q_1", "T50303_6_Q_1", "T50303_7_Q_1", "T50303_8_Q_1"), Conf_Int = TRUE, Title = "By Structures Type") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank())  +
  scale_color_hue(labels = c("Commercial", "Manufacturing", "Power", "Mining", "Other")) + guides(fill = FALSE)
multi_irf_plots(c("T50303_10_Q_1", "T50303_13_Q_1", "T50303_14_Q_1", "T50303_15_Q_1"), Conf_Int = TRUE, Title = "By Equipment Type") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank())  +
  scale_color_hue(labels = c("Information Processing", "Industrial", "Transportation", "Other")) + guides(fill = FALSE)
multi_irf_plots(c("T50303_17_Q_1", "T50303_18_Q_1", "T50303_19_Q_1"), Conf_Int = TRUE, Title = "By Intellectual Property Type") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank())  +
  scale_color_hue(labels = c("Software", "R&D", "Entertainment")) + guides(fill = FALSE)


#IRFs by legal type GFCF
multi_irf_plots(c("fof_gfcf_1", "fof_nc_nonres_gfcf_1"), Conf_Int = TRUE, Title = "GFCF") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("fof_gfcf_2", "fof_nc_nonres_gfcf_2"), Conf_Int = TRUE, Title = "GFCF") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)

## IRFs by Legal type, FOF
multi_irf_plots(c("fof_net_all_cc_1", "fof_nc_net_all_cc_1"), Conf_Int = TRUE, Title = "Total Investment") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("fof_net_equip_cc_1", "fof_nc_net_equip_cc_1"), Conf_Int = TRUE, Title = "Equipment") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("fof_net_struct_cc_1", "fof_nc_net_struct_cc_1"), Conf_Int = TRUE, Title = "Structures") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("fof_net_intan_cc_1", "fof_nc_net_ip_cc_1"), Conf_Int = TRUE, Title = "Intellectual Property") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
#PI
multi_irf_plots(c("fof_net_all_cc_2", "fof_nc_net_all_cc_2"), Conf_Int = TRUE, Title = "Total Investment") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("fof_net_equip_cc_2", "fof_nc_net_equip_cc_2"), Conf_Int = TRUE, Title = "Equipment") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("fof_net_struct_cc_2", "fof_nc_net_struct_cc_2"), Conf_Int = TRUE, Title = "Structures") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("fof_net_intan_cc_2", "fof_nc_net_ip_cc_2"), Conf_Int = TRUE, Title = "Intellectual Property") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)



##IRFs by legal type, CI SHOCK first
multi_irf_plots(c("FAAt407_37_A_KQ_1", "FAAt407_41_A_KQ_1"), Conf_Int = TRUE, Title = "Total Investment") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("FAAt407_38_A_KQ_1", "FAAt407_42_A_KQ_1"), Conf_Int = TRUE, Title = "Equipment") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("FAAt407_39_A_KQ_1", "FAAt407_43_A_KQ_1"), Conf_Int = TRUE, Title = "Structures") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("FAAt407_40_A_KQ_1", "FAAt407_44_A_KQ_1"), Conf_Int = TRUE, Title = "Intellectual Property") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
  #PI
multi_irf_plots(c("FAAt407_37_A_KQ_2", "FAAt407_41_A_KQ_2"), Conf_Int = TRUE, Title = "Total Investment") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("FAAt407_38_A_KQ_2", "FAAt407_42_A_KQ_2"), Conf_Int = TRUE, Title = "Equipment") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("FAAt407_39_A_KQ_2", "FAAt407_43_A_KQ_2"), Conf_Int = TRUE, Title = "Structures") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)
multi_irf_plots(c("FAAt407_40_A_KQ_2", "FAAt407_44_A_KQ_2"), Conf_Int = TRUE, Title = "Intellectual Property") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Corporate", "Noncorporate")) + guides(fill = FALSE)


##R-R with split sample and updated NIPA

multi_irf_plots(c("T10103_1_Q_1", "T10103_1_Q_2"), Conf_Int = TRUE, Title = "GDP") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)
multi_irf_plots(c("T10103_7_Q_1", "T10103_7_Q_2"), Conf_Int = TRUE, Title = "GPDI")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)
multi_irf_plots(c("T10103_8_Q_1", "T10103_8_Q_2"), Conf_Int = TRUE, Title = "Fixed Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)
multi_irf_plots(c("T10103_13_Q_1", "T10103_13_Q_2"), Conf_Int = TRUE, Title = "Residential Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)
multi_irf_plots(c("T10103_9_Q_1", "T10103_9_Q_2"), Conf_Int = TRUE, Title = "Nonresidential Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)




## Replicate R-R with split-sample
multi_irf_plots(c("GDP_1", "GDP_2"), Conf_Int = TRUE, Title = "GDP") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)
multi_irf_plots(c("GPDI_1", "GPDI_2"), Conf_Int = TRUE, Title = "GPDI")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)
multi_irf_plots(c("FI_1", "FI_2"), Conf_Int = TRUE, Title = "Fixed Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)
multi_irf_plots(c("RES_1", "RES_2"), Conf_Int = TRUE, Title = "Residential Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)
multi_irf_plots(c("NONRES_1", "NONRES_2"), Conf_Int = TRUE, Title = "Nonresidential Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Pre-1980", "Post-1980")) + guides(fill = FALSE)



## Replicate R-R with split-sample split-shocks
multi_irf_plots(c("GDP_1", "GDP_2"), Conf_Int = TRUE, Title = "GDP") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Positive Shock", "Negative Shock")) + guides(fill = FALSE)
multi_irf_plots(c("GPDI_1", "GPDI_2"), Conf_Int = TRUE, Title = "GPDI")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Positive Shock", "Negative Shock")) + guides(fill = FALSE)
multi_irf_plots(c("FI_1", "FI_2"), Conf_Int = TRUE, Title = "Fixed Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Positive Shock", "Negative Shock")) + guides(fill = FALSE)
multi_irf_plots(c("RES_1", "RES_2"), Conf_Int = TRUE, Title = "Residential Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Positive Shock", "Negative Shock")) + guides(fill = FALSE)
multi_irf_plots(c("NONRES_1", "NONRES_2"), Conf_Int = TRUE, Title = "Nonresidential Investment")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_blank(), axis.line = element_line()) + xlab("Quarters Ahead") + theme(legend.title = element_blank()) +
  scale_color_hue(labels = c("Positive Shock", "Negative Shock")) + guides(fill = FALSE)




# Replicate R-R with updated NIPA
multi_irf_plots(c("GDP_1", "T10103_1_Q_1"), Conf_Int = TRUE, Title = "GDP")

multi_irf_plots(c("GPDI_1", "T10103_7_Q_1"), Conf_Int = TRUE, Title = "GPDI")

multi_irf_plots(c("FI_1", "T10103_8_Q_1"), Conf_Int =  TRUE, Title = "Fixed Investment")

multi_irf_plots(c("NONRES_1", "T10103_9_Q_1"), Conf_Int = TRUE, Title = "Nonresidential Fixed Investment")

multi_irf_plots(c("RES_1", "T10103_13_Q_1"), Conf_Int = TRUE,  "Residential Fixed Investment")

# generate plots to replicate R-R with annual data
multi_irf_plots(c("GDP_1", "RA_T10103_1_1"), Conf_Int = TRUE, Title = "GDP")

multi_irf_plots(c("GPDI_1", "RA_T10103_6_1"), Conf_Int = TRUE, Title = "GPDI")

multi_irf_plots(c("FI_1", "RA_T10103_7_1"), Conf_Int =  TRUE, Title = "Fixed Investment")

multi_irf_plots(c("NONRES_1", "RA_T10103_8_1"), Conf_Int = TRUE, Title = "Nonresidential Fixed Investment")

multi_irf_plots(c("RES_1", "RA_T10103_11_1"), Conf_Int = TRUE,  "Residential Fixed Investment")



test <- romerdata_q %>% left_join(big) %>% dplyr::select(c(RA_T10103_11, Date, RES)) %>% filter(Date < 2007.25 & Date > 1946.75) %>% mutate(RA_T10103_11 = na.approx(RA_T10103_11))

ggplot(data = test) + geom_line(aes(x = Date, y = RES)) + geom_line(aes(x = Date, y = RA_T10103_11))



### IF we want a single VAR and IRF
romerdatat <- romerdata_a
  ts_invt <- ts(100*log(romerdatat$RA_T10103_11), start = c(romerdatat$Date[1]), frequency = 1) #for level variables--take log to get response in percent
  TAXts <- ts(romerdatat$EXOGENRRATIO, start = c(romerdatat$Date[1]), frequency = 1) # create time series for tax var
  #GDPts <- ts(100*log(romerdatat$RA_T10103_1), start = c(romerdatat$Date[1]), frequency = 1)
  v2 <- cbind(TAXts, ts_invt) # create TS with shock and response
  colnames(v2) <- cbind("TAX", "invt") # rename TS column names
Model1 <- VAR(v2, p = 12, type = "const", season = NULL, exog = NULL)   #VAR of 12 lags
# IRF with a tax impulse
irf <- irf(Model1, impulse = "TAX", response = "invt", ortho = FALSE, cumulative = FALSE,  n.ahead = 5, boot = TRUE, runs = 200, ci = 0.68)
plot(irf)
irf2 <- extract_varirf(irf, "EXOGENRRATIO", 1)
p <- plot_irf(irf2, "invt", 1)
p
