# Produce estimates of quarterly data for annual series using Kalman filter/smoother
# Investment and depreciation variables are flows, so we have to take differences
data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'

source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))

### Read in data
nipa <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_q_vals.csv"))
#annual NIPA data
nipa_a <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_fa_vals.csv"))
#nipa reference table
nipa_ref <- fread(file.path(data_dir, "macro_invt_elasticities/Data/nipa_fa_meta.csv"))

##FOF data
fof <- fread(file.path(data_dir, "macro_invt_elasticities/Data/FOF_data.csv"))

##Make sure annual vars go in XXXX.75
nipa_a <- nipa_a %>% mutate(date = date + .75)

###Sequentially construct estimates of quarterly fixed investment (total), fixed investment in
### in physical assets, fixed investment in intangible assets, total depreciation, physical
### asset depreciation, and intangible asset depreciation. Then use those estimates to 
### estimate historical cost capital stock on a quarterly basis

## Filter relevant vars from quarterly and annual NIPA data. Pull from NIPA tables
## 1.14 and 1.5.5 for quarterly data and fixed asset tables 4.3, 4.6, and 4.7. 
nipa_q_vars <- c(bea_table("T114", "Q"), bea_table("T10505", "Q"))
nipa_q <- nipa %>% dplyr::select(c("date", all_of(nipa_q_vars))) %>%
  mutate(across(-c(date), ~.*10^(-3))) %>%
  left_join(fof, by = c("date"))

# select relevant annual vars
nipa_a_vars <- c(bea_table("FAAt403", "A"), 
                 bea_table("FAAt406", "A"), 
                 bea_table("FAAt407", "A"))
nipa_a <- nipa_a %>% dplyr::select(c("date", all_of(nipa_a_vars)))

nipa_q_a <- nipa_q %>% left_join(nipa_a)

## To use Kalman function, supply a df with date as the first column, quarterly variables as columns 2:n-1 and column n as the annual column
## to estimate. Ensure that the series begins and ends with a non-NA annual value. Annual value should appear in
## q4 of the relevant year. For example, the annual capital stock for 2019 should be in 2019.75 (2019Q4)
### Total Stock (4.3.37)
nipa_q_a_stock <- nipa_q_a %>% dplyr::select(c(date, fof_nonfin_assets, fof_wages, fof_comsumpt, FAAt403_37_A)) %>%
  filter(date >= 1951.75 & date <= 2016.75)

total_stock <- kalman_out(nipa_q_a_stock, detrend = TRUE, flow = FALSE)

ggplot(data = total_stock) + geom_line(aes(x = date, y = FAAt403_37_A_KQ)) + 
  geom_point(aes(x = date, y = FAAt403_37_A))

total_stock <- total_stock %>% mutate(FAAt403_37_A_KQ_NA = FAAt403_37_A_KQ - na.approx(FAAt403_37_A)) %>%
  dplyr::select(c(date, FAAt403_37_A_KQ, FAAt403_37_A_KQ_NA))

ggplot(data = total_stock) + geom_line(aes(x = date, y = 100*FAAt403_37_A_KQ_NA/FAAt403_37_A_KQ), size = 1.25) + geom_hline(yintercept=0, linetype="dashed", color = "red", size = 1.1) +
  theme_minimal() + scale_x_continuous(name="Year", limits=c(1950, 2018), breaks = seq(1960, 2010, 10)) + ylab("Percentage") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

##FAAt403_37_A_KQ_NA is the difference between the smoothed series and the linearly approximated series
### Structures (4.3.39)
# Estimate using Flow of Funds from FRB (FRED) -- items NCBCFCQ027S and HCVSNNWHCBSNNCB (see Readme)
nipa_q_a_stock_struct <- nipa_q_a %>% dplyr::select(c(date,fof_struct,fof_wages, fof_comsumpt, FAAt403_39_A)) %>%
  filter(date >= 1952.75 & date <= 2016.75)

stock_struct <- kalman_out(nipa_q_a_stock_struct, detrend = TRUE, flow = FALSE)

ggplot(data = stock_struct) + geom_line(aes(x = date, y = FAAt403_39_A_KQ)) + 
  geom_point(aes(x = date, y = FAAt403_39_A))
stock_struct <- stock_struct %>% 
  dplyr::select(c(date, FAAt403_39_A_KQ))
ggplot(data = stock_struct) + geom_line(aes(x = date, y = FAAt403_39_A_KQ))

### Equipment (4.3.38)
# Estimate using Flow of Funds from FRB (FRED) -- items NCBCFCQ027S and ESATASHCBSNNCB (see Readme)
nipa_q_a_stock_equip <- nipa_q_a %>% dplyr::select(c(date,fof_wages, fof_equip, fof_comsumpt, FAAt403_38_A)) %>%
  filter(date >= 1952.75 & date <= 2016.75)

stock_equip <- kalman_out(nipa_q_a_stock_equip, detrend = TRUE, flow = FALSE)

ggplot(data = stock_equip) + geom_line(aes(x = date, y = FAAt403_38_A_KQ)) + 
  geom_point(aes(x = date, y = FAAt403_38_A))
stock_equip <- stock_equip %>%
  dplyr::select(c(date, FAAt403_38_A_KQ))
ggplot(data = stock_equip) + geom_line(aes(x = date, y = FAAt403_38_A_KQ))

### Intan (4.3.40)
# Estimate using Flow of Funds from FRB (FRED) -- items NCBCFCQ027S and NCBNIPPHCB (see Readme)
nipa_q_a_stock_intan <- nipa_q_a %>% dplyr::select(c(date, fof_wages, fof_intan, fof_comsumpt, FAAt403_40_A)) %>%
  filter(date >= 1952.75 & date <= 2016.75)

stock_intan <- kalman_out(nipa_q_a_stock_intan, detrend = TRUE, flow = FALSE)

ggplot(data = stock_intan) + geom_line(aes(x = date, y = FAAt403_40_A_KQ)) + 
  geom_point(aes(x = date, y = FAAt403_40_A))
stock_intan <- stock_intan  %>%
  dplyr::select(c(date, FAAt403_40_A_KQ))

# Estimates of Depreciation (Fixed Asset Table 4.6 Lines 37-40)

## Dep Total (4.6.37)
# Make estimate using NIPA Table 1.5.5 Line 28 (nonresidential fixed investment) and Table 1.14 Line 18 (nonfinancial corporate consumption of capital)
nipa_q_a_dep <- nipa_q_a %>% dplyr::select(c(date, T10505_28_Q, T11400_18_Q, FAAt406_37_A)) %>%
  filter(date >= 1947.75 & date <= 2016.75)

total_dep <- kalman_out(nipa_q_a_dep, detrend = TRUE, flow = TRUE)

ggplot(data = total_dep) + geom_line(aes(x = date, y = FAAt406_37_A_KQ))
total_dep <- total_dep %>%
  dplyr::select(c(date, FAAt406_37_A_KQ))
## Dep Structures (4.6.38)
# Make estimate using NIPA Table 1.5.5 Line 29 (nonresidential fixed investment in structures) and Table 1.14 Line 18 (nonfinancial corporate consumption of capital)
nipa_q_a_struct_dep <- nipa_q_a %>% dplyr::select(c(date, T10505_29_Q, T11400_18_Q, FAAt406_38_A)) %>%
  filter(date >= 1947.75 & date <= 2016.75)

struct_dep <- kalman_out(nipa_q_a_struct_dep, detrend = TRUE, flow = TRUE)

ggplot(data = struct_dep) + geom_line(aes(x = date, y = FAAt406_38_A_KQ))
struct_dep <- struct_dep %>% dplyr::select(c(date, FAAt406_38_A_KQ))
## Dep Equipment (4.6.39)
# Make estimate using NIPA Table 1.5.5 Line 30 (nonresidential fixed investment in equipment) and Table 1.14 Line 18 (nonfinancial corporate consumption of capital)
nipa_q_a_equip_dep <- nipa_q_a %>% dplyr::select(c(date, T10505_30_Q, T11400_18_Q, FAAt406_39_A)) %>%
  filter(date >= 1947.75 & date <= 2016.75)

equip_dep <- kalman_out(nipa_q_a_equip_dep, detrend = TRUE, flow = TRUE)

ggplot(data = equip_dep) + geom_line(aes(x = date, y = FAAt406_39_A_KQ))
equip_dep <- equip_dep %>% dplyr::select(c(date, FAAt406_39_A_KQ))

## Dep Intangibles (4.6.40)
# Make estimate using NIPA Table 1.5.5 Line 37 (nonresidential fixed investment in intellectual property products) and Table 1.14 Line 18 (nonfinancial corporate consumption of capital)
nipa_q_a_intan_dep <- nipa_q_a %>% dplyr::select(c(date, T10505_37_Q, T11400_18_Q, FAAt406_40_A)) %>%
  filter(date >= 1947.75 & date <= 2016.75)

intan_invt_dep <- kalman_out(nipa_q_a_intan_dep, detrend = TRUE, flow = TRUE)

ggplot(data = intan_invt_dep) + geom_line(aes(x = date, y = FAAt406_40_A_KQ)) 
intan_invt_dep <- intan_invt_dep %>% dplyr::select(c(date, FAAt406_40_A_KQ))


# Estimates of Investment (Fixed Asset Table 4.7 Lines 37-40)

## Invt Total (4.7.37)
# Make estimate using NIPA Table 1.5.5 Line 28 (nonresidential fixed investment) and Table 1.14 Line 19 (nonfinancial corporate net value added)
nipa_q_a_invt <- nipa_q_a %>% dplyr::select(c(date, T11400_19_Q, T10505_28_Q, FAAt407_37_A)) %>%
  filter(date >= 1947.75 & date <= 2016.75)

total_invt <- kalman_out(nipa_q_a_invt, detrend = TRUE, flow = TRUE)

ggplot(data = total_invt) + geom_line(aes(x = date, y = FAAt407_37_A_KQ))
total_invt <- total_invt %>%
  dplyr::select(c(date, FAAt407_37_A_KQ))
## invt Structures (4.7.39)
# Make estimate using NIPA Table 1.5.5 Line 29 (nonresidential fixed investment in structures) and Table 1.14 Line 19 (nonfinancial corporate net value added)
nipa_q_a_struct_invt <- nipa_q_a %>% mutate(fof_struct = fof_struct - dplyr::lag(fof_struct)) %>% 
  dplyr::select(c(date, T11400_19_Q, T10505_29_Q, fof_wages, fof_struct, FAAt407_39_A))  %>% filter(date >= 1952.75 & date <= 2016.75)

struct_invt <- kalman_out(nipa_q_a_struct_invt, detrend = TRUE, flow = TRUE)

ggplot(data = struct_invt) + geom_line(aes(x = date, y = FAAt407_39_A_KQ))
struct_invt <- struct_invt %>% dplyr::select(c(date, FAAt407_39_A_KQ)) %>% filter(date > 1952.75)
## invt Equipment (4.7.38)
# Make estimate using NIPA Table 1.5.5 Line 30 (nonresidential fixed investment in equipment) and Table 1.14 Line 19 (nonfinancial corporate net value added)
nipa_q_a_equip_invt <- nipa_q_a  %>% mutate(fof_equip = fof_equip - dplyr::lag(fof_equip, n = 1L)) %>% 
  dplyr::select(c(date, T11400_19_Q, fof_wages, T10505_30_Q, fof_equip, FAAt407_38_A)) %>% filter(date >= 1952.75 & date <= 2016.75)

equip_invt <- kalman_out(nipa_q_a_equip_invt, detrend = TRUE, flow = TRUE)

ggplot(data = equip_invt) + geom_line(aes(x = date, y = FAAt407_38_A_KQ))
equip_invt <- equip_invt %>% dplyr::select(c(date, FAAt407_38_A_KQ))  %>% filter(date > 1952.75)

## invt Intangibles (4.7.40)
# Make estimate using NIPA Table 1.5.5 Line 37 (nonresidential fixed investment in intellectual property products) and Table 1.14 Line 19 (nonfinancial corporate net value added)
nipa_q_a_intan_invt <- nipa_q_a %>%
  mutate(fof_intan = fof_intan - dplyr::lag(fof_intan)) %>% dplyr::select(c(date, T11400_19_Q, T10505_37_Q, fof_wages, FAAt407_40_A)) %>% filter(date >= 1952.75 & date <= 2016.75)

intan_invt_invt <- kalman_out(nipa_q_a_intan_invt, detrend = TRUE, flow = TRUE)

ggplot(data = intan_invt_invt) + geom_line(aes(x = date, y = FAAt407_40_A_KQ)) 
intan_invt_invt <- intan_invt_invt %>% dplyr::select(c(date, FAAt407_40_A_KQ))  %>% filter(date > 1952.75)


### Estimate 4.7.41-44

## 4.7.41 
## Use noncorporate net value added, 
nipa_q_a_total_invt_nc <- nipa_q_a %>%  
  dplyr::select(c(date, fof_nc_nva, fof_nc_nonres_gfcf, FAAt407_41_A)) %>% filter(date >= 1959.75 & date <= 2016.75)

total_nc_kalm <- kalman_out(nipa_q_a_total_invt_nc, detrend = TRUE, flow = TRUE)

ggplot(data = total_nc_kalm) + geom_line(aes(x = date, y = FAAt407_41_A_KQ)) 

total_invt_nc <- total_nc_kalm %>% dplyr::select(c(date, FAAt407_41_A_KQ)) %>% filter(date >= 1960)

## 4.7.42 Equipment
## Use noncorporate net value added, cc net investment in equipment (difference between level), nonresidential fixed investment in equipment
nipa_q_a_equip_invt_nc <- nipa_q_a %>%  mutate(fof_nc_cc_equip = fof_nc_cc_equip - dplyr::lag(fof_nc_cc_equip, n = 1L)) %>%
  dplyr::select(c(date, fof_nc_cc_equip, fof_nc_nva, T10505_30_Q, FAAt407_42_A)) %>% filter(date >= 1959.75 & date <= 2016.75)

equip_nc_kalm <- kalman_out(nipa_q_a_equip_invt_nc, detrend = TRUE, flow = TRUE)

ggplot(data = equip_nc_kalm) + geom_line(aes(x = date, y = FAAt407_42_A_KQ)) 

equip_invt_nc <- equip_nc_kalm %>% dplyr::select(c(date, FAAt407_42_A_KQ)) %>% filter(date >= 1960)

## 4.7.43 Structures
nipa_q_a_struct_invt_nc <- nipa_q_a %>% mutate(fof_nc_cc_struct = fof_nc_cc_struct - dplyr::lag(fof_nc_cc_struct, n = 1L)) %>%
  dplyr::select(c(date, fof_nc_cc_struct, fof_nc_nva, T10505_29_Q, FAAt407_43_A))  %>% filter(date >= 1959.75 & date <= 2016.75)

struct_nc_kalm <- kalman_out(nipa_q_a_struct_invt_nc, detrend = TRUE, flow = TRUE)

ggplot(data = struct_nc_kalm) + geom_line(aes(x = date, y = FAAt407_43_A_KQ)) 

struct_invt_nc <- struct_nc_kalm %>% dplyr::select(c(date, FAAt407_43_A_KQ)) %>% filter(date >= 1960)

## 4.7.44 Intellectual Property Products

nipa_q_a_intan_invt_nc <- nipa_q_a %>% mutate(fof_nc_cc_ip = fof_nc_cc_ip - dplyr::lag(fof_nc_cc_ip, n = 1L)) %>%
  dplyr::select(c(date, fof_nc_cc_ip, fof_nc_nva, T10505_37_Q, FAAt407_44_A))  %>% filter(date >= 1959.75 & date <= 2016.75)

intan_nc_kalm <- kalman_out(nipa_q_a_intan_invt_nc, detrend = TRUE, flow = TRUE)

ggplot(data = intan_nc_kalm) + geom_line(aes(x = date, y = FAAt407_44_A_KQ)) 

intan_invt_nc <- intan_nc_kalm %>% dplyr::select(c(date, FAAt407_44_A_KQ)) %>% filter(date >= 1960)

## Put together to use on capital stock estimates

estimates <- total_invt %>%
  left_join(equip_invt) %>%
  left_join(struct_invt) %>%
  left_join(intan_invt_invt) %>%
  left_join(stock_struct) %>%
  left_join(stock_equip) %>%
  left_join(stock_intan) %>%
  left_join(total_stock) %>%
  left_join(equip_invt_nc) %>%
  left_join(struct_invt_nc) %>%
  left_join(intan_invt_nc) %>%
  left_join(total_invt_nc) %>%
    mutate(FAAt407_37_A_KQ = FAAt407_38_A_KQ + FAAt407_39_A_KQ + FAAt407_40_A_KQ,
           FAAt407_41_A_KQ = FAAt407_42_A_KQ + FAAt407_43_A_KQ + FAAt407_44_A_KQ,
           FAAt403_3839_A_KQ = FAAt403_38_A_KQ + FAAt403_39_A_KQ,
           FAAt407_3839_A_KQ = FAAt407_38_A_KQ + FAAt407_39_A_KQ,
           FAAt411_37_A_KQ = FAAt403_37_A_KQ - dplyr::lag(FAAt403_37_A_KQ, n = 1L, default = 0),
           FAAt411_38_A_KQ = FAAt403_38_A_KQ - dplyr::lag(FAAt403_38_A_KQ, n = 1L, default = 0),
           FAAt411_39_A_KQ = FAAt403_39_A_KQ - dplyr::lag(FAAt403_39_A_KQ, n = 1L, default = 0),
           FAAt411_40_A_KQ = FAAt403_40_A_KQ - dplyr::lag(FAAt403_40_A_KQ, n = 1L, default = 0),
           FAAt411_3839_A_KQ = FAAt403_3839_A_KQ - dplyr::lag(FAAt403_3839_A_KQ, n = 1L, default = 0),
           FAAt406_37_A_KQ = FAAt407_37_A_KQ - FAAt411_37_A_KQ,
           FAAt406_38_A_KQ = FAAt407_38_A_KQ + FAAt411_38_A_KQ,
           FAAt406_39_A_KQ = FAAt407_39_A_KQ + FAAt411_39_A_KQ,
           FAAt406_40_A_KQ = FAAt407_40_A_KQ + FAAt411_40_A_KQ,
           FAAt406_3839_A_KQ = FAAt407_3839_A_KQ + FAAt411_3839_A_KQ
           )

nipa_all_q <- nipa %>% left_join(estimates) %>%
  mutate(nirate_KQ_37 = FAAt411_37_A_KQ/dplyr::lag(FAAt403_37_A_KQ, n = 1L),
         girate_KQ_37 = FAAt407_37_A_KQ/dplyr::lag(FAAt403_37_A_KQ, n = 1L),
         nirate_KQ_38 = FAAt411_38_A_KQ/dplyr::lag(FAAt403_38_A_KQ, n = 1L),
         girate_KQ_38 = FAAt407_38_A_KQ/dplyr::lag(FAAt403_38_A_KQ, n = 1L),
         nirate_KQ_39 = FAAt411_39_A_KQ/dplyr::lag(FAAt403_39_A_KQ, n = 1L),
         girate_KQ_39 = FAAt407_39_A_KQ/dplyr::lag(FAAt403_39_A_KQ, n = 1L),
         nirate_KQ_40 = FAAt411_40_A_KQ/dplyr::lag(FAAt403_40_A_KQ, n = 1L),
         girate_KQ_40 = FAAt407_40_A_KQ/dplyr::lag(FAAt403_40_A_KQ, n = 1L),
         nirate_KQ_3839 = FAAt411_3839_A_KQ/dplyr::lag(FAAt403_3839_A_KQ, n = 1L),
         girate_KQ_3839 = FAAt407_3839_A_KQ/dplyr::lag(FAAt403_3839_A_KQ, n = 1L)
         )

write.csv(nipa_all_q, file.path(data_dir, "macro_invt_elasticities/Data/nipa_q_vals_kalman.csv"))
