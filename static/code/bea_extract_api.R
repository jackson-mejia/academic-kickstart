### Use BEA API to extract NIPA annual and quarterly data

data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'

source(file.path(data_dir,"macro_invt_elasticities/Scripts/FUNCTIONS.R"))

# NIPA Annual

# Get vector of NIPA table IDs and names
nipa_tabs <- unlist(beaParamVals('356252FE-692A-4DA7-B43F-5A36CF6CDAA6', "NIPA", "TableName"), use.names = FALSE)

#isolate NIPA table IDs
nipa_tabs <- na.omit(nipa_tabs[1:(length(nipa_tabs)*.5)])

#set time period of interest
year <- (seq(1945, 2020, by = 1))

#initialize data value df with time period of interest
nipa_ann <- data.frame(year) %>% rename(date = year)

#initial metadata df
bea_metadata <- bea_meta(nipa_tabs[1], "A", 'NIPA') %>% filter(is.na(TableName))

#create BEA annual fixed asset table (all vars) and metadata
for(i in 1:length(nipa_tabs)){
  bea_metadata <- rbind(bea_metadata, bea_meta(nipa_tabs[i], "A", 'NIPA'))

  bea_table <- bea_extract(nipa_tabs[i], "A", 'NIPA')
  
  nipa_ann <- nipa_ann %>%
    left_join(bea_table, by = c("date"))
  Sys.sleep(1.1)
}


#Fixed Assets Annual

# Get vector of fixed asset table IDs and names
fixed_asset_tabs <- unlist(beaParamVals('356252FE-692A-4DA7-B43F-5A36CF6CDAA6', "FixedAssets", "TableName"), use.names = FALSE)

#isolate fixed asset table IDs
fixed_asset_tabs <- fixed_asset_tabs[1:(length(fixed_asset_tabs)*.5)]

#set time period of interest
year <- (seq(1945, 2020, by = 1))

#initialize data value df with time period of interest
fixed_assets_ann <- data.frame(year) %>% rename(date = year)

#initial metadata df
bea_metadata_fa <- bea_meta(fixed_asset_tabs[1], "A", 'FixedAssets') %>% filter(is.na(TableName))

#create BEA annual fixed asset table (all vars) and metadata
for(i in 1:length(fixed_asset_tabs)){
  
  bea_table <- bea_extract(fixed_asset_tabs[i], "A", 'FixedAssets')
  
  bea_metadata_fa <- rbind(bea_metadata_fa, bea_meta(fixed_asset_tabs[i], "A", 'FixedAssets'))
  
  fixed_assets_ann <- fixed_assets_ann %>%
    left_join(bea_table, by = c("date"))
  Sys.sleep(1.1)
  print(i/length(fixed_asset_tabs))
}

combined_meta_data <- rbind(bea_metadata, bea_metadata_fa) %>% group_by(SeriesCode) %>%
  filter(row_number() == 1)

combined_nipa_fa <- left_join(nipa_ann, fixed_assets_ann) %>%
  mutate(across(where(is.character), as.numeric))


combined_nipa_fa <- combined_nipa_fa %>% 
  mutate(
    FAAt403_3839_A = FAAt403_38_A + FAAt403_39_A,
    FAAt406_3839_A = FAAt406_38_A + FAAt406_39_A,
    FAAt411_37_A = FAAt403_37_A - dplyr::lag(FAAt403_37_A, n = 1L, default = 0),
    FAAt411_38_A = FAAt403_38_A - dplyr::lag(FAAt403_38_A, n = 1L, default = 0),
    FAAt411_39_A = FAAt403_39_A - dplyr::lag(FAAt403_39_A, n = 1L, default = 0),
    FAAt411_40_A = FAAt403_40_A - dplyr::lag(FAAt403_40_A, n = 1L, default = 0),
    FAAt411_3839_A = FAAt403_3839_A - dplyr::lag(FAAt403_3839_A, n = 1L, default = 0), 
    FAAt407_3839_A = FAAt407_38_A + FAAt407_39_A,
    nirate_A_37 = FAAt411_37_A/dplyr::lag(FAAt403_37_A, n = 1L),
    girate_A_37 = FAAt407_37_A/dplyr::lag(FAAt403_37_A, n = 1L),
    nirate_A_38 = FAAt411_38_A/dplyr::lag(FAAt403_38_A, n = 1L),
    girate_A_38 = FAAt407_38_A/dplyr::lag(FAAt403_38_A, n = 1L),
    nirate_A_39 = FAAt411_39_A/dplyr::lag(FAAt403_39_A, n = 1L),
    girate_A_39 = FAAt407_39_A/dplyr::lag(FAAt403_39_A, n = 1L),
    nirate_A_40 = FAAt411_40_A/dplyr::lag(FAAt403_40_A, n = 1L),
    girate_A_40 = FAAt407_40_A/dplyr::lag(FAAt403_40_A, n = 1L),
    nirate_A_3839 = FAAt411_3839_A/dplyr::lag(FAAt403_3839_A, n = 1L),
    girate_A_3839 = FAAt407_3839_A/dplyr::lag(FAAt403_3839_A, n = 1L)
  )

write.csv(combined_nipa_fa, file.path(data_dir,"macro_invt_elasticities/Data/nipa_fa_vals.csv"))

write.csv(combined_meta_data, file.path(data_dir,"macro_invt_elasticities/Data/nipa_fa_meta.csv"))


# NIPA Quarterly

#set time period of interest
year <- (seq(1945, 2020, by = .25))

#initialize data value df with time period of interest
nipa_q <- data.frame(year) %>% rename(date = year)

#create BEA annual fixed asset table (all vars) and metadata
for(i in 1:length(nipa_tabs)){
  
  bea_table <- bea_extract(nipa_tabs[i], "Q", 'NIPA')
  
  nipa_q <- nipa_q %>%
    left_join(bea_table, by = c("date"))
  Sys.sleep(1.5)
  print(i/length(nipa_tabs))
}

nipa_q <- nipa_q %>% mutate(across(where(is.character), as.numeric))
write.csv(nipa_q, file.path(data_dir, "macro_invt_elasticities/Data/nipa_q_vals.csv"))