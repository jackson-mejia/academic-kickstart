data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'

source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))




# Q_CI_ADJ

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT/jorda_b.RData"))

jordadf(jorda_b, jorda_b_f, 8, "Baseline")

rm(jorda_b, jorda_b_f)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_mnc.RData"))

jordadf(jorda_mnc, jorda_f_mnc, 8, "Multinationals")

rm(jorda_mnc, jorda_f_mnc)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT/jorda_big.RData"))

jordadf(jorda_big, jorda_f_big, 8, "Big Firms")

rm(jorda_big, jorda_f_big)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_div.RData"))

jordadf(jorda_div, jorda_f_div, 8, "Dividend Payers")

rm(jorda_div, jorda_f_div)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_hp.RData"))

jordadf(jorda_hp, jorda_f_hp, 8, "Hadlock-Pierce Contrained")

rm(jorda_hp, jorda_f_hp)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_intan.RData"))

jordadf(jorda_intan, jorda_f_intan, 8, "High Intangible Share of K")

rm(jorda_intan, jorda_f_intan)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_know.RData"))

jordadf(jorda_know, jorda_f_know, 8, "High Knowledge Share of K")

rm(jorda_know, jorda_f_know)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_kz.RData"))

jordadf(jorda_kz, jorda_f_kz, 8, "Kaplan-Zingales Constrained")

rm(jorda_kz, jorda_f_kz)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_lev.RData"))

jordadf(jorda_lev, jorda_f_lev, 8, "High Leverage Firms")

rm(jorda_lev, jorda_f_lev)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_roa.RData"))

jordadf(jorda_roa, jorda_f_roa, 8, "High ROA")

rm(jorda_roa, jorda_f_roa)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_org.RData"))

jordadf(jorda_org, jorda_f_org, 8, "High Org. Share of K")

rm(jorda_org, jorda_f_org)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ/jorda_young.RData"))

jordadf(jorda_young, jorda_f_young, 8, "Young Firms")

rm(jorda_young, jorda_f_young)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_b.RData"))
jorda_b_f_neg <- jorda_b_f
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_b.RData"))
jorda_split(jorda_b_f, jorda_b_f_neg, 8, "Baseline")
rm(jorda_b_f_pos, jorda_b_f_neg)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_mnc.RData"))
jorda_f_mnc_neg <- jorda_f_mnc
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_mnc.RData"))
jorda_split(jorda_f_mnc, jorda_f_mnc_neg, 8, "MNC")
rm(jorda_b_f_pos, jorda_b_f_neg)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_big.RData"))
jorda_f_big_neg <- jorda_f_big
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_big.RData"))
jorda_split(jorda_f_big, jorda_f_big_neg, 8, "Big Firms")
rm(jorda_b_f_pos, jorda_b_f_neg)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_div.RData"))
jorda_f_div_neg <- jorda_f_div
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_div.RData"))
jorda_split(jorda_f_div, jorda_f_div_neg, 8, "Dividend Payers")
rm(jorda_f_div, jorda_f_div_neg)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_lev.RData"))
jorda_f_lev_neg <- jorda_f_lev
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_lev.RData"))
jorda_split(jorda_f_lev, jorda_f_lev_neg, 8, "High Leverage")
rm(jorda_f_lev, jorda_f_lev_neg)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_hp.RData"))
jorda_f_hp_con_neg <- jorda_f_hp
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_hp.RData"))
jorda_split(jorda_f_hp, jorda_f_hp_con_neg, 8, "Hadlock-Pierce Constrained")
rm(jorda_f_hp, jorda_f_hp_con_neg)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_young.RData"))
jorda_f_young_neg <- jorda_f_young
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_young.RData"))
jorda_split(jorda_f_young, jorda_f_young_neg, 8, "Young Firms")
rm(jorda_f_young, jorda_f_young_neg)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_roa.RData"))
jorda_f_roa_neg <- jorda_f_roa
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_roa.RData"))
jorda_split(jorda_f_roa, jorda_f_roa_neg, 8, "ROA")
rm(jorda_f_roa, jorda_f_roa_neg)






data_dir <- 'C:/Users/Jackson_Mejia/Documents/ERM_Practice/'

source(file.path(data_dir, "macro_invt_elasticities/Scripts/FUNCTIONS.R"))




# Q_CI_ADJ

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT/jorda_b.RData"))

jordadf(jorda_b, jorda_b_f, 8, "Baseline")

rm(jorda_b, jorda_b_f)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_mnc.RData"))

jordadf(jorda_mnc, jorda_f_mnc, 8, "Multinationals")

rm(jorda_mnc, jorda_f_mnc)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT/_NAICS/NEUT/jorda_big.RData"))

jordadf(jorda_big, jorda_f_big, 8, "Big Firms")

rm(jorda_big, jorda_f_big)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_div.RData"))

jordadf(jorda_div, jorda_f_div, 8, "Dividend Payers")

rm(jorda_div, jorda_f_div)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_hp.RData"))

jordadf(jorda_hp, jorda_f_hp, 8, "Hadlock-Pierce Contrained")

rm(jorda_hp, jorda_f_hp)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_intan.RData"))

jordadf(jorda_intan, jorda_f_intan, 8, "High Intangible Share of K")

rm(jorda_intan, jorda_f_intan)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_know.RData"))

jordadf(jorda_know, jorda_f_know, 8, "High Knowledge Share of K")

rm(jorda_know, jorda_f_know)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_kz.RData"))

jordadf(jorda_kz, jorda_f_kz, 8, "Kaplan-Zingales Constrained")

rm(jorda_kz, jorda_f_kz)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_lev.RData"))

jordadf(jorda_lev, jorda_f_lev, 8, "High Leverage Firms")

rm(jorda_lev, jorda_f_lev)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_roa.RData"))

jordadf(jorda_roa, jorda_f_roa, 8, "High ROA")

rm(jorda_roa, jorda_f_roa)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_org.RData"))

jordadf(jorda_org, jorda_f_org, 8, "High Org. Share of K")

rm(jorda_org, jorda_f_org)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NAICS/NEUT//jorda_young.RData"))

jordadf(jorda_young, jorda_f_young, 8, "Young Firms")

rm(jorda_young, jorda_f_young)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_b.RData"))
jorda_b_f_neg <- jorda_b_f
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_b.RData"))
jorda_split(jorda_b_f, jorda_b_f_neg, 8, "Baseline")
rm(jorda_b_f_pos, jorda_b_f_neg)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_mnc.RData"))
jorda_f_mnc_neg <- jorda_f_mnc
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_mnc.RData"))
jorda_split(jorda_f_mnc, jorda_f_mnc_neg, 8, "MNC")
rm(jorda_b_f_pos, jorda_b_f_neg)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_big.RData"))
jorda_f_big_neg <- jorda_f_big
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_big.RData"))
jorda_split(jorda_f_big, jorda_f_big_neg, 8, "Big Firms")
rm(jorda_b_f_pos, jorda_b_f_neg)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_div.RData"))
jorda_f_div_neg <- jorda_f_div
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_div.RData"))
jorda_split(jorda_f_div, jorda_f_div_neg, 8, "Dividend Payers")
rm(jorda_f_div, jorda_f_div_neg)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_lev.RData"))
jorda_f_lev_neg <- jorda_f_lev
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_lev.RData"))
jorda_split(jorda_f_lev, jorda_f_lev_neg, 8, "High Leverage")
rm(jorda_f_lev, jorda_f_lev_neg)

load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_hp.RData"))
jorda_f_hp_con_neg <- jorda_f_hp
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_hp.RData"))
jorda_split(jorda_f_hp, jorda_f_hp_con_neg, 8, "Hadlock-Pierce Constrained")
rm(jorda_f_hp, jorda_f_hp_con_neg)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_young.RData"))
jorda_f_young_neg <- jorda_f_young
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_young.RData"))
jorda_split(jorda_f_young, jorda_f_young_neg, 8, "Young Firms")
rm(jorda_f_young, jorda_f_young_neg)


load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_NEG_NEG/jorda_roa.RData"))
jorda_f_roa_neg <- jorda_f_roa
load(file.path(data_dir, "macro_invt_elasticities/Jorda_LPs/Q_CI_ADJ_POS/jorda_roa.RData"))
jorda_split(jorda_f_roa, jorda_f_roa_neg, 8, "ROA")
rm(jorda_f_roa, jorda_f_roa_neg)

