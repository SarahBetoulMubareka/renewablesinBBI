
library(tidyr)
library(dplyr)
library(eurostat)
library(ggplot2)

# ---- Functions ----
# -- function binding dataframes with different numbers of columns --
rbind.all.columns <- function(x, y) {
  
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  
  x[, c(as.character(y.diff))] <- NA
  
  y[, c(as.character(x.diff))] <- NA
  
  return(rbind(x, y))
}

# -- function fill time --
fillValueTime <- function(inputSet, value, year, dependentVars){ # -- inputSet: data frame to expand, value: name of the column with the value (as charachter), year: name of the time variable, dependentVars: list of the dependent variables to use for expanding(vector of characters), NB do not use variables that are linked by definition (e.g. code and label)
  completeSet <- inputSet[,c(value, year, dependentVars)]
  colnames(completeSet)[1:2] <- c("value", "year")
  completeSet[, c(dependentVars, "year")] <- apply(completeSet[, c(dependentVars, "year")],2, FUN=as.character)
  completeSet <- merge(apply((data.frame(expand.grid(apply(completeSet[,c(dependentVars, "year")], 2, unique)))), 2, FUN=as.character), completeSet, all.x=TRUE)
  completeSet[, c(dependentVars, "year")] <- apply(completeSet[, c(dependentVars, "year")],2, FUN=as.character)
  completeSet$year <- as.numeric(as.character(completeSet$year))
  
  completeSet <- merge(completeSet[is.na(completeSet$value)|is.nan(completeSet$value), ], completeSet[!is.na(completeSet$value)&!is.nan(completeSet$value), ], by.x =dependentVars, by.y = dependentVars)
  completeSet$yeardif <-  completeSet$year.x - completeSet$year.y
  completeSetp <- merge(completeSet, aggregate(as.formula(paste("yeardif ~ year.x +", paste(dependentVars, collapse=" + "))), FUN = min, data=completeSet[completeSet$yeardif>0, ]))[, c(dependentVars, "year.x", "year.y",  "value.y")]
  colnames(completeSetp)[(ncol(completeSetp)-2):ncol(completeSetp)] <- c("year", "year_p", "value_p")
  
  completeSetn <- merge(completeSet, aggregate(as.formula(paste("yeardif ~ year.x +", paste(dependentVars, collapse=" + "))), FUN = max, data=completeSet[completeSet$yeardif<0, ]))[, c(dependentVars, "year.x", "year.y",  "value.y")]
  colnames(completeSetn)[(ncol(completeSetn)-2):ncol(completeSetn)] <- c("year", "year_n", "value_n")
  
  completeSet <- merge(completeSetp, completeSetn, all=TRUE)
  
  completeSet$value_est <- with(completeSet, ifelse(is.na(value_n), value_p, ifelse(is.na(value_p), value_n, (value_p-value_n)/(year_p-year_n)*(year-year_n)+value_n  )))
  
  completeSet <- merge(inputSet, completeSet, by.x = c(year, dependentVars), by.y = c("year", dependentVars), all=TRUE)
  completeSet$value_est <- ifelse(!is.na(completeSet[, value]), completeSet[, value], completeSet$value_est)
  
  return(completeSet)
}



## ---- Data preparation ----
# country selection tibble

EU28 <- c("AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK", "UK")
EU28Countries <- tibble(geo=EU28)
EU28Countries <- label_eurostat(EU28Countries, code="geo")

# Energy balance
nrg_bal_c <- get_eurostat("nrg_bal_c", time_format = "num")
nrg_bal_c <- label_eurostat(nrg_bal_c, code = c("nrg_bal", "siec", "unit", "geo"))
# saveRDS(nrg_bal_c, "nrg_bal_c.RDS")
# nrg_bal_c %>% filter(unit_code=="GWH", geo_code=="EU28", time==2015, grepl("bio", siec, ignore.case=TRUE) & !grepl("excluding bio", siec, ignore.case=TRUE)) %>% write.csv("clipboard-2048")
# nrg_bal_c %>% filter(geo_code=="DE", time==2015) %>% saveRDS("nrg_bal_c_DE_2015.RDS") 
# Keep only data in terajoules which is the unit used in the environmental accounts
nrg_bal_c <- nrg_bal_c %>% filter(unit_code %in% c("TJ"))

# list of SIEC codes excluding those that correspond to aggregates, needed to be able to avoid double counting in the processing
single_siec_code <- nrg_bal_c %>% select(siec_code, siec) %>% unique() %>% filter (!siec_code %in% c("BIOE", "FE", "C0000X0350-0370", "C0350-0370", "O4000XBIO", "RA000", "W6100_6220", "TOTAL"))

# Shares of renewables in electricity, heating and cooling, transport and total
nrg_ind_ren <- get_eurostat("nrg_ind_ren", time_format = "num")
nrg_ind_ren <- label_eurostat(nrg_ind_ren, code = c("nrg_bal", "unit", "geo"))


# Energy accounts
env_ac_pefa04 <- get_eurostat("env_ac_pefa04", time_format = "num")
env_ac_pefa04 <- label_eurostat(env_ac_pefa04, code = c("nace_r2", "indic_pefa", "unit", "geo"))
# saveRDS(env_ac_pefa04, "env_ac_pefa04.RDS")

env_ac_pefa05 <- get_eurostat("env_ac_pefa05", time_format = "num")
env_ac_pefa05 <- label_eurostat(env_ac_pefa05, code = c("indic_pefa", "unit", "geo"))
# saveRDS(env_ac_pefa05, "env_ac_pefa05.RDS")

env_ac_pefasu <- get_eurostat("env_ac_pefasu", time_format = "num")
env_ac_pefasu <- label_eurostat(env_ac_pefasu, code = c("stk_flow", "nace_r2", "prod_nrg", "unit", "geo"))
## saveRDS(env_ac_pefasu, "env_ac_pefasu.RDS")

# National accounts: Use table at purchasers' prices
naio_10_cp16 <- get_eurostat("naio_10_cp16", time_format = "num")
naio_10_cp16 <- label_eurostat(naio_10_cp16, code = c("stk_flow", "induse", "prod_na", "unit", "geo"))
# saveRDS(naio_10_cp16, "naio_10_cp16.RDS")

## ---- fixed tables for selection and connecting data ----
# SIEC of renewable energy sources (source Eurostat / RAMON & Regulation (EC) No 1099/2008 on energy statistics)
ren_siec <- ("siec	siec_code
Primary solid biofuels	R5110-5150_W6000RI
Charcoal	R5160
Blended biogasoline	R5210B
Pure biogasoline	R5210P
Blended biodiesels	R5220B
Pure biodiesels	R5220P
Blended bio jet kerosene	R5230B
Pure bio jet kerosene	R5230P
Other liquid biofuels	R5290
Biogases	R5300
Hydro	RA100
Geothermal	RA200
Wind	RA300
Solar thermal	RA410
Solar photovoltaic	RA420
Tide, wave, ocean	RA500
Ambient heat (heat pumps)	RA600
Renewable municipal waste	W6210") %>% 
  read.table(text = ., sep ="\t", header = TRUE, stringsAsFactors = FALSE) %>% 
  mutate(ren="Y") %>% 
  right_join(single_siec_code) %>% 
  mutate(ren=ifelse(is.na(ren), ifelse(siec_code %in% c("E7000", "H8000"), "P", "N"), ren),  # Flag renewables: yes, no, partially 
         bioenergy=ifelse(substr(siec_code, 1, 2) %in% c("R5"),1 , 0)) # Here, we consider that for bio-fuels, the bio-based share is considered.


# sectors including a bioeconomy component and corresponding NACE Rev. 2 codes

cor_nrg_bal_nace <- ("nrg_bal_code	nrg_bal	nace_r2_code_list	nace_r2_accounts
FC_OTH_AF_E	Final consumption - other sectors - agriculture and forestry - energy use	A01, A02	A01, A02
FC_OTH_FISH_E	Final consumption - other sectors - fishing - energy use	A03	A03
FC_IND_FBT_E	Final consumption - industry sector - food, beverages and tobacco - energy use	C10, C11, C12	C10-C12
FC_IND_TL_E	Final consumption - industry sector - textile and leather - energy use	C13, C14, C15	C13-C15
FC_IND_WP_E	Final consumption - industry sector - wood and wood products - energy use	C16	C16
FC_IND_PPP_E	Final consumption - industry sector - paper, pulp and printing - energy use	C17, C18	C17, C18
FC_IND_CPC_E	Final consumption - industry sector - chemical and petrochemical - energy use	C20, C21	C20, C21
FC_IND_NSP_E	Final consumption - industry sector - not elsewhere specified - energy use	C22, C31, C32	C22, C31_C32") %>% read.table(text = ., sep ="\t", header = TRUE, stringsAsFactors = FALSE)

# Correspondence table with the NACE Rev. 2 to link to SBS
cor_nrg_bal_nace_ext <- cor_nrg_bal_nace %>% 
  mutate(strsplit(cor_nrg_bal_nace$nace_r2_code, ", ") %>%
           plyr::ldply(rbind)) %>% pivot_longer(c("1", "2","3")) %>% 
  filter(!is.na(value)) %>% 
  select(-"name") %>% 
  rename(nace_r2_code=value)

# Correspondence table with the NACE Rev. 2 to link to environmental accounts
cor_nrg_bal_accounts_ext <- cor_nrg_bal_nace %>% 
  mutate(strsplit(cor_nrg_bal_nace$nace_r2_accounts, ", ") %>%
           plyr::ldply(rbind)) %>% pivot_longer(c("1", "2")) %>% 
  filter(!is.na(value)) %>% 
  mutate(nace_r2_code=as.character(value)) %>% 
select(-name, -value)

# Full correspondence table between the energy codes, the NACE rev. 2 codes used the accounts and in SBS
cor_nace <- 
  cor_nrg_bal_accounts_ext %>% 
  select(nrg_bal_code, nace_r2_code) %>% 
  rename(nace_r2_accounts=nace_r2_code) %>% 
  full_join(cor_nrg_bal_nace_ext %>% select(nrg_bal_code, nace_r2_code)) %>% 
  filter(nace_r2_accounts==nace_r2_code | nchar(nace_r2_accounts)>3) %>% 
  filter(!(nace_r2_accounts=="C31_C32" & nace_r2_code=="C22"))

  
# bio-based sectors identified in Ronzon et al. 2018

bioeco_sectors <- ("Sector.NACE.rev.2.bio	nace_r2_bio_code	nace_r2_code# nace_r2_code are parent codes in the NACE
Agriculture	A01	A01
Forestry	A02	A02
Fishing and Aquaculture	A03	A03
Food	C10	C10
Beverage	C11	C11
Tobacco	C12	C12
Bio-based textile	bC13	C13
Bio-based wearing apparel	bC14	C14
Leather	bC15	C15
Wood products	bC16	C16
Paper	bC17	C17
Bio-based chemicals (excluding biofuels)	bchem	C20# (except C2059 and C2014)
Bioethanol	Bioeth	C2014
Biodiesel	Biod	C2059
Bio-based pharmaceuticals	bC21	C21
Rubber and bio-based plastics	bC22	C22
Wooden furniture	bC31	C31") %>% 
  read.table(text = ., sep ="\t", header = TRUE, stringsAsFactors = FALSE)




# Correspondence between SIEC and PEFA's prod_nrg_code
siec_pefa_nrg <- ("siec_code	prod_nrg_code	siec	prod_nrg
C0110	P08	Anthracite	Hard coal
C0121	P08	Coking coal	Hard coal
C0129	P08	Other bituminous coal	Hard coal
C0210	P09	Sub-bituminous coal	Brown coal and peat
C0220	P09	Lignite	Brown coal and peat
C0311	P11	Coke oven coke	Secondary coal products (coke, coal tar, patent fuel, BKB and peat products)
C0312	P11	Gas coke	Secondary coal products (coke, coal tar, patent fuel, BKB and peat products)
C0320	P11	Patent fuel	Secondary coal products (coke, coal tar, patent fuel, BKB and peat products)
C0330	P11	Brown coal briquettes	Secondary coal products (coke, coal tar, patent fuel, BKB and peat products)
C0340	P11	Coal tar	Secondary coal products (coke, coal tar, patent fuel, BKB and peat products)
C0350	P10	Coke oven gas	Derived gases (= manufactured gases excl. biogas)
C0360	P10	Gas works gas	Derived gases (= manufactured gases excl. biogas)
C0371	P10	Blast furnace gas	Derived gases (= manufactured gases excl. biogas)
C0379	P10	Other recovered gases	Derived gases (= manufactured gases excl. biogas)
E7000	P26	Electricity	Electrical energy
G3000	P13	Natural gas	Natural gas (without bio)
H8000	P27	Heat	Heat
N900H	P22	Nuclear heat	Nuclear fuel
N900H	N02	Nuclear heat	Nuclear non-renewable natural energy inputs
O4100_TOT	P12	Crude oil	Crude oil, NGL, and other hydrocarbons (excl. bio)
O4200	P12	Natural gas liquids	Crude oil, NGL, and other hydrocarbons (excl. bio)
O4300	N01	Refinery feedstocks	Fossil non-renewable natural energy inputs
O4400X4410	P21	Additives and oxygenates (excluding biofuel portion)	Other petroleum products incl. additives/oxygenates and refinery feedstocks
O4500	P12	Other hydrocarbons	Crude oil, NGL, and other hydrocarbons (excl. bio)
O4610	P20	Refinery gas	Refinery gas, ethane and LPG
O4620	P20	Ethane	Refinery gas, ethane and LPG
O4630	P20	Liquefied petroleum gases	Refinery gas, ethane and LPG
O4640	P16	Naphtha	Naphtha
O4651	P14	Aviation gasoline	Motor spirit (without bio)
O4652XR5210B	P14	Motor gasoline (excluding biofuel portion)	Motor spirit (without bio)
O4653	P15	Gasoline-type jet fuel	Kerosenes and jet fuels (without bio)
O4661XR5230B	P15	Kerosene-type jet fuel (excluding biofuel portion)	Kerosenes and jet fuels (without bio)
O4669	P15	Other kerosene	Kerosenes and jet fuels (without bio)
O4671XR5220B	P17	Gas oil and diesel oil (excluding biofuel portion)	Transport diesel (without bio)
O4671XR5220B	P18	Gas oil and diesel oil (excluding biofuel portion)	Heating and other gasoil (without bio)
O4680	P19	Fuel oil	Residual fuel oil
O4691	P21	White spirit and special boiling point industrial spirits	Other petroleum products incl. additives/oxygenates and refinery feedstocks
O4692	P21	Lubricants	Other petroleum products incl. additives/oxygenates and refinery feedstocks
O4693	P21	Paraffin waxes	Other petroleum products incl. additives/oxygenates and refinery feedstocks
O4694	P21	Petroleum coke	Other petroleum products incl. additives/oxygenates and refinery feedstocks
O4695	P21	Bitumen	Other petroleum products incl. additives/oxygenates and refinery feedstocks
O4699	P21	Other oil products n.e.c.	Other petroleum products incl. additives/oxygenates and refinery feedstocks
P1100	P09	Peat	Brown coal and peat
P1200	P11	Peat products	Secondary coal products (coke, coal tar, patent fuel, BKB and peat products)
R5110-5150_W6000RI	P23	Primary solid biofuels	Wood, wood waste and other solid biomass, charcoal
R5110-5150_W6000RI	N06	Primary solid biofuels	Biomass based renewable natural energy inputs
R5160	P23	Charcoal	Wood, wood waste and other solid biomass, charcoal
R5210B	P24	Blended biogasoline	Liquid biofuels
R5210P	P24	Pure biogasoline	Liquid biofuels
R5220B	P24	Blended biodiesels	Liquid biofuels
R5220P	P24	Pure biodiesels	Liquid biofuels
R5230B	P24	Blended bio jet kerosene	Liquid biofuels
R5230P	P24	Pure bio jet kerosene	Liquid biofuels
R5290	P24	Other liquid biofuels	Liquid biofuels
R5300	P25	Biogases	Biogas
RA100	N03	Hydro	Hydro based renewable natural energy inputs
RA200	N07	Geothermal	Other renewable natural energy inputs
RA300	N04	Wind	Wind based renewable natural energy inputs
RA410	N05	Solar thermal	Solar based renewable natural energy inputs
RA420	N05	Solar photovoltaic	Solar based renewable natural energy inputs
RA500	N07	Tide, wave, ocean	Other renewable natural energy inputs
RA600	N07	Ambient heat (heat pumps)	Other renewable natural energy inputs
S2000	P09	Oil shale and oil sands	Brown coal and peat
W6100	R29	Industrial waste (non-renewable)	Non-renewable waste
W6210	R28	Renewable municipal waste	Renewable waste
W6220	R29	Non-renewable municipal waste	Non-renewable waste") %>% 
  read.table(text = ., sep ="\t", header = TRUE, stringsAsFactors = FALSE)

siec_pefa_nrg <- siec_pefa_nrg %>% left_join(siec_pefa_nrg %>% select(siec_code,prod_nrg_code)%>% group_by(siec_code) %>% summarise(s_prod_nrg_code=paste(prod_nrg_code, collapse=", ")))

# correspondance between the energy transformation sector and the use of energy in the energy sector
nrg_consumption_transformation <- ("nrg_cons_code	nrg_bal_code
NRG_EHG_E	TO_EHG
NRG_CM_E	TO_CO
NRG_CM_E	TO_CL
NRG_OIL_NG_E	TO_GW
NRG_OIL_NG_E	TO_RPI
NRG_OIL_NG_E	TO_BNG
NRG_OIL_NG_E	TO_GTL
NRG_PF_E	TO_PF
NRG_CO_E	TO_CO
NRG_BKBPB_E	TO_BKBPB
NRG_GW_E	TO_GW
NRG_BF_E	TO_BF
NRG_PR_E	TO_RPI
NRG_NI_E	TO_EHG_MAPE
NRG_CL_E	TO_CL
NRG_LNG_E	TO_GTL
NRG_BIOG_E	TO_LBB
NRG_GTL_E	TO_GTL
NRG_CPP_E	TO_CPP
NRG_NSP_E	TO_NSP") %>% 
  read.table(text = ., sep ="\t", header = TRUE, stringsAsFactors = FALSE)

# ---- Procedure based on DATAM to recalculate the shares -----
#  usefull only in absense of direct data from JRC.D4
# shares of bioeconomy in mixed sectors (from DATAM/SBS)
# DATAM data import to compute the share of bio-based in mixed sectors
datam <- read.csv("Dataset_JRC_-_Bioeconomics.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(datam) <- c("time","geo","geo_code", "indic_sb", "indic_sb_code", "nace_r2_bio", "nace_r2_bio_code", "unit", "value_bioeco")

sbs_na_ind_r2 <- get_eurostat("sbs_na_ind_r2", time_format = "num")
sbs_na_ind_r2 <- label_eurostat(sbs_na_ind_r2, code = c("nace_r2", "indic_sb", "geo"), fix_duplicated = TRUE)
# subset the sbs dataset to reduce memory footprint, limits to EU countries, sectors C (manufacturing) and D (energy) and 5 indicators 
sbs_na_ind_r2 <- sbs_na_ind_r2 %>% filter(indic_sb_code %in% c("V11110", "V12110", "V12150", "V16110", "V20110"), geo_code %in% datam$geo_code, substr(nace_r2_code, 1, 1) %in% c("C", "D"))
# saveRDS(sbs_na_ind_r2, "sbs_na_ind_r2.RDS")
  
datam_shares <- sbs_na_ind_r2 %>% 
  filter(indic_sb_code %in% c("V16110", "V12110"), geo_code %in% datam$geo_code) %>% 
  inner_join(bioeco_sectors) %>% 
  left_join(datam %>% select(time, geo_code, indic_sb_code, nace_r2_bio_code, unit, value_bioeco)) %>% 
  mutate(bio_share=ifelse(value_bioeco<=values, value_bioeco/values, 1)) %>% 
  group_by(time, geo_code, nace_r2_code, nace_r2_bio_code) %>%
  summarise(bio_share=ifelse(mean(bio_share, na.rm = TRUE)>=0, mean(bio_share, na.rm = TRUE), NA)) %>% 
  fillValueTime(., "bio_share", "time", c("nace_r2_code", "nace_r2_bio_code", "geo_code")) %>%  # when there is no information for a specific year, fill with the closest value in time.
  mutate(bio_share=value_est) %>% 
  select(-c("year_p", "value_p", "year_n", "value_n", "value_est")) %>%
  mutate(bio_share=ifelse(is.na(bio_share) & nace_r2_bio_code==nace_r2_code , 1, bio_share)) # ************ this must be modified when data are available **********
#  filter(!is.na(bio_share)) %>% View()

datam_shares <- datam_shares %>% 
  left_join(datam_shares %>% 
              filter(geo_code=="EU28") %>% 
              select(-geo_code) %>% 
              rename(eu_bio_share = bio_share)) %>% 
  mutate(bio_share = ifelse(is.na(bio_share), eu_bio_share, bio_share)) %>% 
  select(-eu_bio_share)

datam_shares <- expand_grid(datam_shares %>% select(time, geo_code) %>% unique(), nace_r2_code=c("A01", "A02", "A03")) %>% # fill the value for the primary sectors considered 100% bio-based
  mutate(nace_r2_bio_code=nace_r2_code, bio_share=1) %>% 
  rbind(datam_shares) %>% 
  filter(!geo_code %in% c("EU28"))
# select(nace_r2_code, indic_sb_code, geo_code, time, bio_share) %>% #for control
# pivot_wider(values_from = bio_share, names_from=indic_sb_code) %>% 
# View()
# -- inconsistencies between datam and SBS: datam does not have any negative value.
# sbs_na_ind_r2 %>% filter(values<0) %>% View()
# datam %>%  filter(value_bioeco<0) %>% View()

# sbs_na_ind_r2 <- sbs_na_ind_r2 %>% mutate(time=format(time, format="%Y")) 

# ---- Share of bio-based activities in the NACE level 2 activities ----
# incorporate shares at NACE level 4 sent by Robert M'Barek on 14.01.2022 for internal use
# and calculate the shares at level 2 of the the NACE, for manufacturing sectors only
# the dataset does not contain information on some bio-based activities: A01, A02, A03, C10, C11, C12 (shares considered 100%)

BBshare_avg <- as_tibble(read.csv("2021_10_29_bb_shares_avg_qlik.csv", stringsAsFactors = FALSE))
BBshare_avg %>% 
  select(NACE, Attribute) %>% 
  unique() %>% 
  rename(nace_r2_bio_code=NACE) %>%
  full_join(bioeco_sectors) %>% 
  mutate(Attribute=ifelse(is.na(Attribute), "NA", ifelse(Attribute=="", "none", Attribute))) %>% 
  pivot_wider(names_from="Attribute", values_from = nace_r2_code, names_repair = "check_unique") %>% 
  View() # present the correspondance between bio-based activities and NACE activities and the indicators available

# Prepare a biobased-shrare table at NACE level 4, with only the share for V12110 (or other if V12110 is not available)
# all shares are the same at that level.
BBshare <- BBshare_avg %>% 
  filter(nchar(NACE)==6) %>% 
  pivot_wider(names_from = Attribute, values_from=Share_.) %>% 
  rename(shareBBI=V12110) %>%  
  mutate(shareBBI=ifelse(is.na(shareBBI), ifelse(is.na(V12150), V16110, V12150), shareBBI)) %>%
  select(-V12150, -V16110) %>% 
  rename(time=Year, geo_code=Country.code, geo=Country, nace_r2_bio_code=NACE)

# Fill in the missing shares with the last known, first known or regression between the previous and next known.
BBshare <- expand.grid(geo_code=unique(BBshare$geo_code), 
                       time=unique(env_ac_pefasu$time), # env_ac_pefasu$time is used instead of BBshare$time because the shares do not cover 2020 yet, whereas all other data do
                       nace_r2_bio_code=unique(BBshare$nace_r2_bio_code)) %>% 
  full_join(BBshare) %>% 
  fillValueTime(., "shareBBI", "time", c("geo_code", "nace_r2_bio_code")) %>% 
  select(geo_code, nace_r2_bio_code, time, value_est) %>% 
  rename(shareBBI=value_est)

# Add the sectors for which there is no information and fill in with the average EU27
BBshare <- BBshare %>%
  left_join(BBshare %>% filter(geo_code %in% "EU27") %>% rename(EU27ShareBBI=shareBBI) %>% select(-geo_code)) %>% 
  mutate(shareBBI=ifelse(is.na(shareBBI), EU27ShareBBI, shareBBI)) %>% select(-EU27ShareBBI)

# BBshare %>% group_by(nace_r2_bio_code, Country.code) %>% summarise(meanShare=mean(shareBBI, na.rm = TRUE)) %>% pivot_wider(values_from = meanShare, names_from=nace_r2_bio_code) %>% View
# BBshare %>% pivot_wider(values_from = shareBBI, names_from=nace_r2_bio_code) %>% View

# find data with issues: the values at NACE level 4 do not sum up to the values at NACE level 2 (not used afterwards)

sectors_l2_with_issues <- sbs_na_ind_r2 %>% 
  filter(indic_sb_code %in% "V20110", nchar(nace_r2_code)==5, substr(nace_r2_code, 1,1) %in% "C") %>% 
  mutate(nace_r2_code=substr(nace_r2_code, 1,3)) %>% 
  group_by(geo_code, time, nace_r2_code) %>% 
  summarise(total=sum(values, na.rm = TRUE)) %>% 
  left_join(sbs_na_ind_r2 %>% filter(indic_sb_code %in% "V20110")) %>% 
  mutate(diff=total-values, ratio=total/values) %>% filter ( abs(diff)>0.3, abs(ratio-1)>0.1, !is.na(diff))
  # select(-nace_r2) %>% 
  # left_join(sbs_na_ind_r2 %>% 
  #             filter(indic_sb_code %in% "V20110", nchar(nace_r2_code)==5, substr(nace_r2_code, 1,1) %in% "C") %>% 
  #             rename(nace_r2_c4 = nace_r2_code,  value_c4 = values) %>% 
  #             mutate(nace_r2_code = substr(nace_r2_c4,1,3)) ) %>%
  # View()


# When there is a discrepancy between level 4 and level 2 data, remove BBshares calculated using nace level 4 to replace them by the average from the DATAM calculation
BBshare <- BBshare %>% filter(geo_code %in% c(EU28, "EU27", "EU28")) %>%  
    mutate(nace_r2_code=ifelse(nace_r2_bio_code %in% c("Bioeth"), "C2014", substr(nace_r2_bio_code, 2, length(nace_r2_bio_code)))) %>% 
    left_join(sbs_na_ind_r2 %>% filter(indic_sb_code %in% "V20110")) %>% 
  mutate(energy_purchase=values*shareBBI/100, nace_r2_code=substr(nace_r2_code, 1,3)) %>% 
  group_by(geo_code, time, nace_r2_code) %>% 
  summarise(energy_purchase4BBIs=sum(energy_purchase, na.rm = TRUE)) %>% 
  left_join(sbs_na_ind_r2 %>% filter(indic_sb_code %in% "V20110")) %>% 
  mutate(energy_purchase4BBIs=energy_purchase4BBIs / values) %>% 
  select(geo_code, time, nace_r2_code, energy_purchase4BBIs) %>% 
  left_join(as_tibble(read.csv("2021_10_29_bb_shares_avg_qlik.csv", stringsAsFactors = FALSE)) %>% 
              filter(nchar(NACE)==4) %>% 
              mutate(nace_r2_code=substr(NACE, 2, 4), share=Share_./100, Attribute = ifelse(Attribute %in% "", "All", "Mean")) %>% # for a reason, the Attribute "" is in many cases very different from the estimates by indicator, therefore, the corresponding value is only used if there is no other information
              rename(geo_code=Country.code, time=Year) %>%
              select(-Country, -Share_.)%>% 
              group_by(geo_code, time, nace_r2_code, Attribute) %>% summarise(share=mean(share, na.rm=TRUE)) %>%
              pivot_wider(values_from = share, names_from=Attribute) %>%
              mutate(share=ifelse(is.na(Mean), All, Mean))
            ) %>% 
  mutate(BBI_energy_share=ifelse(is.na(energy_purchase4BBIs)|abs(energy_purchase4BBIs-share)>0.05, share, energy_purchase4BBIs)) %>% 
  filter(substr(nace_r2_code, 1,1) %in% "C") 

BBshare <- BBshare %>% mutate(BBI_energy_share=ifelse(BBI_energy_share>1, 1, BBI_energy_share)) # because of rounding, the BBI_energy_share may be higher than 1. This is corrected here.

# Patch because energy purchase is not reported yet in SBS for 2020
BBshare <- BBshare %>% left_join(BBshare %>% filter(time==2019) %>% ungroup() %>%  select(geo_code, nace_r2_code, BBI_energy_share) %>% rename(BBI_energy_share2019 = BBI_energy_share)) %>% 
  mutate(BBI_energy_share = ifelse(is.na(BBI_energy_share) & time==2020, BBI_energy_share2019, BBI_energy_share)) %>% 
  select(-BBI_energy_share2019)
  

# Add 1 for A01, A02, A03, C10, C11, C12
BBshare  <- expand.grid(geo_code=unique(BBshare$geo_code), 
                        time=unique(BBshare$time), 
                        nace_r2_code=c("A01", "A02", "A03", "C10", "C11", "C12")) %>% 
    as_tibble() %>% 
    mutate(BBI_energy_share=1) %>% 
    bind_rows(BBshare)


## ---- Preprocessing ----

# # Calculate the share of renewables including the part from renewable heat and power.
# nrg_bal_c %>% 
#       # Extract data on sectors including a bioeconomy component, as defined in the table "cor_nrg_bal_nace"
#   right_join(cor_nrg_bal_nace) %>% 
#       # and select only the total energy, energy from renewable as well as energy as electricity or heat:  siec_code in TOTAL, RA000, E7000, H8000
#   filter(siec_code %in% c("TOTAL", "RA000", "E7000", "H8000")) %>% 
#       # multiplied by the share of renewable used in the production of electricity and heat
#       # Extracted from NRG_IND_REN, with nrg_bal in REN_ELEC and REN_HEAT_CL
#   left_join(
#     nrg_ind_ren %>% 
#       mutate(siec_code=ifelse(nrg_bal_code=="REN_ELC", "E7000", ifelse(nrg_bal_code=="REN_HEAT_CL", "H8000", NA)), share=values/100) %>% 
#       select(geo_code, time, siec_code, share) %>% 
#       filter(!is.na(siec_code))
#     ) %>% 
# 
#   # use 100% for non electricity or heat
#   mutate(share=ifelse(siec_code %in% c("RA000", "TOTAL"), 1, share)) %>% 
#   # compute the new value with the percentages
#   mutate(values=values * share) %>% 
#   # select only the main variables to create a wide table and calculate the shares
#   select(nrg_bal_code, unit_code, geo_code, time, nace_r2_code_list, siec_code, values) %>% 
#   pivot_wider(values_from = values, names_from=siec_code) %>% 
#   mutate(share=((E7000 + H8000 + RA000) / TOTAL)) %>% 
#   filter(time>2004, !nrg_bal_code %in% c("FC_OTH_AF_E", "FC_OTH_FISH_E")) %>% 
# 
#     # graph
#     group_by(nrg_bal_code) %>% 
#     ggplot(aes(as.factor(time), share, group=nrg_bal_code, colour=nrg_bal_code)) +
#     geom_line() +
#     geom_point()

# Note: DE: REN in fish only since 2019; Agri: disruptions in 2010 and 2018

# overview of the data
nrg_bal_c %>% filter(geo_code %in% EU28, nrg_bal_code%in%c("DL", "FC_E", "FC_NE", "TI_E", "NRG_E"), !siec_code %in% c("BIOE", "FE", "C0000X0350-0370", "C0350-0370", "O4000XBIO", "RA000", "W6100_6220", "TOTAL")) %>% # all energies, but no aggregated numbers
  select(nrg_bal_code, siec_code, unit_code, geo_code, time, values) %>% 
  mutate(values=ifelse(is.na(values), 0, values)) %>% 
  pivot_wider(values_from = values, names_from=nrg_bal_code)


# --- Coefficients linking the use of tranformed fuels to the use of energy to transform the fuel (NRG_E) ---

to_siec <- nrg_bal_c %>% filter(time >= 2010, geo_code %in% EU28, substr(nrg_bal_code, 1,3) %in% "TO_", !siec_code %in% c("BIOE", "FE", "C0000X0350-0370", "C0350-0370", "O4000XBIO", "RA000", "W6100_6220", "TOTAL")) %>% # all energies, but no aggregated numbers
  select(nrg_bal_code, siec_code, unit_code, geo_code, time, values) %>% filter(lengths(regmatches(nrg_bal_code, gregexpr("_", nrg_bal_code)))==1) %>% left_join(nrg_consumption_transformation)

nrg_e_to <- to_siec %>% 
  group_by(siec_code, unit_code, geo_code, time) %>% 
  summarise(total_to=sum(values, na.rm = TRUE)) %>% 
  mutate(total_to=ifelse(total_to>0, total_to, NA)) %>% 
  left_join(to_siec %>% 
              group_by(nrg_cons_code, unit_code, geo_code, time) %>% 
              summarise(total_siec=sum(values, na.rm = TRUE)) %>% 
              mutate(total_siec=ifelse(total_siec>0, total_siec, NA))) %>% 
  left_join(to_siec) %>% 
  mutate(share_pTJoutput=values/total_to/total_siec) %>% # values/total_to * values/total_siec * values
  filter(!is.na(values), values>0) %>% 
  left_join(
    nrg_bal_c %>% filter(time >= 2010, geo_code %in% EU28, substr(nrg_bal_code, 1,4) %in% "NRG_", !(nrg_bal_code %in% "NRG_E"), !siec_code %in% c("BIOE", "FE", "C0000X0350-0370", "C0350-0370", "O4000XBIO", "RA000", "W6100_6220", "TOTAL")) %>% # all energies, but no aggregated numbers
      select(nrg_bal_code, siec_code, unit_code, geo_code, time, values) %>% rename(nrg_cons_code=nrg_bal_code, siec_cons_code=siec_code, values_cons=values)
  ) %>%  mutate(values_cons_pTJoutput=values_cons*share_pTJoutput)



# --- Coefficients linking distribution losses to the energy consumption by fuel (SIEC codes) ---
# These loss coefficients apply to the consumption of all energy including the NRG_E 

# nrg_ratio <- nrg_bal_c %>% 
#   filter(nrg_bal_code%in%c("FC_E", "DL", "NRG_E"), siec_code %in% c(ren_siec$siec_code,"TOTAL", "E7000", "H8000")) %>% # all renewable energies and mixed renewable such as TOTAL, electricity and heat
#   select(nrg_bal_code, siec_code, unit_code, geo_code, time, values) %>% 
#   pivot_wider(values_from = values, names_from=nrg_bal_code) %>% 
#   mutate(FC_E=ifelse(is.na(FC_E), 0, FC_E), DL=ifelse(is.na(DL), 0, DL), NRG_E=ifelse(is.na(NRG_E), 0, NRG_E)) %>% 
#   mutate(final2gross_nrg_ratio=ifelse(FC_E==0, 1, (FC_E+DL+NRG_E)/FC_E))
  
nrg_ratio <- nrg_bal_c %>% 
  filter(geo_code %in% EU28, nrg_bal_code%in%c("DL", "FC_E", "FC_NE", "TI_E", "NRG_E"), !siec_code %in% c("BIOE", "FE", "C0000X0350-0370", "C0350-0370", "O4000XBIO", "RA000", "W6100_6220", "TOTAL")) %>% # all energies, but no aggregated numbers
  select(nrg_bal_code, siec_code, unit_code, geo_code, time, values) %>% 
  mutate(values=ifelse(is.na(values), 0, values)) %>% 
  pivot_wider(values_from = values, names_from=nrg_bal_code) %>% 
#  mutate(FC_E=ifelse(is.na(FC_E), 0, FC_E), DL=ifelse(is.na(DL), 0, DL), NRG_E=ifelse(is.na(NRG_E), 0, NRG_E)) %>% 
  mutate(FC_E2FC_EpDL=ifelse((FC_E + FC_NE + TI_E + NRG_E)==0, 1, (DL+FC_E + FC_NE + TI_E + NRG_E)/(FC_E + FC_NE + TI_E + NRG_E))) %>% 
  left_join(ren_siec %>% select(-siec))

# check high values
nrg_ratio %>% left_join(nrg_ratio %>% 
                          group_by(siec_code, unit_code, geo_code) %>% 
                          summarise(mean=mean(FC_E2FC_EpDL), sd=sd(FC_E2FC_EpDL))) %>% 
  filter((FC_E2FC_EpDL-mean)>3*sd|FC_E2FC_EpDL-mean>3) %>% View()

#nrg_bal_c%>% filter(time >= 2010, geo_code %in% EU28, substr(nrg_bal_code, 1,3) %in% c("TO", "AFC"), !siec_code %in% c("BIOE", "FE", "C0000X0350-0370", "C0350-0370", "O4000XBIO", "RA000", "W6100_6220", "TOTAL")) %>% # all energies, but no aggregated numbers
#  select(nrg_bal_code, siec_code, unit_code, geo_code, time, values) %>% pivot_wider(values_from = values, names_from=nrg_bal_code) %>% filter(!is.na(TO))

# nrg_ratio %>% 
#   fillValueTime(., "FC_E2FC_EpDL", "time", c("siec_code", "unit_code", "geo_code"))


# ---------------------------------------------------------
# ---- Viewing of the raw data and coherence analysis -----
# bypass in the normal calculation


## env_ac_pefasu %>% right_join(cor_nrg_bal_nace_ext) %>%  filter(stk_flow_code=="USE_END", time==2018, values>1) %>% write.csv("clipboard-1024")
## nrg_bal_c %>% filter(substr(nrg_bal_code, 1, 3)=="NRG", time==2018) %>% write.csv("clipboard-1024")
## nrg_bal_c %>% filter(nrg_bal_code%in%c("DL","EXP","FC_E", "IMP", "NRG_E", "TO", "TI_E", "STATDIFF"), time==2018) %>% write.csv("clipboard-1024")

# show the differnt fuels used in the activity of manufacturing products of wood and cork
env_ac_pefasu %>% 
  filter(geo_code %in% EU28, !(geo_code %in% "UK"), nace_r2_code=="C16", unit_code=="TJ", stk_flow_code=="USE_END", !prod_nrg_code%in%c("N00", "P00", "R00", "EPRD_OUSE", "N00_P00_R00"), time<2020) %>% # ER_USE, SUP, USE, USE_END, USE_TRS
  # graph
  group_by(prod_nrg_code, time) %>% 
  summarise(values=sum(values, na.rm = TRUE)) %>% 
  ggplot(aes(x=as.factor(time), y=values, group=paste(prod_nrg_code), colour=prod_nrg_code )) +
  geom_line() +
  geom_point()


# env_ac_pefasu %>% select(stk_flow_code, stk_flow) %>% unique()
# env_ac_pefasu %>% select(prod_nrg_code, prod_nrg) %>% unique() %>% View()
env_ac_pefasu %>%
  left_join(siec_pefa_nrg %>% select(siec_code, siec, prod_nrg_code)) %>% 
  filter(!is.na(values), !is.na(siec_code)) %>% 
  group_by(stk_flow_code, nace_r2_code, unit_code, geo_code, stk_flow, nace_r2, unit, geo, time, siec_code)


# Coherence control between nrg_bal_c and env_ac_pefasu
nrg_bal_c %>% 
  right_join(cor_nrg_bal_nace) %>% 
  left_join(siec_pefa_nrg %>% 
        select(siec_code, prod_nrg_code, prod_nrg) %>% 
          group_by(siec_code) %>% 
          summarise(prod_nrg_code=paste(prod_nrg_code, collapse = "-"), prod_nrg=paste(prod_nrg, collapse = "-"))) %>% 
  filter(!is.na(values), !is.na(prod_nrg_code)) %>% 
  group_by(geo_code, geo, nrg_bal_code, nrg_bal, nace_r2_code_list, prod_nrg_code, prod_nrg, unit_code, unit, time) %>% 
  summarise(values=sum(values)) %>% 

  inner_join(env_ac_pefasu %>% 
    inner_join(cor_nrg_bal_accounts_ext) %>% 
    filter(!is.na(values)) %>% 
    group_by(stk_flow_code, prod_nrg_code, unit_code, geo_code, unit, geo, time, nace_r2_code_list) %>% 
    summarise(values_pefasu=sum(values, na.rm=TRUE)) %>% 
      pivot_wider(values_from=values_pefasu, names_from=stk_flow_code) ) %>% 
#  View()
  write.csv("coherence_nrg_bal_c_env_ac_pefasu.csv")
  

# ---- Coefficients to downscale the energy statistics (reported in NACE groups) to the NACE activities reported in the environmental accounts ---- 
# NB: in environmetal accounts, 64 activities are considered

# PEFA Method 1
# pefa_sector_shares <- env_ac_pefasu %>% 
#   filter(geo_code %in% datam$geo_code, stk_flow_code %in% c("USE"), !prod_nrg_code %in% c("EPRD_OUSE", "SD_IO")) %>%   # Calculation using only the end-use of energy, remove energy for own use (which are only on the supply side) and statistical discrepancies (global numbers without sector/energy definition)
#   inner_join(cor_nrg_bal_accounts_ext) %>%      # adds the correspondence table between pefa and nrg data to be used as a parameter to open the sectors
#   inner_join(siec_pefa_nrg %>% select(prod_nrg_code, s_prod_nrg_code)) %>% #  attaches the summarised prod_nrg codes to connect to the totals calculated hereafter
#   group_by(stk_flow_code, nace_r2_code, unit_code, geo_code, stk_flow, 
#          nace_r2, time, nrg_bal_code, nrg_bal, nace_r2_code_list, nace_r2_accounts, 
#          s_prod_nrg_code) %>% # remove the prod_nrg_code, and prod_nrg to use only the summarised version s_prod_nrg_code
#   summarise(values=sum(values, na.rm=TRUE)) 
# 
# pefa_sector_shares <- pefa_sector_shares %>% 
#   left_join(pefa_sector_shares %>%                  # reopen the table to calculate the total energy consumption (by energy product) by groups of activities as reported in nrg_bal_c 
#              inner_join(cor_nrg_bal_accounts_ext) %>% # select only the sectors of interest
#     group_by(stk_flow_code, s_prod_nrg_code, unit_code, geo_code, time, nace_r2_code_list) %>% 
#     summarise(values_pefasu=sum(values, na.rm=FALSE))) %>% # the NAs are not removed to avoid biased estimates because of the absence of data.
#     filter(!is.na(values_pefasu)) %>% 
#   mutate(nrg_pefa=ifelse(values_pefasu>0, values/values_pefasu, NA)) %>% # Compute the shares
#   select(time, nace_r2_code, s_prod_nrg_code, geo_code, nrg_bal_code, values, values_pefasu, nrg_pefa) %>% #   keeps only the codes to simplify the use of the table.
#   fillValueTime(., "nrg_pefa", "time", c("nace_r2_code", "s_prod_nrg_code", "geo_code", "nrg_bal_code")) %>%  # when there is no information for a specific year, fill with the closest value in time.
#   mutate(nrg_pefa=value_est) %>% 
#   select(-c("year_p", "value_p", "year_n", "value_n", "value_est")) %>% 
#   left_join(siec_pefa_nrg %>% select(s_prod_nrg_code, siec_code) %>% distinct()) %>%           # add the links to the SIEC codes
#   left_join(ren_siec) %>% 
#   left_join(cor_nrg_bal_accounts_ext %>% select(-nrg_bal))
# 
# 
# pefa_sector_shares <- pefa_sector_shares %>% left_join(
#   pefa_sector_shares %>% 
#     group_by(time, nace_r2_code, geo_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts, ren) %>% 
#     summarise(values=sum(values, na.rm = TRUE), values_pefasu=sum(values_pefasu, na.rm = TRUE)) %>% 
#     mutate(shares_ren_nren=ifelse(values_pefasu>0, values/values_pefasu, NA)) %>% 
#     select(-c(values, values_pefasu))
# ) %>% 
#   mutate(nrg_pefa=ifelse(is.na(nrg_pefa), shares_ren_nren, nrg_pefa)) %>%
#   mutate(nrg_pefa = ifelse(!(s_prod_nrg_code  %in% c("N00_P00_R00", "N00", "P00", "R00")) & nace_r2_code==nace_r2_accounts & is.na(nrg_pefa), 1, nrg_pefa)) %>%  # sometimes the calculation of the share returns NA because the quantity is null. For sectors defined the same way in the energy balance and the energy accounts, we force the value 1
#   select(-c("shares_ren_nren"))  # %>% names() %>% paste(collapse = ", ")
# #  group_by(time, prod_nrg_code, geo_code, nrg_bal_code, stk_flow_code, unit_code, stk_flow, prod_nrg, unit, geo, nrg_bal, nace_r2_code_list, siec_code, siec, ren) %>% # check that the total is 1 for all categories
# #  summarise(shares=sum(nrg_pefa)-1) %>% filter(round(shares, 15)!=0) # except for the calculation rounding.
# 
# 
# pefa_sector_shares <- pefa_sector_shares %>% 
#   full_join(nrg_bal_c %>%                      # expands the dataset to make sure that every entry in the nrg_bal_c table has a corresponding factor 
#               inner_join(siec_pefa_nrg %>% select(s_prod_nrg_code, siec_code) %>% distinct()) %>% 
#               left_join(ren_siec) %>%
#               inner_join(cor_nrg_bal_accounts_ext) %>% 
#               filter(geo_code %in% EU28Countries$geo_code,  time %in% (pefa_sector_shares$time %>% unique)) %>% 
#               select(nrg_bal_code, siec_code, s_prod_nrg_code, ren, bioenergy, unit_code, geo_code, time, nace_r2_code_list, nace_r2_accounts, nace_r2_code))
# 
# # Fills in the gaps with linean interpolation if possible
# pefa_sector_shares2 <- pefa_sector_shares %>% select(time, nace_r2_code, s_prod_nrg_code, geo_code) %>% 
#   fillValueTime(., "nrg_pefa", "time", c("nace_r2_code", "geo_code", "siec_code")) %>%  # when there is no information for a specific year, fill with the closest value in time.
#   select(-c("year_p", "value_p", "year_n", "value_n", "value_est"))
# 
# # Check that energy consumption is 100% allocated 
# pefa_sector_shares2 %>% 
#   left_join(pefa_sector_shares2 %>% 
#     group_by(geo_code, nace_r2_code_list , time, nrg_bal_code, siec_code) %>%
#     summarise(notNull=round(sum(nrg_pefa, na.rm=TRUE), 5))) %>%
#     filter(notNull!=1) %>% 
#   View()



# PEFA Method 2

s_env_ac_pefasu <- env_ac_pefasu %>% 
  filter(geo_code %in% EU28Countries$geo_code, stk_flow_code %in% c("USE_END", "USE"), !prod_nrg_code %in% c("EPRD_OUSE", "SD_IO", "N00_P00_R00", "N00", "P00", "R00", "R31", "R30"), !substr(prod_nrg_code,1,1) %in% "N") %>%   # Calculation using only the end-use of energy, remove energy for own use (which are only on the supply side) and statistical discrepancies (global numbers without sector/energy definition
  select(-stk_flow) %>% 
  pivot_wider(names_from = stk_flow_code, values_from = values) %>% 
  mutate(values=ifelse(!is.na(USE_END), USE_END, USE)) %>% 
  select(-USE_END, -USE) %>% 
  inner_join(cor_nrg_bal_accounts_ext) %>%       # adds the correspondence table between pefa and nrg data to be used as a parameter to open the sectors
  left_join(siec_pefa_nrg %>% 
              select(prod_nrg_code, s_prod_nrg_code, siec_code)) %>% 
  group_by(nace_r2_code, unit_code, geo_code, nace_r2, prod_nrg, time, values, nrg_bal_code, nrg_bal, nace_r2_code_list, nace_r2_accounts, s_prod_nrg_code, siec_code) %>% 
  summarise(values=sum(values, na.rm = TRUE))
#  filter(!is.na(values))

# -- Identify the issues with the zeros for some sectors / energy sources --
#  (especially for heat and power, most zeros are in fact absence of information; undues zeros, i.e. if there is information in the time-series or in other types of energies are replaced by NAs) 
issue_pefasu <- s_env_ac_pefasu %>%
  group_by(siec_code, unit_code, geo_code, time, nace_r2_code, nace_r2_code_list, nrg_bal_code) %>%
  summarise(values=ifelse(sum(!is.na(values))>0, sum(values, na.rm=TRUE), NA)) %>%  # if at least one value is not NA
  left_join(s_env_ac_pefasu %>%                  # reopen the pefa table to calculate the total energy consumption (by energy product) by groups of activities as reported in nrg_bal_c
              group_by(siec_code, unit_code, geo_code, time, nace_r2_code, nace_r2_code_list, nrg_bal_code) %>%
              summarise(values_pefasu=ifelse(sum(!is.na(values))>0, sum(values, na.rm=TRUE), NA)) %>%
              ungroup() %>%
              group_by(siec_code, unit_code, geo_code, time, nace_r2_code_list, nrg_bal_code) %>%
              summarise(values_pefasu=sum(values_pefasu, na.rm=FALSE))
  ) %>%
  mutate(nrg_pefa=ifelse(values_pefasu>0, values/values_pefasu, NA)) %>% # Compute the shares
  select(time, nace_r2_code, siec_code, geo_code, nrg_bal_code, values, values_pefasu, nrg_pefa) %>% #   keeps only the codes to simplify the use of the table.
  ungroup()
  

# remove the undue zeros and ones and replaces the by NAs

# s_env_ac_pefasu <- 
s_env_ac_pefasu %>% left_join(issue_pefasu %>% 
  group_by(unit_code, nace_r2_code_list, nace_r2_code, siec_code, geo_code, nrg_bal_code) %>% 
  summarise(zeros=sum(!is.na(nrg_pefa)&(nrg_pefa==0)), unos=sum(!is.na(nrg_pefa)&(nrg_pefa==1)), nonna=sum(!is.na(nrg_pefa))) %>% 
  filter(values>0) %>% 
  ungroup() %>% 
  select(unit_code, nace_r2_code_list, siec_code, geo_code, nrg_bal_code) %>% 
  distinct() %>% 
    mutate(remove_value=TRUE)) %>% 
  mutate(values=ifelse(!is.na(values)&is.na(remove_value), values, NA)) %>% 
  View()

pefa_sector_shares <- s_env_ac_pefasu %>% 
  group_by(siec_code, unit_code, geo_code, time, nace_r2_code, nace_r2_code_list, nrg_bal_code) %>% 
  summarise(values=ifelse(sum(!is.na(values))>0, sum(values, na.rm=TRUE), NA)) %>% 
  left_join(s_env_ac_pefasu %>%                  # reopen the pefa table to calculate the total energy consumption (by energy product) by groups of activities as reported in nrg_bal_c 
              group_by(siec_code, unit_code, geo_code, time, nace_r2_code, nace_r2_code_list, nrg_bal_code) %>% 
              summarise(values_pefasu=ifelse(sum(!is.na(values))>0, sum(values, na.rm=TRUE), NA)) %>%
              ungroup() %>%  
              group_by(siec_code, unit_code, geo_code, time, nace_r2_code_list, nrg_bal_code) %>% 
              summarise(values_pefasu=sum(values_pefasu, na.rm=FALSE))
              ) %>%
  mutate(nrg_pefa=ifelse(values_pefasu>0, values/values_pefasu, NA)) %>% # Compute the shares
  select(time, nace_r2_code, siec_code, geo_code, nrg_bal_code, values, values_pefasu, nrg_pefa) %>% #   keeps only the codes to simplify the use of the table.
  ungroup() %>% 
  fillValueTime(., "nrg_pefa", "time", c("nace_r2_code", "siec_code", "geo_code")) %>%  # when there is no information for a specific year, fill with the closest value in time.
  mutate(nrg_pefa=value_est) %>% 
  select(-c("year_p", "value_p", "year_n", "value_n", "value_est")) %>% 
  select(time, nace_r2_code, siec_code, geo_code, values, values_pefasu, nrg_pefa) %>% 
  left_join(ren_siec) %>% 
  left_join(cor_nrg_bal_accounts_ext %>% select(-nrg_bal))

pefa_sector_shares <- pefa_sector_shares %>% 
  full_join(nrg_bal_c %>%                      # expands the dataset to make sure that every entry in the nrg_bal_c table has a corresponding factor 
              inner_join(siec_pefa_nrg %>% select(siec_code, s_prod_nrg_code) %>% distinct()) %>% 
              left_join(ren_siec) %>%
              inner_join(cor_nrg_bal_accounts_ext) %>% 
              filter(geo_code %in% EU28Countries$geo_code,  time %in% (pefa_sector_shares$time %>% unique)) %>% 
              select(nrg_bal_code, siec_code, s_prod_nrg_code, ren, unit_code, geo_code, time, nace_r2_code_list, nace_r2_accounts, nace_r2_code))


pefa_sector_shares2 <- pefa_sector_shares %>% left_join(
  pefa_sector_shares %>% 
    group_by(nace_r2_code, geo_code, siec_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts, ren) %>% 
    summarise(values_nace_all_time= sum(values, na.rm = TRUE)/sum(!is.na(values))) %>% 
    ungroup() %>% 
    left_join(  pefa_sector_shares %>% 
                  group_by(nace_r2_code, geo_code, siec_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts, ren) %>% 
                  summarise(values_nace_all_time= sum(values, na.rm = TRUE)/sum(!is.na(values))) %>% 
                  ungroup() %>% 
                  group_by(geo_code, siec_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts, ren) %>% 
                  summarise(values_nrg_all_time= sum(values_nace_all_time, na.rm = TRUE))) %>% 
    mutate(shares_ovtime=values_nace_all_time/values_nrg_all_time)
  ) %>% 
  mutate(nrg_pefa=ifelse(is.na(nrg_pefa) & !is.na(shares_ovtime) & !is.nan(shares_ovtime), shares_ovtime, nrg_pefa)) %>%
  mutate(nrg_pefa = ifelse(nace_r2_code==nace_r2_accounts & is.na(nrg_pefa), 1, nrg_pefa)) %>%  # sometimes the calculation of the share returns NA because the quantity is null. For sectors defined the same way in the energy balance and the energy accounts, we force the value 1
  select(-shares_ovtime, -values_nace_all_time, -values_nrg_all_time)  # %>% names() %>% paste(collapse = ", ")



pefa_sector_shares2 <- pefa_sector_shares2 %>% left_join(
  pefa_sector_shares2 %>% 
    group_by(time, nace_r2_code, geo_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts, ren) %>% 
    summarise(values=sum(values, na.rm = TRUE)) %>% 
    left_join(pefa_sector_shares2 %>% 
                group_by(time, geo_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts, ren) %>% 
                summarise(values_pefasu=sum(values, na.rm = TRUE))) %>% 
    mutate(shares_ren_nren=ifelse(values_pefasu>0, values/values_pefasu, NA)) %>% 
    select(-c(values, values_pefasu)) %>% 
    ungroup() %>% 
    fillValueTime(., "shares_ren_nren", "time", c("nace_r2_code", "geo_code", "nrg_bal_code", "ren")) %>%  # when there is no information for a specific year, fill with the closest value in time.
    mutate(shares_ren_nren=value_est) %>% 
    select(time, nace_r2_code, geo_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts, ren, shares_ren_nren)
) %>% 
  mutate(nrg_pefa=ifelse(is.na(nrg_pefa), shares_ren_nren, nrg_pefa)) %>%
  mutate(nrg_pefa = ifelse(nace_r2_code==nace_r2_accounts & is.na(nrg_pefa), 1, nrg_pefa)) %>%  # sometimes the calculation of the share returns NA because the quantity is null. For sectors defined the same way in the energy balance and the energy accounts, we force the value 1
  select(-c("shares_ren_nren"))  # %>% names() %>% paste(collapse = ", ")
#  group_by(time, prod_nrg_code, geo_code, nrg_bal_code, stk_flow_code, unit_code, stk_flow, prod_nrg, unit, geo, nrg_bal, nace_r2_code_list, siec_code, siec, ren) %>% # check that the total is 1 for all categories
#  summarise(shares=sum(nrg_pefa)-1) %>% filter(round(shares, 15)!=0) # except for the calculation rounding.


pefa_sector_shares2 <- pefa_sector_shares2 %>% left_join(
  pefa_sector_shares2 %>% 
    group_by(time, nace_r2_code, geo_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts) %>% 
    summarise(values=sum(values, na.rm = TRUE), values_pefasu=sum(values_pefasu, na.rm = TRUE)) %>% 
    mutate(shares_tot=ifelse(values_pefasu>0, values/values_pefasu, NA)) %>% 
    select(-c(values, values_pefasu)) %>% 
    fillValueTime(., "shares_tot", "time", c("nace_r2_code", "geo_code", "nrg_bal_code")) %>%  # when there is no information for a specific year, fill with the closest value in time.
    mutate(shares_tot=value_est) %>% 
    select(time, nace_r2_code, geo_code, nrg_bal_code, nace_r2_code_list, nace_r2_accounts, shares_tot)
) %>% 
  mutate(nrg_pefa=ifelse(is.na(nrg_pefa), shares_tot, nrg_pefa)) %>%
  select(-c("shares_tot"))  

  

# Check that energy consumption is 100% allocated 
pefa_sector_shares2 %>% 
  left_join(pefa_sector_shares2 %>% 
              group_by(geo_code, nace_r2_code_list , time, nrg_bal_code, siec_code) %>%
              summarise(notNull=round(sum(nrg_pefa, na.rm=FALSE)))) %>%
  filter(notNull!=1) %>% 
  View()
  
  
# ---- Coefficients to go from the activities considered in the accounts down to the NACE level 2 based on energy purchase reported in SBS ----
# The results are at NACE level 2 and sometimes NACE level 4.

tmp_shares_sbs <- 
  sbs_na_ind_r2 %>% 
  filter(indic_sb_code=="V20110", geo_code %in% datam$geo_code) %>%
  inner_join(cor_nace %>% 
               select(nace_r2_code, nace_r2_accounts) %>% 
             full_join(BBshare_avg %>% 
                         select(NACE)%>% 
                         unique() %>% 
                         mutate(nace_r2_code=gsub('b', "", NACE)) %>% 
                         rbind(tibble(NACE=c("bC10", "bC11", "bC12"), nace_r2_code=c("C10", "C11", "C12"))) # C10-C12 (food, beverage and tobaco) are not in BBshare_avg but are part of the bioeconomy
                       ))


# here we consider non existent any sector for which there is no information in SBS.

tmp_shares_sbs <- tmp_shares_sbs %>%
  left_join(
    sbs_na_ind_r2 %>%
      select(nace_r2_code, indic_sb_code, geo_code, time, values) %>%
      filter(
        geo_code %in% datam$geo_code,
        nace_r2_code %in% cor_nace$nace_r2_code
      ) %>%
      group_by(nace_r2_code, geo_code) %>%
      summarise(no_info = sum(!is.na(values)) < 1) %>%
      filter(no_info) %>% ungroup()
  ) %>%
  mutate(values = ifelse(!is.na(no_info) & no_info == TRUE, 0, values)) %>%
  select(-no_info)
  

tmp_shares_sbs_no_info <- tmp_shares_sbs %>%
  select(nace_r2_code, indic_sb_code, geo_code, time, values) %>%
  group_by(nace_r2_code, indic_sb_code, geo_code) %>%
  summarise(no_info=sum(!is.na(values))<1) %>% 
  filter(no_info) %>% ungroup()

# tmp_shares_sbs_no_info %>% 
#   select(-indic_sb_code) %>% 
#   inner_join(sbs_na_ind_r2 %>% filter(indic_sb_code=="V11110")) %>% # check that there is no information for the sectors without any energy data in the time series.
#   View()

# Fix the problem in Malta for the sector C15 having one only number in 2008 where data for other sectors is not available and Malta sector 12 not existing.


# For the countries where V20110 is missing for all years, use the number of enterprises in the sector (if available) multiplied by the EU average Purchases of energy products per enterprise in the sector 
tmp_shares_sbs <- tmp_shares_sbs %>% 
  mutate(values=ifelse(geo_code=="MT" & nace_r2_code=="C13", 
                              sbs_na_ind_r2[sbs_na_ind_r2$geo_code=="MT" & sbs_na_ind_r2$indic_sb_code=="V20110" & sbs_na_ind_r2$nace_r2_code=="C13" & sbs_na_ind_r2$time==2018, ]$values, 
                              values)) %>% 
  mutate(values=ifelse(geo_code=="MT" & nace_r2_code=="C12", 0, 
                       values)) %>% 
  left_join(sbs_na_ind_r2 %>%
  filter(indic_sb_code %in% c("V11110")) %>% 
  select(-indic_sb_code, -indic_sb) %>%
  rename(V11100=values) %>% 
  inner_join(tmp_shares_sbs_no_info) %>%
  inner_join(
    sbs_na_ind_r2 %>%
      filter(
        indic_sb_code %in% c("V11110", "V20110"),
        geo_code %in% datam$geo_code,
        nace_r2_code %in% tmp_shares_sbs_no_info$nace_r2_code
      ) %>%
      select(-indic_sb) %>%
      pivot_wider(values_from = values, names_from = indic_sb_code) %>%
      filter(!is.na(V20110 + V11110), V11110 > 0) %>%
      group_by(time, nace_r2_code) %>%
      summarise(energy_enterprise = sum(V20110) / sum(V11110)))) %>% # ungroup() %>% group_by(nace_r2_code) %>% summarise(mean(energy_enterprise))
  mutate(V11100=ifelse(geo_code=="MT" & nace_r2_code=="C15" & time==2009, 
                       sbs_na_ind_r2[sbs_na_ind_r2$geo_code=="MT" & sbs_na_ind_r2$indic_sb_code=="V11110" & sbs_na_ind_r2$nace_r2_code=="C15" & sbs_na_ind_r2$time==2008, ]$values, V11100)) %>% 
  mutate(values=ifelse(!is.na(values), values, ifelse(!is.na(energy_enterprise), energy_enterprise*V11100, NA))) %>% 
  select(-V11100, -no_info, -energy_enterprise)


# tmp_shares_sbs <- tmp_shares_sbs %>%
#   group_by(geo_code, indic_sb_code, nace_r2_accounts, time) %>%
#   summarise(total = sum(values, na.rm = FALSE)) %>%
#   inner_join(sbs_na_ind_r2 %>%
#                inner_join(cor_nrg_bal_nace_ext %>%
#                    select(nace_r2_code, nace_r2_accounts)
#                )) %>%
#   mutate(share_nace_r2_accounts = values / total) %>%
#   ungroup() %>%
#   select(geo_code, nace_r2_accounts, nace_r2_code, time, share_nace_r2_accounts) %>%
#   group_by(geo_code, nace_r2_accounts, nace_r2_code)
  #  summarise(mean=mean(share_nace_r2_accounts, na.rm = TRUE), var=var(share_nace_r2_accounts, na.rm = TRUE)) %>% ungroup() %>% 
#  select(geo_code, nace_r2_accounts, nace_r2_code, mean, var) %>% 
#    filter(is.na(var)) %>% 
#  inner_join(sbs_na_ind_r2) %>% 

tmp_shares_sbs <- tmp_shares_sbs %>% 
  inner_join(tmp_shares_sbs%>%
  group_by(geo_code, indic_sb_code, nace_r2_accounts, time) %>%
  summarise(total = sum(values, na.rm = FALSE))
  ) %>%
  mutate(share_nace_r2_accounts = values / total) %>%
  ungroup() %>%
  select(geo_code, nace_r2_accounts, nace_r2_code, time, share_nace_r2_accounts) %>%
  group_by(geo_code, nace_r2_accounts, nace_r2_code)


tmp_shares_sbs <- tmp_shares_sbs %>% 
  select(-nace_r2_accounts) %>% 
  fillValueTime(., "share_nace_r2_accounts", "time", c("nace_r2_code", "geo_code")) %>% # when there is no information for a specific year, fill with the closest value in time.)
  select(geo_code, nace_r2_code, time, value_est) %>% 
  mutate(share_nace_r2_accounts=ifelse(!is.na(value_est), value_est, 0)) %>% 
  select(-value_est)

# check the exhaustiveness of the shares
tmp_shares_sbs %>%
  left_join(cor_nace) %>% 
  group_by(geo_code, time, nace_r2_accounts) %>%
  summarise(value=sum(share_nace_r2_accounts, na.rm=FALSE)) %>%
  filter(round(value, 6)!=1, !is.na(nace_r2_accounts)) %>%
  View()

# issues in sector C21 n LU (should be 1, but no data because only one company)
tmp_shares_sbs <- tmp_shares_sbs %>% mutate(share_nace_r2_accounts=ifelse(nace_r2_code %in% c("C21"), 1, share_nace_r2_accounts))

shares_sbs <- tmp_shares_sbs 
rm(tmp_shares_sbs)

# sbs does not have energy values for 2020 yet. We copy the shares of 2019.
shares_sbs <- shares_sbs %>% bind_rows(shares_sbs %>% filter(time==2019) %>% mutate(time=2020))


# ---- Calculation of direct energy use by NACE level 2 activities using nrg_bal_c, pefa_sector_shares2 and shares_sbs ----
## -- Coefficients to open the activities reported in nrg_bal, using the shares from pefa --

database <- nrg_bal_c %>%
  filter(nrg_bal_code %in% cor_nrg_bal_nace$nrg_bal_code, siec_code %in% siec_pefa_nrg$siec_code, geo_code %in% EU28Countries$geo_code,  time %in% (pefa_sector_shares2$time %>% unique))

database <- database %>% left_join(ren_siec %>% select(-ren))

# add the uses of energy in the energy sectors that provides the types of energy directly consumed by the sectors
database <- database %>% left_join(database %>% 
  left_join(nrg_e_to %>% 
              filter(!is.na(values_cons_pTJoutput) & values_cons_pTJoutput!=0) %>% 
              select(siec_code, unit_code, geo_code, time, siec_cons_code, values_cons_pTJoutput)) %>% 
    filter(!is.na(siec_cons_code)) %>% 
    mutate(nrg_to_consumption = values * values_cons_pTJoutput) %>% 
    select(nrg_bal_code, unit_code, geo_code, time, siec_cons_code, nrg_to_consumption) %>% 
    group_by(nrg_bal_code, unit_code, geo_code, time, siec_cons_code) %>% 
    summarise(nrg_to_consumption=sum(nrg_to_consumption, na.rm=TRUE)) %>% 
    rename(siec_code=siec_cons_code)) %>% 
  mutate(nrg_to_consumption=ifelse(is.na(nrg_to_consumption), 0, nrg_to_consumption))

# add the energy losses
database <- database %>% left_join(nrg_ratio %>% select(siec_code, unit_code, geo_code, time, FC_E2FC_EpDL))
  
# add the split by activities reported in the energy accounts
database <- database %>% left_join(pefa_sector_shares2 %>% 
            select(nrg_bal_code, siec_code, geo_code, time, nace_r2_code, nrg_pefa, ren) %>%
            unique())

# add the split by activities reported in BBIs and calculates the BBI energy use by energy type
database <- database %>% 
  rename(nace_r2_accounts=nace_r2_code) %>% 
  left_join(cor_nace) %>% 
  left_join(shares_sbs) %>%
  mutate(share_nace_r2_accounts = ifelse(nace_r2_code==nace_r2_accounts, 1, share_nace_r2_accounts)) %>% 
  left_join(BBshare %>% select(geo_code, time, nace_r2_code, BBI_energy_share)) %>% 
  filter(!nace_r2_code %in% c("C18", "C32"))

# Quantity of energy product (siec_code) used by the activity (nace_r2_code)  = (reported value + estimated consumption in the sector)
#                                                                                   * (use+loss)/use ratio 
#                                                                                   * split of NRG statistics activities by NRG accounts activities
#                                                                                   * split of NRG accounts activities by SBS activities (NACE)
#                                                                                   * share of bio-based activities in the SBS activities
database <- database %>% mutate(bbi_ener_use = (values + nrg_to_consumption) * FC_E2FC_EpDL * nrg_pefa * share_nace_r2_accounts * BBI_energy_share)


# add the shares of renewable in electricity and heat
database2 <- database %>% left_join(nrg_ind_ren %>% 
    mutate(siec_code=ifelse(nrg_bal_code=="REN_ELC", "E7000", ifelse(nrg_bal_code=="REN_HEAT_CL", "H8000", NA)), share=values/100) %>% 
    select(geo_code, time, siec_code, share) %>% rename(share_ren=share)) %>% 
  mutate(share_ren=ifelse(is.na(share_ren), ifelse(ren=="Y", 1, 0), share_ren)) %>% 
  mutate(values_ren = bbi_ener_use * share_ren, values_bio = bbi_ener_use * bioenergy) 

database3 <- database2 %>% 
  # select only the main variables to create a wide table and calculate the shares
  group_by(geo_code, geo, unit_code, time, nace_r2_code) %>% # , nace_r2_bio_code
  summarise(total=sum(bbi_ener_use, na.rm = TRUE), renewables=sum(values_ren, na.rm=TRUE), bio_quantity=sum(values_bio, na.rm=TRUE)) %>% 
  mutate(share_ren = renewables / total, share_bio = bio_quantity / total)

database2 %>%
  filter(!geo_code%in%c("UK")) %>% 
  group_by(unit_code, time, nace_r2_code) %>% 
  summarise(total=sum(bbi_ener_use, na.rm = TRUE), renewables=sum(values_ren, na.rm=TRUE)) %>% 
  mutate(share = renewables / total) %>% 
  group_by(nace_r2_code) %>% 
  ggplot(aes(as.factor(time), share, group=nace_r2_code, colour=nace_r2_code)) +
  geom_line() +
  geom_point()

# database2 %>%
#  filter(!geo_code%in%c("UK")) %>% 
#  group_by(unit_code, time, nace_r2_code, nace_r2_bio_code) %>% 
#  summarise(total=sum(bbi_ener_use, na.rm = TRUE), renewables=sum(values_ren, na.rm=TRUE)) %>% 
#  mutate(share = renewables / total) %>% left_join(bioeco_sectors) %>% write.csv("clipboard-2048")
  
database2 %>%
  filter(!geo_code%in%c("UK")) %>% 
  group_by(geo_code, unit_code, time) %>% 
  summarise(total=sum(bbi_ener_use, na.rm = TRUE), renewables=sum(values_ren, na.rm=TRUE)) %>% 
  mutate(share = renewables / total) %>% 
  group_by(geo_code) %>% 
  ggplot(aes(as.factor(time), share, group=geo_code, colour=geo_code)) +
  geom_line() +
  geom_point()

database2 %>% 
  filter(!geo_code%in%c("UK"), substr(nace_r2_code, 1, 1) %in% "C") %>% 
  group_by(geo_code, unit_code, time) %>% 
  summarise(total=sum(bbi_ener_use, na.rm = TRUE), renewables=sum(values_ren, na.rm=TRUE)) %>% 
  mutate(share = renewables / total) %>% write.csv("clipboard-2048")


# pdf('test.pdf', paper="a4", onefile=TRUE)
# dev.off()

for (i in unique(database2$geo_code)){ 
  database2 %>%
    filter(geo_code==i) %>% 
    group_by(geo_code, nace_r2_bio_code, unit_code, time) %>% 
    summarise(total=sum(bio_ener_use, na.rm = TRUE), renewables=sum(values_ren, na.rm=TRUE)) %>% 
    mutate(share = renewables / total) %>% 
    group_by(geo_code, nace_r2_bio_code) %>% 
    ggplot(aes(as.factor(time), share, group=nace_r2_bio_code, colour=nace_r2_bio_code)) +
      geom_line() +
      geom_point() +
    facet_wrap(vars(nace_r2_bio_code), scales="free")
    ggsave(paste(".//plots//", i, ".pdf", collapse=""), device="pdf", width = 29.7, height = 21, units = "cm")
}




# ---- final calculation with all coefficients ----
  
  # Calculate the share of renewables including the part from renewable heat and power.
for (i in unique(nrg_bal_c_temp$geo_code)){
    nrg_bal_c %>% 
    filter(geo_code %in% i) %>% 
    # Extract data on sectors including a bioeconomy component, as defined in the table "cor_nrg_bal_nace"
    right_join(cor_nrg_bal_nace) %>% 
    # and select only the total energy, energy from renewable as well as energy as electricity or heat:  siec_code in TOTAL, RA000, E7000, H8000
    inner_join(ren_siec) %>% 
    # multiplied by the share of renewable used in the production of electricity and heat
    # Extracted from NRG_IND_REN, with nrg_bal in REN_ELEC and REN_HEAT_CL
    left_join(
      nrg_ind_ren %>% 
        mutate(siec_code=ifelse(nrg_bal_code=="REN_ELC", "E7000", ifelse(nrg_bal_code=="REN_HEAT_CL", "H8000", NA)), share=values/100) %>% 
        select(geo_code, time, siec_code, share) %>% 
        filter(!is.na(siec_code))
    ) %>%
    filter(time %in% unique(nrg_ind_ren$time)) %>% 
    # use 100% for non electricity or heat
    mutate(share=ifelse(is.na(share), ren, share)) %>% 
    # compute the new value with the percentages
    mutate(values_ren = values * share) %>% 
    # select only the main variables to create a wide table and calculate the shares
    select(nrg_bal_code, unit_code, geo_code, time, nace_r2_code_list, siec_code, values, values_ren) %>% 
    group_by(nrg_bal_code, unit_code, geo_code, time, nace_r2_code_list) %>% 
    summarise(total=sum(values, na.rm=TRUE), renewables=sum(values_ren, na.rm=TRUE)) %>% 
    mutate(share = renewables / total) %>% 
    
    #  left_join(nrg_bal_c %>% filter(siec_code=="TOTAL")) %>% mutate(diff=values-total) %>% filter(diff>0) # check the total
    
    filter(time>2004, !nrg_bal_code %in% c("FC_OTH_AF_E", "FC_OTH_FISH_E")) %>% 
    # graph
    group_by(nrg_bal_code) %>% 
    ggplot(aes(as.factor(time), share, group=nrg_bal_code, colour=nrg_bal_code)) +
    geom_line() +
    geom_point()
    ggsave(paste(".//plots//nrg//nrg_", i, ".pdf", collapse=""), device="pdf", width = 29.7, height = 21, units = "cm")
}

nrg_bal_c %>% 
  filter(geo_code %in% EU28) %>% 
  # Extract data on sectors including a bioeconomy component, as defined in the table "cor_nrg_bal_nace"
  full_join(cor_nrg_bal_nace) %>% 
  filter(!is.na(nace_r2_code_list)|nrg_bal_code %in% "FC_IND_E") %>% 
  # and select only the total energy, energy from renewable as well as energy as electricity or heat:  siec_code in TOTAL, RA000, E7000, H8000
  inner_join(ren_siec) %>% 
  # multiplied by the share of renewable used in the production of electricity and heat
  # Extracted from NRG_IND_REN, with nrg_bal in REN_ELEC and REN_HEAT_CL
  left_join(
    nrg_ind_ren %>% 
      mutate(siec_code=ifelse(nrg_bal_code=="REN_ELC", "E7000", ifelse(nrg_bal_code=="REN_HEAT_CL", "H8000", NA)), share=values/100) %>% 
      select(geo_code, time, siec_code, share) %>% 
      filter(!is.na(siec_code))
  ) %>%
  filter(time %in% unique(nrg_ind_ren$time)) %>% 
  # use 100% for non electricity or heat
  mutate(share=ifelse(is.na(share), ren, share)) %>% 
  # compute the new value with the percentages
  mutate(values_ren = values * share) %>% 
  # select only the main variables to create a wide table and calculate the shares
  select(nrg_bal_code, unit_code, geo_code, time, nace_r2_code_list, siec_code, values, values_ren) %>% 
  group_by(nrg_bal_code, unit_code, geo_code, time, nace_r2_code_list) %>% 
  summarise(total=sum(values, na.rm=TRUE), renewables=sum(values_ren, na.rm=TRUE)) %>% 
  mutate(share = renewables / total) %>% 
  
  #  left_join(nrg_bal_c %>% filter(siec_code=="TOTAL")) %>% mutate(diff=values-total) %>% filter(diff>0) # check the total
  
  filter(time>2004, !nrg_bal_code %in% c("FC_OTH_AF_E", "FC_OTH_FISH_E")) %>% 
  # graph
  group_by(nrg_bal_code) %>% 
  ggplot(aes(as.factor(time), share, group=nrg_bal_code, colour=nrg_bal_code)) +
  geom_line() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+ 
  facet_wrap(vars(geo_code), scales="free")


# ---- analysis of the missing data ----

nrg_bal_c_temp <- nrg_bal_c %>%
  filter(geo_code %in% unique(database2$geo_code), time>2007, time<2020) %>% 
  inner_join(ren_siec) %>% 
  mutate(ren=ifelse(siec_code %in% c("E7000", "H8000"), 0.5, ren)) %>% 
  inner_join(cor_nrg_bal_nace)

for (i in unique(nrg_bal_c_temp$geo_code)){ 
    nrg_bal_c_temp %>% 
      filter(geo_code==i) %>% 
      group_by(geo_code, nace_r2_code_list, unit_code, time) %>% 
      summarise(total=sum(values, na.rm = TRUE), renewables=sum(values*ren, na.rm=TRUE)) %>% 
      mutate(share = renewables / total) %>% 
      group_by(geo_code, nace_r2_code_list) %>% 
      ggplot(aes(as.factor(time), share, group=nace_r2_code_list, colour=nace_r2_code_list)) +
        geom_line() +
        geom_point() +
        facet_wrap(vars(nace_r2_code_list), scales="free")
      ggsave(paste(".//plots//nrg_bal_c//", i, ".pdf", collapse=""), device="pdf", width = 29.7, height = 21, units = "cm")
}



# Coherence and exhaustiveness checks
sbs_V20_out_range <- sbs_na_ind_r2 %>% 
  filter(indic_sb_code=="V20110", geo_code %in% datam$geo_code) %>%
  inner_join(cor_nace %>% 
               select(nace_r2_code, nace_r2_accounts) %>% 
               full_join(BBshare_avg %>% 
                           select(NACE)%>% 
                           unique() %>% 
                           mutate(nace_r2_code=gsub('b', "", NACE)) %>% 
                           rbind(tibble(NACE=c("bC10", "bC11", "bC12"), nace_r2_code=c("C10", "C11", "C12"))))) %>% 
  filter(nchar(nace_r2_code)==5) %>% 
  mutate(nace_r2_code=substr(nace_r2_code, 1,3)) %>% 
  group_by(nace_r2_code, indic_sb_code, geo_code, indic_sb, geo, time) %>% 
  summarise(value_l2=sum(values, na.rm=TRUE)) %>% 
  left_join(sbs_na_ind_r2 %>% 
              filter(indic_sb_code=="V20110", geo_code %in% datam$geo_code)) %>% 
  filter(abs(value_l2-values)>0.2)
  
sbs_V20_out_range %>% 
  select(nace_r2_code, geo_code, time, value_l2, values) %>% 
  rename(nace_group=nace_r2_code, valuesT=values) %>% 
  left_join(sbs_na_ind_r2 %>% filter(nchar(nace_r2_code)==5) %>% 
              mutate(nace_group=substr(nace_r2_code, 1,3))) %>% 
  select(-nace_r2) %>% 
  pivot_wider(names_from = nace_r2_code, values_from = values) %>% 
  write.csv("clipboard-2048")
  
  
  
  group_by(nace_group, geo_code, time, value_l2, valuesT) %>% 
  summarise(vNA=sum(is.na(values))) %>% View()
