library(knitr)
opts_chunk$set(comment = NA, prompt = FALSE, cache = FALSE, echo = TRUE,
               results = "asis")
library(haven)
library(rmdformats)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(survminer)
library(wesanderson)
library(dplyr)
library(janitor)
library(survival)
library(fastDummies)
library(car) #VIF
library(tableone)
library(ggpubr)
library(gtsummary)
library(readr)
library(lubridate)
library(zoo)
library(labelled)
library(survivalAnalysis)

setwd("/gpfs/home/fatman01/Rcodes")

start_time <- Sys.time()

# 1. Import data sets from SRTR

setwd("/gpfs/data/massielab/data/srtr/srtr2501/saf/sas")


don_deceased <- read_sas("donor_deceased.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()

don_disposition <- read_sas("donor_disposition.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


cand_thor <- read_sas("cand_thor.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()

tx_hr <- read_sas( "tx_hr.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


stathist_thor <- 
  read_sas("stathist_thor.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


stat_just_hr1a <- read_sas( "statjust_hr1a.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


stat_just_hr1b <- read_sas( "statjust_hr1b.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


setwd("/gpfs/data/massielab/data/srtr/srtr2410/thor_reg/sas")
just_form_hr <- read_sas("JustFormHR.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


just_form_hr_data_link <- 
  read_sas("JustFormHRDataLink.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


just_form_hr_stat1 <- 
  read_sas("JustFormHRStat1.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


just_form_hr_stat2 <- 
  read_sas("JustFormHRStat2.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()

just_form_hr_stat3 <- 
  read_sas("JustFormHRStat3.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


just_form_hr_stat4 <- 
  read_sas("JustFormHRStat4.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


risk_strat_data_hr <- 
  read_sas("RiskStratDataHR.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


status_just_episode <- 
  read_sas("StatusJustEpisode.sas7bdat", NULL) %>%
  zap_formats() %>% zap_labels()


thor_support_device <- 
  read_sas("ThorSupportDevice.sas7bdat", NULL) %>% 
  zap_formats() %>% zap_labels()


column_descriptions <- 
  read_sas("ColumnDescriptions.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


table_descriptions <- 
  read_sas( "TableDescriptions.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


setwd("/gpfs/home/fatman01/Acceptancedataset")

save(cand_thor, column_descriptions, just_form_hr, just_form_hr_data_link,
     just_form_hr_stat1, just_form_hr_stat2, just_form_hr_stat3,
     just_form_hr_stat4, risk_strat_data_hr, stat_just_hr1a, stat_just_hr1b,
     stathist_thor, status_just_episode, table_descriptions, 
     thor_support_device, tx_hr, file =  "raw_data_candidate.RData")


# 2. Link datasets using JustID, px_id, and WlregAuditId

risk_strat_data_hr <- risk_strat_data_hr %>% 
  mutate(ChangeDt = floor_date(ChangeDate, unit = "day")) %>% 
  select(-ChangeDate) %>%  
  full_join(just_form_hr_data_link %>%
              select(-ChangeDate, -InitialFormJustId), by = "WlregAuditId") %>% 
  mutate(PX_ID = px_id) %>%  
  select(-px_id) %>% 
  
  # removes entries that have valid WlregAuditId but missing JustId
  filter(!is.na(JustId)) %>%
  
  # there are instances in which two just. forms were submitted on the same day for a
  # single PX_ID. The time step in this process is one day, so these are removed
  group_by(PX_ID, ChangeDt) %>% slice_max(JustId) %>% 
  ungroup() %>% 
  
  select(-RiskStratId, -WlregAuditId, -ChangeUserId, -ChangeRequestorId,
         -HemoDataObtained, -HemoDataObtainedOther, -HemoPcwpOrLvedp) %>% # removes several columns, likely because they are not needed for further analysis
  select(-(ends_with("St"))) %>% 
  select(-(starts_with("CandHist"))) %>%
  select(-(starts_with("SenData"))) %>%
  select(-(ends_with("Perf"))) %>%
  relocate(ChangeDt:PX_ID) 


# right_join to filter out erroneous forms, as above
just_form_hr_data_link <- just_form_hr_data_link %>%
  select(-ChangeDate) %>% # Removes the ChangeDate column from the just_form_hr_data_link dataset
  right_join(risk_strat_data_hr %>% 
               select(ChangeDt, JustId, PX_ID), by = "JustId")


# right_join to filter out erroneous forms, as above, refines the just_form_hr dataset and uses a right join to merge it with the just_form_hr_data_link dataset
just_form_hr <- just_form_hr %>% 
  select(JustId, RequestedCandStatCd, RequestedCandStat_descrip,
         ExtensionNumber, Exception, ThorSupportDevicePrimary,
         ThorSupportDeviceSecondary, descrip, AdmittedToHospital, FormStatus_descrip, ApplicationStatus_descrip) %>%
  mutate(listing_description = descrip) %>% 
  select(-descrip) %>% 
  right_join(just_form_hr_data_link %>% 
               select(-WlregAuditId, -InitialFormJustId), by = "JustId")



just_form_hr <- just_form_hr %>% # columns ThorSupportDevicePrimary and ThorSupportDeviceSecondary are no longer required for analysis
  select(-ThorSupportDevicePrimary, -ThorSupportDeviceSecondary)

# enrich the just_form_hr_stat1 dataset with the PX_ID and ChangeDt information from the just_form_hr_data_link dataset.
just_form_hr_stat1 <- just_form_hr_stat1 %>% 
  select(-ChangeDate) %>% 
  left_join(just_form_hr_data_link %>% 
              select(JustId, PX_ID, ChangeDt), by = "JustId") 

just_form_hr_stat2 <- just_form_hr_stat2 %>% 
  select(-ChangeDate) %>%
  left_join(just_form_hr_data_link %>% 
              select(JustId, PX_ID, ChangeDt), by = "JustId")

just_form_hr_stat3 <- just_form_hr_stat3 %>% 
  select(-ChangeDate) %>%
  left_join(just_form_hr_data_link %>%
              select(JustId, PX_ID, ChangeDt), by = "JustId")

just_form_hr_stat4 <- just_form_hr_stat4 %>% 
  select(-ChangeDate) %>%
  left_join(just_form_hr_data_link %>% 
              select(JustId, PX_ID, ChangeDt), by = "JustId")


# 3. Tidy input datasets
#     a. Filter out candidates listed before and after filter dates
#     b. Filter out pediatric transplant candidates
#     c. Filter out lung-only transplant candidates


filter_date_start <- mdy("10-18-2018")
filter_date_end <- mdy("12-12-2024")
include_multi_organ_recipients <- TRUE


include_pre_policy <- FALSE
missingness_threshold <- 1


# filter candidates based on inclusion/exclusion criteria 
cand_list <- cand_thor %>% 
  
  # Remove recipients listed prior to policy change
  filter(CAN_LISTING_DT >= filter_date_start) %>%
  
  # Remove recipients transplanted after
  filter(REC_TX_DT <= filter_date_end) %>% 
  
  # Remove heart-lung and lung-only recipients
  filter(WL_ORG != "HL") %>%   
  filter(WL_ORG != "LU") %>% 
  
  # Remove recipients who were < 18 yrs old at listing
  filter((CAN_AGE_IN_MONTHS_AT_LISTING / 12) >= 18)



# Executes the subsequent block only if include_multi_organ_recipients is set to FALSE.

if (include_multi_organ_recipients == FALSE) {
  multi_organ_recipients <- tx_hr %>% 
    filter(REC_TX_TY == 2 | REC_TX_TY == 4) %>% 
    select(PX_ID, REC_TX_TY) # Creates a dataset of multi-organ recipients (multi_organ_recipients), keeping only their patient IDs (PX_ID) and transplant type (REC_TX_TY)
  
  cand_list <- cand_list %>% 
    filter(CAN_INIT_STAT != 2150) %>% # Excludes candidates with Status 5
    filter(!(PX_ID %in% multi_organ_recipients$PX_ID)) # Filters out any candidates whose patient IDs (PX_ID) appear in the multi_organ_recipients dataset.
}


cand_list <- cand_list %>% relocate(CAN_LISTING_DT, .after = REC_TX_DT)


cand_list$CAN_LISTING_DT <- as.Date(cand_list$CAN_LISTING_DT)
cand_list$REC_TX_DT <- as.Date(cand_list$REC_TX_DT)

# Calculate time difference (in days)
cand_list$Time_Diff <- difftime(cand_list$REC_TX_DT, cand_list$CAN_LISTING_DT, units = "days")

cand_list <- cand_list %>% relocate(Time_Diff, .after = REC_TX_DT)

# Each dataset (e.g., tx_hr, risk_strat_data_hr, stathist_thor, etc.) is being filtered to retain only the rows that correspond to the patient IDs (PX_ID) in the cand_list

tx_list <- tx_hr %>% filter(PX_ID %in% cand_list$PX_ID)

risk_strat <- risk_strat_data_hr %>%
  filter(PX_ID %in% cand_list$PX_ID)

# filter status history file
stathist <- stathist_thor %>% filter(PX_ID %in% cand_list$PX_ID)

# filter common justification form file
just_form <- just_form_hr %>% filter(PX_ID %in% cand_list$PX_ID)

# filter support device file
#support_device <- thor_support_device %>% filter(PX_ID %in% cand_list$PX_ID)

# filter data linking dataframe
data_link <- just_form_hr_data_link %>% filter(PX_ID %in% cand_list$PX_ID)

# filter Status 1A justification forms
just_stat1a <- stat_just_hr1a %>% filter(PX_ID %in% cand_list$PX_ID)

# filter Status 1B justification forms
just_stat1b <- stat_just_hr1b %>% filter(PX_ID %in% cand_list$PX_ID)

# filter Status 1 justification forms
just_stat1 <- just_form_hr_stat1 %>% filter(PX_ID %in% cand_list$PX_ID)

# filter Status 2 (new policy) justification forms
just_stat2 <- just_form_hr_stat2 %>% filter(PX_ID %in% cand_list$PX_ID)

# filter Status 3 justification forms
just_stat3 <- just_form_hr_stat3 %>% filter(PX_ID %in% cand_list$PX_ID)

# filter Status 4 justification forms
just_stat4 <- just_form_hr_stat4 %>% filter(PX_ID %in% cand_list$PX_ID)


# Removes several large variables that are no longer needed in memory after the filtering is complete.
rm(cand_thor, tx_hr, risk_strat_data_hr, stathist_thor, just_form_hr,
   stat_just_hr1a, stat_just_hr1b, just_form_hr_stat1, just_form_hr_stat2,
   just_form_hr_stat3, just_form_hr_stat4, thor_support_device,
   status_just_episode, just_form_hr_data_link)

# Explicitly calls garbage collection to release unused memory and optimize performance.
gc()


# 4. Pivot cand_list and stathist by date, then full_join to create full_list
# Selects a subset of variables from cand_list to keep only those relevant for analysis.


cand_list <- cand_list %>% 
  select(
    PX_ID, WL_ORG, CAN_GENDER, CAN_ABO, CAN_RACE, CAN_ETHNICITY_SRTR, CAN_LISTING_DT,
    CAN_LISTING_CTR_CD, CAN_LISTING_CTR_TY, CAN_PRELIM_XMATCH_REQUEST, CAN_REM_DT, CAN_LAST_ACT_STAT_DT, CAN_LAST_INACT_STAT_DT,
    CAN_REM_CD, CAN_DGN, CAN_MAX_MILE, CAN_MIN_AGE, CAN_MAX_AGE, CAN_MIN_HGT, CAN_MIN_WGT, CAN_MAX_HGT, CAN_MAX_WGT, CAN_ACPT_GENDER, CAN_ACPT_DCD, 
    CAN_ACPT_HIST_CAD, CAN_ACPT_HIST_CIGARETTE, CAN_ECMO, CAN_VENTILATOR, CAN_IABP, CAN_IV_INOTROP, CAN_ICU, CAN_BALLOON, CAN_ACPT_HCV_POS, 
    CAN_VAD_TY, CAN_VAD1, CAN_PRIMARY_PAY, CAN_HGT_CM, CAN_WGT_KG, CAN_BMI, CAN_ACPT_HBC_POS, CAN_EDUCATION, CAN_LIFE_SUPPORT, CAN_INHALED_NO, CAN_LIFE_SUPPORT_OTHER, CAN_WORK_INCOME,
    CAN_DIAB_TY, CAN_DIAL, CAN_CEREB_VASC, CAN_MOST_RECENT_CREAT,
    CAN_TOT_ALBUMIN, CAN_IMPLANT_DEFIB, CAN_PULM_ART_MEAN, CAN_PCW_MEAN, CAN_DRUG_TREAT_COPD,
    CAN_PULM_ART_SYST, CAN_PULM_ART_DIAST, CAN_PULM_ART_SYST_MEDS, CAN_PULM_ART_DIAST_MEDS, CAN_PULM_ART_MEAN_MEDS, CAN_PCW_MEAN_MEDS, 
    CAN_CARDIAC_OUTPUT_MEDS, CAN_CARDIAC_SURG, CAN_RACE_WHITE,
    CAN_CARDIAC_OUTPUT, CAN_HIST_CIGARETTE, CAN_MALIG, CAN_LAST_STAT,
    CAN_AGE_IN_MONTHS_AT_LISTING, CAN_INIT_STAT, CAN_ANGINA, CAN_ANGINA_CAD, 
    CAN_PEPTIC_ULCER, CAN_EXERCISE_O2, CAN_DRUG_TREAT_HYPERTEN, CAN_PERIPH_VASC,
    CAN_ANTI_ARRYTHM, CAN_OTHER_TOBACCO_USE,CAN_STAT_EXTEND_FLG, CAN_PULM_EMBOL, CAN_DEATH_DT,
    PERS_SSA_DEATH_DT, PERS_OPTN_DEATH_DT )


cand_list_missing_data_check <- tibble(
  pct_missing = vector(mode = "double", length = ncol(cand_list)),
  var_names = colnames(cand_list))

for (i in 1:ncol(cand_list)) {
  
  cand_list_missing_data_check$pct_missing[i] <-
    sum(is.na(cand_list[, i])) / nrow(cand_list)
}


cand_list_missing_data_check <- cand_list_missing_data_check %>% 
  filter(pct_missing >= missingness_threshold) %>% 
  arrange(desc(pct_missing))


cand_list <- cand_list %>% select(-cand_list_missing_data_check$var_names)

cand_list <- cand_list %>%
  distinct(.keep_all = TRUE) %>% 
  pivot_longer(cols = ends_with("DT"),
               names_to = "event", 
               values_to = "date", 
               values_drop_na = TRUE) %>%
  arrange(date) %>% 
  group_by(date) %>% 
  mutate(unique_date = date) %>%
  group_by(PX_ID, date) %>% 
  mutate(unique_event = event) %>% 
  pivot_wider(names_from = event, values_from = date) %>% 
  arrange(PX_ID, unique_date) %>% 
  ungroup() %>%
  select(PX_ID, unique_event, unique_date, 
         WL_ORG:CAN_PULM_EMBOL)

stathist <- stathist %>% 
  select(PX_ID, CANHX_BEGIN_DT, CANHX_STAT_CD, 
         CANHX_REASON_STAT_INACT) %>% 
  pivot_longer(cols = ends_with("DT"),
               names_to = "event", 
               values_to = "date", 
               values_drop_na = TRUE) %>%
  arrange(date) %>% 
  group_by(date) %>% 
  mutate(unique_date = date) %>%
  group_by(PX_ID, date) %>% 
  mutate(unique_event = event) %>% 
  pivot_wider(names_from = event, values_from = date) %>%
  arrange(PX_ID, unique_date) %>% 
  ungroup() %>%
  select(PX_ID, unique_event, unique_date, CANHX_STAT_CD, 
         CANHX_REASON_STAT_INACT)

full_list <- cand_list %>% 
  # full join by common variables, thus no need for suffix
  full_join(stathist, by = c("PX_ID", "unique_date", "unique_event")) %>% 
  relocate(CANHX_STAT_CD:CANHX_REASON_STAT_INACT, .after = unique_date) %>% 
  arrange(PX_ID, unique_date) %>% 
  ungroup()


full_list$unique_date <- as.Date(full_list$unique_date, format = "%m-%d-%y")

min_date <- min(full_list$unique_date, na.rm = TRUE)
max_date <- max(full_list$unique_date, na.rm = TRUE)

print(paste("Minimum Date:", min_date))
print(paste("Maximum Date:", max_date))

# 5. Pivot tx_list by date, then join with full_list

tx_list <- tx_list %>% 
  distinct(.keep_all = TRUE) %>% 
  select(PX_ID, ORG_TY, REC_TX_DT, REC_POSTX_LOS, REC_A_MM_EQUIV_TX,
         REC_B_MM_EQUIV_TX, REC_DR_MM_EQUIV_TX, REC_DGN, REC_CTR_CD,
         REC_CTR_TY, REC_DISCHRG_DT, REC_ADMISSION_DT, REC_MED_COND, 
         REC_FUNCTN_STAT, REC_PRIMARY_PAY, REC_HGT_CM, REC_WGT_KG,
         REC_BMI, REC_HIV_STAT, REC_CMV_STAT, REC_HCV_STAT,
         REC_EBV_STAT, REC_LIFE_SUPPORT, REC_ECMO,
         REC_IABP, REC_PGE, REC_INOTROP, REC_VENTILATOR, REC_VAD1,
         REC_VAD2, REC_CREAT, REC_TOT_BILI, REC_CHRONIC_STEROIDS,
         REC_TXFUS, REC_INFECT_IV_DRUG, REC_DIAL, 
         REC_VENTILATOR_SUPPORT, REC_HR_ISCH, REC_POSTX_STROKE,
         REC_POSTX_DIAL, REC_POSTX_PACEMAKER,
         REC_ACUTE_REJ_EPISODE, REC_AGE_IN_MONTHS_AT_TX,
         REC_A1, REC_A2, REC_B1, REC_B2, REC_DR1, REC_DR2,
         TFL_LASTATUS, TFL_GRAFT_DT, TFL_DEATH_DT,
         PERS_RETX, TFL_LAFUDATE, REC_MM_EQUIV_TX)

tx_list_missing_data_check <- tibble(
  pct_missing = vector(mode = "double", length = ncol(tx_list)),
  var_names = colnames(tx_list))

for (i in 1:ncol(tx_list)) {
  
  tx_list_missing_data_check$pct_missing[i] <-
    sum(is.na(tx_list[, i])) / nrow(tx_list)
}

tx_list_missing_data_check <- tx_list_missing_data_check %>% 
  filter(pct_missing >= missingness_threshold) %>% 
  arrange(desc(pct_missing))

tx_list <- tx_list %>% select(-tx_list_missing_data_check$var_names)

tx_list <- tx_list %>% 
  mutate(TFL_LAST_FU_DT = TFL_LAFUDATE) %>%
  select(-TFL_LAFUDATE) %>% 
  pivot_longer(cols = ends_with("DT"),
               names_to = "event", 
               values_to = "date", 
               values_drop_na = TRUE) %>%
  arrange(date) %>% 
  group_by(date) %>% 
  mutate(unique_date = date) %>%
  group_by(PX_ID, date) %>% 
  mutate(unique_event = event) %>% 
  pivot_wider(names_from = event, values_from = date) %>% 
  arrange(PX_ID, unique_date) %>% 
  ungroup() %>% 
  select(PX_ID, unique_event, unique_date, ORG_TY:REC_MM_EQUIV_TX)

full_list <- full_list %>% 

  full_join(tx_list, by = c("PX_ID", "unique_date", "unique_event")) %>% 
  arrange(PX_ID, unique_date) %>% 
  ungroup()



# 6. Join just_form and just_stat1-just_stat4 with risk_strat, 
#    pivot by date, then join with full_list

# Removes duplicate rows in just_stat1 based on all columns except JustId, retaining all other columns
# Converts all columns ending in Dt (assumed to contain date-time values) to only the date component by flooring the values to the day

just_stat1 <- just_stat1 %>% 
  distinct(across(-JustId), .keep_all = TRUE) %>% 
  mutate(across(ends_with("Dt"), ~ floor_date(.x, unit = "day")))

just_stat2 <- just_stat2 %>% 
  distinct(across(-JustId), .keep_all = TRUE) %>% 
  mutate(across(ends_with("Dt"), ~ floor_date(.x, unit = "day")))

just_stat3 <- just_stat3 %>% 
  distinct(across(-JustId), .keep_all = TRUE) %>% 
  mutate(across(ends_with("Dt"), ~ floor_date(.x, unit = "day")))

just_stat4 <- just_stat4 %>% 
  distinct(across(-JustId), .keep_all = TRUE) %>% 
  mutate(across(ends_with("Dt"), ~ floor_date(.x, unit = "day")))


risk_strat <- risk_strat %>%
  mutate(across(ends_with("Dt"), ~ floor_date(.x, unit = "day"))) %>%
  
  mutate(HrSevFailNtBnpType = ifelse(HrSevFailNtBnpType == "", NA, HrSevFailNtBnpType)) %>% 
  
  # adding just_form and just_stat1-4 columns to risk_strat
  full_join(just_form, by = c("ChangeDt", "JustId", "PX_ID")) %>%
  full_join(just_stat1, by = c("JustId", "PX_ID", "ChangeDt")) %>%
  full_join(just_stat2, by = c("JustId", "PX_ID", "ChangeDt"),
            suffix = c(".stat1", ".stat2")) %>%
  full_join(just_stat3, by = c("JustId", "PX_ID", "ChangeDt"),
            suffix = c(".stat2", ".stat3")) %>% 
  full_join(just_stat4, by = c("JustId", "PX_ID", "ChangeDt"),
            suffix = c(".stat3", ".stat4")) %>%
  
  pivot_longer(cols = contains("Dt", ignore.case = FALSE),
               names_to = "event", 
               values_to = "date", 
               values_drop_na = TRUE) %>% 
  
  group_by(date) %>% 
  mutate(unique_date = date) %>%
  group_by(PX_ID, date) %>% 
  mutate(unique_event = event) %>% 
  ungroup() %>% 
  pivot_wider(names_from = event, values_from = date) %>%   
  
  # risk_strat variables
  mutate(across(starts_with("CPTest"), ~ case_when(!is.na(CPTestDt) ~ .))) %>% 
  mutate(across(starts_with("Hemo"), ~ case_when(!is.na(HemoDt) ~ .))) %>%
  mutate(
    VadLdhLevels = case_when(!is.na(VadLdhLevelsDt) ~ VadLdhLevels),
    VadPlasmaFreeHemo = case_when(!is.na(VadPlasmaFreeHemoDt) ~ VadPlasmaFreeHemo),
    HrSevFailSodium = case_when(!is.na(HrSevFailSodiumDt) ~ HrSevFailSodium),
    HrSevFailCreatinine = case_when(!is.na(HrSevFailCreatinineDt) ~ HrSevFailCreatinine),
    HrSevFailBun = case_when(!is.na(HrSevFailBunDt) ~ HrSevFailBun),
    HrSevFailAlbumin = case_when(!is.na(HrSevFailAlbuminDt) ~ HrSevFailAlbumin),
    HrSevFailAsparTrans = case_when(!is.na(HrSevFailAsparTransDt) ~ HrSevFailAsparTrans),
    HrSevFailBilirubin = case_when(!is.na(HrSevFailBilirubinDt) ~ HrSevFailBilirubin),
    HrSevFailArterialLact = case_when(!is.na(HrSevFailArterialLactDt) ~
                                        HrSevFailArterialLact),
    HrSevFailInr = case_when(!is.na(HrSevFailInrDt) ~ HrSevFailInr),
    HrSevFailInrAntiCoag = case_when(!is.na(HrSevFailInrDt) ~ HrSevFailInrAntiCoag),
    HrSevFailBnp = case_when(!is.na(HrSevFailBnpDt) ~ HrSevFailBnp)) %>%
  
  mutate(RequestedCandStatCd = case_when(!is.na(ChangeDt) ~
                                           RequestedCandStatCd)) %>%
  
  
  
  # just_stat1 variables
  mutate(
    EcmoSystolicBloodPressure = case_when(!is.na(EcmoSystolicBloodPressureDt) ~
                                            EcmoSystolicBloodPressure),
    EcmoCardiacIndex = case_when(!is.na(EcmoCardiacIndexDt) ~
                                   EcmoCardiacIndex),
    EcmoCapWedgePressure = case_when(!is.na(EcmoCapWedgePressureDt) ~
                                       EcmoCapWedgePressure),
    EcmoWithoutHemoSbp = case_when(!is.na(EcmoWithoutHemoSbpDt) ~
                                     EcmoWithoutHemoSbp),
    EcmoWithoutHemoArtLac = case_when(!is.na(EcmoWithoutHemoArtLacDt) ~
                                        EcmoWithoutHemoArtLac),
    EcmoWithoutHemoAst = case_when(!is.na(EcmoWithoutHemoAstDt) ~
                                     EcmoWithoutHemoAst),
    EcmoWithoutHemoAlt = case_when(!is.na(EcmoWithoutHemoAltDt) ~
                                     EcmoWithoutHemoAlt),
    ExtMeanPressure.stat1 = case_when(!is.na(ExtMeanPressureDt.stat1) ~
                                        ExtMeanPressure.stat1),
    ExtCardiacIndex.stat1 = case_when(!is.na(ExtCardiacIndexDt.stat1) ~
                                        ExtCardiacIndex.stat1),
    ExtCapWedgePressure.stat1 = case_when(!is.na(ExtCapWedgePressureDt.stat1) ~
                                            ExtCapWedgePressure.stat1),
    ExtSvo2.stat1 = case_when(!is.na(ExtSvo2Dt.stat1) ~
                                ExtSvo2.stat1)) %>% 
  
  # just_stat2 variables
  mutate(
    McsdCardiacIndex = case_when(!is.na(McsdCardiacIndexDt) ~ McsdCardiacIndex),
    McsdCapWedgePressure = case_when(!is.na(McsdCapWedgePressureDt) ~
                                       McsdCapWedgePressure),
    McsdSystolicBloodPressure = case_when(!is.na(McsdSystolicBloodPressureDt) ~
                                            McsdSystolicBloodPressure),
    McsdWithoutHemoSbp = case_when(!is.na(McsdWithoutHemoSbpDt) ~
                                     McsdWithoutHemoSbp),
    McsdWithoutHemoArtLac = case_when(!is.na(McsdWithoutHemoArtLacDt) ~
                                        McsdWithoutHemoArtLac),
    McsdWithoutHemoAst = case_when(!is.na(McsdWithoutHemoAstDt) ~
                                     McsdWithoutHemoAst),
    
    McsdWithoutHemoAlt = case_when('McsdWithoutHemoAltDt' %in% colnames(risk_strat) &&
                                     !is.na(McsdWithoutHemoAltDt) ~
                                     McsdWithoutHemoAlt),
    
    IabpCardiacIndex = case_when(!is.na(IabpCardiacIndexDt) ~
                                   IabpCardiacIndex),
    IabpCapWedgePressure = case_when(!is.na(IabpCapWedgePressureDt) ~
                                       IabpCapWedgePressure),
    IabpSystolicBloodPressure = case_when(!is.na(IabpSystolicBloodPressureDt) ~
                                            IabpSystolicBloodPressure),
    IabpWithoutHemoSbp = case_when(!is.na(IabpWithoutHemoSbpDt) ~
                                     IabpWithoutHemoSbp),
    IabpWithoutHemoArtLac = case_when(!is.na(IabpWithoutHemoArtLacDt) ~
                                        IabpWithoutHemoArtLac),
    IabpWithoutHemoAst = case_when(!is.na(IabpWithoutHemoAstDt) ~
                                     IabpWithoutHemoAst),
    IabpWithoutHemoAlt = case_when(!is.na(IabpWithoutHemoAltDt) ~
                                     IabpWithoutHemoAlt),
    ExtMeanPressure.stat2 = case_when(!is.na(ExtMeanPressureDt.stat2) ~
                                        ExtMeanPressure.stat2),
    ExtCardiacIndex.stat2 = case_when(!is.na(ExtCardiacIndexDt.stat2) ~
                                        ExtCardiacIndex.stat2),
    ExtCapWedgePressure.stat2 = case_when(!is.na(ExtCapWedgePressureDt.stat2) ~
                                            ExtCapWedgePressure.stat2),
    ExtSvo2.stat2 = case_when(!is.na(ExtSvo2Dt.stat2) ~
                                ExtSvo2.stat2)) %>%
  
  # just_stat3 variables
  mutate(
    
    InoCardiacIndex = case_when('InoCardiacIndexDt' %in% colnames(risk_strat) &&
                                  !is.na(InoCardiacIndexDt) ~ InoCardiacIndex),
    InoCapWedgePressure = case_when('InoCapWedgePressureDt' %in% colnames(risk_strat) &&
                                      !is.na(InoCapWedgePressureDt) ~
                                      InoCapWedgePressure),
    InoSysBloodPressure = case_when('InoSysBloodPressureDt' %in% colnames(risk_strat) &&
                                      !is.na(InoSysBloodPressureDt) ~
                                      InoSysBloodPressure),
    McsdWithRhfDobutamine = case_when('McsdWithRhfDobutamineDt' %in% colnames(risk_strat) &&
                                        !is.na(McsdWithRhfDobutamineDt) ~
                                        McsdWithRhfDobutamine),
    McsdWithRhfDopamine = case_when('McsdWithRhfDopamineDt' %in% colnames(risk_strat) &&
                                      !is.na(McsdWithRhfDopamineDt) ~
                                      McsdWithRhfDopamine),
    McsdWithRhfEpinephrine = case_when('McsdWithRhfEpinephrineDt' %in% colnames(risk_strat) &&
                                         !is.na(McsdWithRhfEpinephrineDt) ~
                                         McsdWithRhfEpinephrine),
    McsdWithRhfMilrinone = case_when('McsdWithRhfMilrinoneDt' %in% colnames(risk_strat) &&
                                       !is.na(McsdWithRhfMilrinoneDt) ~
                                       McsdWithRhfMilrinone),
    McsdWithRhfNitricOxide = case_when('McsdWithRhfNitricOxideDt' %in% colnames(risk_strat) &&
                                         !is.na(McsdWithRhfNitricOxideDt) ~
                                         McsdWithRhfNitricOxide),
    McsdWithRhfProstacyclin = case_when('McsdWithRhfProstacyclinDt' %in% colnames(risk_strat) &&
                                          !is.na(McsdWithRhfProstacyclinDt) ~
                                          McsdWithRhfProstacyclin),
    McsdWithRhfPcwp = case_when('McsdWithRhfPcwpDt' %in% colnames(risk_strat) &&
                                  !is.na(McsdWithRhfPcwpDt) ~
                                  McsdWithRhfPcwp),
    McsdWithRhfVenPressure = case_when('McsdWithRhfVenPressureDt' %in% colnames(risk_strat) &&
                                         !is.na(McsdWithRhfVenPressureDt) ~
                                         McsdWithRhfVenPressure)) %>%
  
  # just_stat4 variables
  mutate(
    InotropeCardiacIndex = case_when(!is.na(InotropeCardiacIndexDt) ~
                                       InotropeCardiacIndex),
    InotropePcwp = case_when(!is.na(InotropePcwpDt) ~
                               InotropePcwp)) %>%  
    
  
  mutate(
    systolicBP = coalesce(HemoSbp, EcmoWithoutHemoSbp, McsdWithoutHemoSbp,
                          IabpWithoutHemoSbp, EcmoSystolicBloodPressure,
                          McsdSystolicBloodPressure, IabpSystolicBloodPressure,
                          InoSysBloodPressure),
    PCWP = coalesce(HemoPcwp, McsdWithRhfPcwp, InotropePcwp, 
                    EcmoCapWedgePressure, McsdCapWedgePressure, 
                    IabpCapWedgePressure, InoCapWedgePressure, 
                    ExtCapWedgePressure.stat1, ExtCapWedgePressure.stat2),
    mean_arterial_pressure = coalesce(ExtMeanPressure.stat1, ExtMeanPressure.stat2),
    central_venous_pressure = coalesce(HemoCvp, McsdWithRhfVenPressure),
    SVO2 = coalesce(HemoSvo2, ExtSvo2.stat1, ExtSvo2.stat2),
    cardiac_index = coalesce(HemoCardiacIndex, EcmoCardiacIndex, McsdCardiacIndex,
                             IabpCardiacIndex, InoCardiacIndex, 
                             InotropeCardiacIndex, ExtCardiacIndex.stat1,
                             ExtCardiacIndex.stat2),
    arterial_lactate = coalesce(EcmoWithoutHemoArtLac, McsdWithoutHemoArtLac,
                                IabpWithoutHemoArtLac, HrSevFailArterialLact),
    AST = coalesce(EcmoWithoutHemoAst, McsdWithoutHemoAst, IabpWithoutHemoAst,
                   HrSevFailAsparTrans),
    ALT = coalesce(EcmoWithoutHemoAlt, McsdWithoutHemoAlt, IabpWithoutHemoAlt),
    dobutamine_dose = coalesce(McsdWithRhfDobutamine, InotropeDobutamine),
    milrinone_dose = coalesce(McsdWithRhfMilrinone, InotropeMilrinone),
    dopamine_dose = coalesce(McsdWithRhfDopamine, InotropeDopamine),
    epinephrine_dose = coalesce(McsdWithRhfEpinephrine, InotropeEpinephrine)) %>% 
  
  arrange(PX_ID, unique_date)



# indicator variables from just_stat1-just_stat4
just_form_indicator_variables <- c(
  
  # just_stat1 variables
  "CriteriaEcmoSupport", "EcmoWithHemo",
  "CardiacIndexInotropeSupport", "EcmoWithoutHemo", "ExtDemoContra.stat1",
  "CriteriaBivadSupport", "CriteriaMcsdSupport", "VentricularEpisode",
  "BivadPlacement",
  
  # just_stat2 variables
  "CriteriaLvadSupport.stat2", "CriteriaDurableDevSupport",
  "CriteriaMcsdMalfunction", "CriteriaMcsdEndovasSupp", "McsdWithHemo",
  "McsdCardiacIndexInotropeSup", "McsdWithoutHemo",
  "CriteriaIabpSupport.stat2", "IabpWithHemo",
  "IabpCardiacIndexInotropeSup", "IabpWithoutHemo", "CriteriaVentEpisode",
  "ExtDemoContra.stat2",
  
  # just_stat3 variables
  "CriteriaLvadDiscSupport", "CriteriaInotropeSupport.stat3",
  "InoInvasiveCatheter", "InoHemodynamicMonitoring", "InoSingle", "InoMultiple",
  "InoSinDobutamine", "InoSinMilrinone", "InoSinEpinephrine", "InoMulDobutamine",
  "InoMulMilrinone", "InoMulEpinephrine", "InoMulDopamine", "InoInotropeSupport",
  "CriteriaMcsdWithHemo", "McsdLactateDehydrogenase", 
  "McsdPlasmaFreeHemoglobin", "McsdHemoglobinuria", "CriteriaMcsdWithPump",
  "McsdWithPumpThrombosis", "McsdWithPumpTransIscAttack", 
  "CriteriaMcsdWithRhf",
  "McsdWithRhfNitricOxide", "McsdWithRhfProstacyclin",
  "CriteriaMcsdInfection", "McsdInfectionErythema", "McsdInfectionDebridement",
  "McsdInfectionBacAnti", "McsdInfectionBacReccur", "McsdInfectionPosCulture",
  "CriteriaMcsdMucosalBleed", "MucosalBleedNumHosp", "CriteriaMcsdWithAI",
  "CriteriaVaEcmoSupport", "CriteriaLvadSupport.stat3", "CriteriaPercuSupport",
  "CriteriaIabpSupport.stat3", "ExtInoSingleDose", "ExtInoMultiDose",
  "ExtInoSinDobutamine", "ExtInoSinMilrinone", "ExtInoSinEpinephrine",
  "ExtInoMulDobutamine", "ExtInoMulMilrinone", "ExtInoMulEpinephrine",
  "ExtInoMulDopamine", "ExtInvasiveCatheter", "ExtHemodynamicMonitoring",
  "ExtCardiacIndx", "ExtFailedWeaningAttempt", "ExtFWACardiacIndex",
  "ExtFWASerumCreatinine", "ExtFWAArterialLactate", "ExtFWASvo2",
  "ExtMcsdDobutamine", "ExtMcsdDopamine", "ExtMcsdEpinephrine",
  "ExtMcsdMilrinone", "ExtMcsdNitricOxide", "ExtMcsdProstacyclin",
  
  # just_stat4 variables
  "CriteriaLvadSupport", "CriteriaInotropeSupport.stat4",
  "CriteriaHeartDisease", "HeartDiseaseValues", "HeartDiseaseOther",
  "CriteriaIschemicHeart", "CriteriaCardiomyopathy", "CardioHypertrophic",
  "CardioAmyloidosis", "CardioRestrict", "CardiomyopathyCanadian",
  "CardiomyopathyNyha", "CardiomyopathyNyhaCdx", "CardiomyopathyNyhaPcwp",
  "CardiomyopathyVenTachy", "CardiomyopathyVenFib", "CardiomyopathyVenArr",
  "CardiomyopathySuddenDeath", "CriteriaRetransplant", "ExtInotropeDobutamine",
  "ExtInotropeMilrinone", "ExtInotropeEpinephrine", "ExtInotropeDopamine")



risk_strat <- risk_strat %>%   
  select(PX_ID, JustId, unique_date, unique_event,
         
         # aggregated variables
         systolicBP, PCWP, mean_arterial_pressure, central_venous_pressure, 
         SVO2, cardiac_index, arterial_lactate, AST, ALT, dobutamine_dose,
         milrinone_dose, dopamine_dose, epinephrine_dose,
         
         
         # risk_strat variables
         CPTestMvo2, CPTestRer, CPTestVeVco2, HemoObtainedOnSupport, HemoDbp,
         HemoRestingHeartRate, HemoPasp, HemoPadp, HemoMeanPressure, HemoLvedp,
         HemoCardiacOutput, HemoHemoglobin, VadLdhLevels, VadPlasmaFreeHemo,
         VadHemoglobinuria, HrSevFailSodium, HrSevFailCreatinine,
         HrSevFailBun, HrSevFailAlbumin, HrSevFailBilirubin, HrSevFailInr,
         HrSevFailInrAntiCoag, HrSevFailBnp, HrSevFailNtBnpType, RequestedCandStatCd,
         ExtensionNumber, Exception, AdmittedToHospital, listing_description, FormStatus_descrip, ApplicationStatus_descrip,
         
         
         # indicator variables from just_stat1-just_stat4
         any_of(just_form_indicator_variables))


risk_strat <- risk_strat %>%
  
  pivot_longer(cols = -c(PX_ID, JustId, unique_date, unique_event,
                         dobutamine_dose, milrinone_dose, dopamine_dose,
                         epinephrine_dose, HemoObtainedOnSupport, 
                         VadHemoglobinuria, HrSevFailInrAntiCoag,
                         HrSevFailNtBnpType,
                         ExtensionNumber, Exception, AdmittedToHospital, listing_description, FormStatus_descrip, ApplicationStatus_descrip,
                         #FormStatus, FormStatus_descrip, ApplicationStatus, ApplicationStatus_descrip, StatusType, DeviceRecallException,
                         any_of(just_form_indicator_variables)),
               names_to = "variable", 
               values_to = "value", 
               values_drop_na = TRUE) %>% 
  relocate(variable, value, .after = unique_event) %>% 
  
  distinct(across(c(PX_ID, unique_date, variable, value)), .keep_all = TRUE) %>% 
  arrange(PX_ID, JustId, unique_date)

duplicated_JustId <- risk_strat %>% 
  group_by(PX_ID, unique_date, variable) %>% 
  filter(n() > 1) %>% 
  group_by(PX_ID, unique_date, variable) %>% 
  select(JustId) %>% 
  ungroup()


earlier_JustId <- risk_strat %>% 
  group_by(PX_ID, unique_date, variable) %>% 
  filter(n() > 1) %>% 
  group_by(PX_ID, unique_date, variable) %>% 
  slice_min(JustId) %>% 
  select(JustId) %>% 
  ungroup()



later_JustId <- duplicated_JustId %>% anti_join(earlier_JustId)


later_ChangeDt <- risk_strat %>% 
  select(PX_ID, unique_date, variable, JustId) %>% 
  filter(grepl("RequestedCandStatCd", variable)) %>% 
  semi_join(later_JustId, by = c("PX_ID", "JustId"))



merged_JustId <- later_JustId %>% 
  left_join(later_ChangeDt %>% select(-variable), 
            by = c("PX_ID", "JustId"), 
            suffix = c("", ".replaced"))

risk_strat <- risk_strat %>% 
  left_join(merged_JustId, 
            by = c("PX_ID", "JustId", "variable", "unique_date")) %>% 
  mutate(unique_date = case_when(
    !is.na(unique_date.replaced) ~ unique_date.replaced,
    is.na(unique_date.replaced) ~ unique_date)) %>% 
  
  mutate(date_changed = case_when(
    !is.na(unique_date.replaced) ~ variable)) %>%
  
  select(-unique_date.replaced) %>% 
  distinct(across(c(PX_ID, unique_date, variable)), .keep_all = TRUE)

# rm(duplicated_JustId, earlier_JustId, later_JustId, later_ChangeDt, merged_JustId)

risk_strat <- risk_strat %>% 
  pivot_wider(names_from = variable,
              values_from = value) %>% 
  
  relocate(systolicBP, PCWP, mean_arterial_pressure, central_venous_pressure, 
           SVO2, cardiac_index, arterial_lactate, AST, ALT,
           HemoDbp, HemoRestingHeartRate, HemoPasp, HemoPadp, HemoMeanPressure,
           HemoLvedp, HemoCardiacOutput, HemoHemoglobin, CPTestMvo2, CPTestRer,
           CPTestVeVco2, VadLdhLevels, VadPlasmaFreeHemo, HrSevFailSodium,
           HrSevFailCreatinine, HrSevFailBun, HrSevFailAlbumin, 
           HrSevFailBilirubin, HrSevFailInr, HrSevFailBnp, HrSevFailNtBnpType,
           RequestedCandStatCd,  ExtensionNumber, Exception, AdmittedToHospital, listing_description, FormStatus_descrip, ApplicationStatus_descrip,
           .after = unique_event) %>% 
  mutate(RequestedCandStatCd = as.integer(RequestedCandStatCd))



risk_strat <- risk_strat %>%
  
  pivot_longer(cols = starts_with("Criteria", ignore.case = FALSE),
               names_to = "criteria", 
               values_to = "indicator", 
               values_drop_na = FALSE) %>% 
  relocate(criteria, indicator, .after = unique_event) %>% 
  mutate(status_criteria = ifelse(indicator == 1, criteria, NA)) %>%
  filter(is.na(indicator) | indicator == 1) %>% 
  
  pivot_wider(names_from = criteria,
              values_from = indicator) %>% 
  
  relocate(status_criteria, .after = unique_event) %>% 
  arrange(PX_ID, JustId, unique_date) %>% 
  group_by(PX_ID, JustId, unique_event, unique_date) %>% 
  filter(row_number() == n()) %>% 
  ungroup() %>% 
  select(-starts_with("Criteria")) %>% 
  distinct(PX_ID, unique_event, unique_date, .keep_all = TRUE)



full_list <- full_list %>%
  
  full_join(risk_strat, by = c("PX_ID", "unique_date", "unique_event")) %>%  
  relocate(JustId, .after = PX_ID) %>%
  arrange(PX_ID, unique_date) %>%
  ungroup()

# use for new allocation policy


full_list <- full_list %>% 
  mutate(
    creatinine = HrSevFailCreatinine,
    bilirubin = HrSevFailBilirubin,
    albumin = HrSevFailAlbumin,
    sodium = HrSevFailSodium,
    BNP = HrSevFailBnp,
    cardiac_output = HemoCardiacOutput,
    resting_HR = HemoRestingHeartRate,
    diastolicBP = HemoDbp,
    PASP = HemoPasp,
    PADP = HemoPadp,
    MPAP = HemoMeanPressure,
    BUN = HrSevFailBun,
    INR = HrSevFailInr,
    LDH = VadLdhLevels)


vars_to_remove <- c("CANHX_HEMO_SBP", "CANHX_HEMO_PCWP", "CANHX_HEMO_MAP", "CANHX_HEMO_CVP",
                    "CANHX_HEMO_CI", "CANHX_LAB_SGOT", "CANHX_INTRP_DOBU",
                    "CANHX_INTRP_MILRIN", "CANHX_INTRP_DOPA", "CANHX_LAB_SERUM_CREAT",
                    "HrSevFailCreatinine", "CANHX_LAB_BILI", "HrSevFailBilirubin",
                    "HrSevFailBun", "HrSevFailInr", "VadLdhLevels",
                    "CANHX_LAB_ALBUMIN", "HrSevFailAlbumin", "CANHX_LAB_SODIUM",
                    "HrSevFailSodium", "CANHX_LAB_BNP", "HrSevFailBnp", "CANHX_CARD_OUTPUT",
                    "HemoCardiacOutput", "CANHX_HEMO_REST_HR_RATE", "HemoRestingHeartRate",
                    "CANHX_HEMO_DBP", "HemoDbp", "CANHX_HEMO_PASP", "HemoPasp",
                    "CANHX_HEMO_PADP", "HemoPadp", "CANHX_HEMO_MPAP", "HemoMeanPressure")

"CANHX_ADULT_CRITERIA_A" %in% colnames(full_list)

full_list <- full_list %>% 
  
  select(-any_of(vars_to_remove)) %>%
  
  relocate(systolicBP, PCWP, mean_arterial_pressure, central_venous_pressure, 
           SVO2, cardiac_output, cardiac_index, arterial_lactate, AST, ALT,
           creatinine, bilirubin, albumin, sodium, BNP, HrSevFailNtBnpType,
           resting_HR, diastolicBP, 
           PASP, PADP, MPAP, HemoLvedp, HemoHemoglobin, CPTestMvo2, CPTestRer,
           CPTestVeVco2, LDH, VadPlasmaFreeHemo, BUN,
           INR,
           .after = unique_date) %>% 
  
  relocate(dobutamine_dose, milrinone_dose, dopamine_dose, epinephrine_dose,
           date_changed,
           .after = last_col()) %>% 
  
  group_by(PX_ID, unique_date) %>%
  mutate(unique_event = str_c(unique_event, collapse = ",")) %>%
  group_by(PX_ID, unique_date) %>%
  fill(everything(), .direction = "downup") %>%
  distinct(PX_ID, unique_date, unique_event, .keep_all = TRUE) %>%   
  
  # necessary for filling steps below
  arrange(PX_ID, unique_date) %>%
  
  # fill variables "down" to show change over time
  # this fills in missing values using the na.locf() method
  group_by(PX_ID) %>%
  fill(systolicBP:CANHX_STAT_CD, .direction = "down") %>%
  ungroup() %>%
  
  arrange(PX_ID, unique_date) %>%
  
  # fill cand_thor and tx_hr variables "downup" because these variables are static
  group_by(PX_ID) %>%
  fill(WL_ORG:REC_MM_EQUIV_TX, .direction = "downup") %>%
  ungroup() %>%
  
  arrange(PX_ID, unique_date) %>%
  group_by(PX_ID, JustId) %>%
  fill(status_criteria:ExtInotropeDopamine, .direction = "down") %>%
  ungroup() %>%
  
  arrange(PX_ID, unique_date) %>%
  group_by(PX_ID, JustId) %>% #, RowNumber) %>%
  fill(dobutamine_dose:epinephrine_dose, .direction = "downup") %>%
  ungroup() %>%
  
  arrange(PX_ID, unique_date) %>%
  ungroup()


# 8. Method to filter full_list to exclude unwanted timepoints
# inlcudes main dates for listing, transplant, death, last follow-up, and status changes


final_list <- full_list %>% 
  
  filter(
    
    # cand_thor variables
    grepl("CAN_LISTING_DT", unique_event) |
      grepl("PERS_OPTN_DEATH_DT", unique_event) |
      grepl("PERS_SSA_DEATH_DT", unique_event) |
      grepl("CAN_REM_DT", unique_event) |
      
      # stathist_thor variables
      grepl("CANHX_BEGIN_DT", unique_event) |
      
      # tx_hr variables
      grepl("REC_TX_DT", unique_event) |
      grepl("REC_ADMISSION_DT", unique_event) |
      # grepl("REC_DISCHRG_DT", unique_event) |
      # grepl("TFL_GRAFT_DT", unique_event) |
      grepl("TFL_DEATH_DT", unique_event) |
      grepl("TFL_LAST_FU_DT", unique_event) |
      
      # just_form_hr variables
      grepl("ChangeDt", unique_event))

# 9. Calculating number of days between each observation 
#    in full_list and final_list, by PX_ID

final_list <- final_list %>%
  ungroup() %>% 
  mutate(t_start = 0, t_stop = 0) %>% 
  arrange(PX_ID, unique_date)

for (i in 2:nrow(final_list)) {
  if (final_list$PX_ID[[i]] == 
      final_list$PX_ID[[i-1]]) {
    
    final_list$t_stop[[i-1]] = 
      final_list$unique_date[[i]] - 
      final_list$unique_date[[i-1]] +
      final_list$t_start[[i-1]]
    
    final_list$t_start[[i]] = final_list$t_stop[[i-1]]
    
  } else {
    
    final_list$t_stop[[i-1]] = 
      final_list$t_start[[i-1]]
    
    final_list$t_start[[i]] = 0
    
  }
}

final_list <- final_list %>% 
  relocate(t_start, t_stop, .after = unique_date)

rdata_filename <-  paste0("Continuousdataset_November14",  ".RData")

csv_filename <-  paste0("Continuousdataset_November14", ".csv")

save(final_list, file = rdata_filename)
write_csv(final_list, csv_filename)


end_time <- Sys.time()
end_time - start_time


df <- read.csv('Continuousdataset_November14.csv')


df <- df %>%
  filter(WL_ORG != "LU")  # Remove rows where wl_org is "LU"
table(df$WL_ORG)

df_raw <- df

columns_to_check <- c('LDH', 'BUN', 'INR')

# Check if these columns exist in the data frame
missing_columns <- columns_to_check[!columns_to_check %in% colnames(df)]

if (length(missing_columns) == 0) {
  print("All specified columns are present in the data frame.")
} else {
  print(paste("The following columns are missing:", paste(missing_columns, collapse = ", ")))
}

df2 <- subset(df, df$t_start == '0' & as.Date(df$unique_date) >= '2018-10-18' & 
                as.Date(df$unique_date) <= '2024-12-12')

postpolicy_id <- unique(df2$PX_ID)
rm(df2) # Removes the df2 object from memory to free up resources..
df <- df[which(df$PX_ID %in% postpolicy_id), ] # df now contains only rows where PX_ID matches patients listed in the specified time range.

## Select relevant variables
# Stores the names of key clinical variables, laboratory test variables, related to blood flow and cardiac function, related to treatments or interventions

clinical_vars <- c('CAN_AGE_IN_MONTHS_AT_LISTING', 'CAN_DGN',
                   'CAN_DIAB_TY', 'CAN_DIAL','CAN_PERIPH_VASC', 
                   'CAN_GENDER', 'CAN_ABO', 'CAN_RACE', 'CAN_ETHNICITY_SRTR' ,
                   'CAN_LISTING_CTR_CD', 'CAN_LISTING_CTR_TY', 'CAN_PRELIM_XMATCH_REQUEST',
                   'CAN_REM_CD', 'CAN_MAX_MILE', 'CAN_MIN_AGE', 'CAN_MAX_AGE', 'CAN_MIN_HGT', 'CAN_MIN_WGT', 
                   'CAN_MAX_HGT', 'CAN_MAX_WGT', 'CAN_ACPT_GENDER', 'CAN_ACPT_DCD', 'CAN_ACPT_HIST_CAD', 'CAN_ACPT_HIST_CIGARETTE', 
                   
                   'CAN_VENTILATOR', 'CAN_ICU', 'CAN_ACPT_HCV_POS', 
                   'CAN_PRIMARY_PAY', 'CAN_HGT_CM', 'CAN_WGT_KG', 'CAN_BMI', 'CAN_ACPT_HBC_POS', 'CAN_EDUCATION', 
                   'CAN_LIFE_SUPPORT', 'CAN_INHALED_NO', 'CAN_LIFE_SUPPORT_OTHER', 'CAN_WORK_INCOME',
                   
                   'SVO2', 'cardiac_index', 
                   
                   'CAN_CEREB_VASC', 'CAN_IMPLANT_DEFIB', 'CAN_DRUG_TREAT_COPD',
                   'CAN_CARDIAC_SURG', 'CAN_RACE_WHITE', 'CAN_HIST_CIGARETTE', 'CAN_MALIG',  'CAN_INIT_STAT', 'CAN_PEPTIC_ULCER',
                   'CAN_EXERCISE_O2', 'CAN_DRUG_TREAT_HYPERTEN', 'CAN_ANTI_ARRYTHM', 
                   'CAN_OTHER_TOBACCO_USE','CAN_STAT_EXTEND_FLG', 'CAN_PULM_EMBOL'
)

labs_vars <- c('sodium', 'creatinine', 'HrSevFailBun', 'albumin', 'AST', 'LDH', 'BUN', 'INR',
               'arterial_lactate', 'HrSevFailInr', 'bilirubin', 
               'HrSevFailBun', 'BNP', 'VadLdhLevels')

hemodynamic_vars <- c('systolicBP', 'diastolicBP', 'resting_HR',
                      'central_venous_pressure', 'cardiac_output', 
                      'PCWP', 'PASP', 'PADP', 'HemoHemoglobin')

treatment_vars <- c(grep(names(df), 
                         pattern='ExtIno|_dose$|^InoSin|^InoMul|Iabp', value=T),
                    'EcmoWithHemo', 'EcmoWithoutHemo',
                    'ExtMcsdDobutamine', 'ExtMcsdDopamine',
                    'ExtMcsdEpinephrine', 'ExtMcsdMilrinone', 'CANHX_TAH',
                    'BivadPlacement', 'CAN_VAD_TY', 'CAN_VAD1', 'CAN_VAD2')


df <- df[ ,which(colnames(df) %in% c('PX_ID', 'JustId', 'RowNumber',
                                     'unique_event', 'unique_date',
                                     't_start', 't_stop',
                                     'RequestedCandStatCd', 'status_criteria', 'Exception', 
                                     'ExtensionNumber', 'AdmittedToHospital', 'listing_description', 'FormStatus_descrip', 'ApplicationStatus_descrip',
                                     
                                     clinical_vars, labs_vars, 
                                     hemodynamic_vars, treatment_vars))]


colnames(df) <- c('PX_ID', 'JustId', 'events', 'date', 
                  't_start', 't_stop', 'systolicBP', 'PCWP', 
                  'central_venous_pressure',  'SVO2', 'cardiac_output', 'cardiac_index', 'arterial_lactate', 'AST',
                  
                  'creatinine', 'bilirubin', 'albumin', 'sodium', 
                  'BNP', 'heart_rate', 'diastolicBP', 'PASP', 
                  'PADP', 'hemoglobin', 'LDH', 'BUN',
                  
                  'INR', 'CAN_GENDER', 'CAN_ABO', 'CAN_RACE', 'CAN_ETHNICITY_SRTR' ,
                  'CAN_LISTING_CTR_CD', 'CAN_LISTING_CTR_TY', 'CAN_PRELIM_XMATCH_REQUEST',
                  'CAN_REM_CD', 
                  
                  'diagnosis',
                  
                  'CAN_MAX_MILE', 'CAN_MIN_AGE', 'CAN_MAX_AGE', 'CAN_MIN_HGT', 'CAN_MIN_WGT', 
                  'CAN_MAX_HGT', 'CAN_MAX_WGT', 'CAN_ACPT_GENDER', 'CAN_ACPT_DCD', 'CAN_ACPT_HIST_CAD', 'CAN_ACPT_HIST_CIGARETTE', 
                  
                  'CAN_VENTILATOR', 'CAN_ICU', 'CAN_ACPT_HCV_POS', 
                  
                  'VAD', 'VAD_brand', 
                  
                  'CAN_PRIMARY_PAY', 'CAN_HGT_CM', 'CAN_WGT_KG', 'CAN_BMI', 'CAN_ACPT_HBC_POS', 'CAN_EDUCATION', 
                  'CAN_LIFE_SUPPORT', 'CAN_INHALED_NO', 'CAN_LIFE_SUPPORT_OTHER', 'CAN_WORK_INCOME',
                  
                  'diabetes', 'dialysis', 
                  
                  'CAN_CEREB_VASC', 'CAN_IMPLANT_DEFIB', 'CAN_DRUG_TREAT_COPD',
                  'CAN_CARDIAC_SURG', 'CAN_RACE_WHITE', 'CAN_HIST_CIGARETTE', 'CAN_MALIG',  
                  
                  'age_m',
                  
                  'CAN_INIT_STAT', 'CAN_EXERCISE_O2', 'CAN_DRUG_TREAT_HYPERTEN', 
                  
                  'peripheral_vascular',
                  
                  'CAN_ANTI_ARRYTHM', 'CAN_OTHER_TOBACCO_USE','CAN_STAT_EXTEND_FLG', 'CAN_PULM_EMBOL',
                  
                  'status_criteria', 'status',  
                  
                  'ExtensionNumber', 'exception', 'AdmittedToHospital', 'listing_description', 'FormStatus_descrip', 'ApplicationStatus_descrip',
                  
                  'ecmo_hemo',
                  'ecmo_without_hemo', 'bivad', 'iabp_hemo', 'iabp_cardiacindex', 
                  'iabp_without_hemo', 
                  
                  'InoSingle', 'InoMultiple', 'InoSinDobutamine', 
                  'InoSinMilrinone', 'InoSinEpinephrine', 
                  'InoMulDobutamine', 'InoMulMilrinone',
                  
                  'InoMulEpinephrine', 'InoMulDopamine', 'ExtInoSingleDose', 
                  'ExtInoMultiDose', 'ExtInoSinDobutamine', 
                  'ExtInoSinMilrinone', 'ExtInoSinEpinephrine', 
                  'ExtInoMulDobutamine','ExtInoMulMilrinone',
                  'ExtInoMulEpinephrine', 'ExtInoMulDopamine', 
                  'ExtMcsdDobutamine',
                  
                  'ExtMcsdDopamine', 'ExtMcsdEpinephrine', 
                  'ExtMcsdMilrinone', 'ExtInotropeDobutamine', 
                  'ExtInotropeMilrinone', 'ExtInotropeEpinephrine', 
                  'ExtInotropeDopamine', 
                  
                  'dobutamine_dose', 'milrinone_dose', 
                  'dopamine_dose', 'epinephrine_dose')



rownames(df) <- seq(1:nrow(df))


all(c('LDH', 'BUN', 'INR') %in% colnames(df))

table(df$TAH, useNA='a')

### Add transplant indicator based on a REC_TX event
# Adding a New Column (transplant)
# updates the transplant column to 1 for rows where the events column contains 'REC_TX_DT'


# Create transplant column based on "REC_TX_DT"
df$transplant <- 0
df$transplant[which(grepl(pattern = 'REC_TX_DT', x = df$events))] <- 1
table(df$transplant)


df <- df
df$JustId <- trimws(df$JustId)

setwd("/gpfs/data/massielab/data/srtr/srtr2410/thor_reg/sas")

df_just1 <- haven::read_sas('./JustFormHRStat1.sas7bdat')
df_just2 <- haven::read_sas('./JustFormHRStat2.sas7bdat')
df_just3 <- haven::read_sas('./JustFormHRStat3.sas7bdat')
df_just4 <- haven::read_sas('./JustFormHRStat4.sas7bdat')


setwd("/gpfs/home/fatman01/Rcodes")

df_just1 <- df_just1[ ,c('JustId', 'EcmoWithHemo', 'EcmoWithoutHemo',
                         'EcmoCapWedgePressure', 'EcmoCardiacIndex',
                         'EcmoSystolicBloodPressure', 'EcmoWithoutHemoCprDt', 
                         'EcmoWithoutHemoSbp', 'EcmoWithoutHemoArtLac', 
                         'EcmoWithoutHemoAst', 'EcmoWithoutHemoAlt', 
                         'VentricularEpisode', 
                         'CriteriaBivadSupport', 'CriteriaEcmoSupport')]
df_just1$JustId <- trimws(df_just1$JustId)
# Convert all factors to characters to remove factor levels/labels
df_just1[] <- lapply(df_just1, function(x) {
  if (is.factor(x)) {
    return(as.character(x))  # Convert factor to character (removes labels)
  } else {
    return(x)
  }
})
df <- merge(df, df_just1, by = 'JustId', all.x = T, all.y = F, suffixes = c('.x', '.y'), no.dups = T)

## Status 2 justification
df_just2 <- df_just2[ ,c('JustId', 'IabpWithHemo', 'IabpWithoutHemo', 
                         'IabpCapWedgePressure', 'IabpCardiacIndex',
                         'IabpSystolicBloodPressure', 'IabpWithoutHemoCprDt', 
                         'IabpWithoutHemoSbp', 'IabpWithoutHemoArtLac', 
                         'IabpWithoutHemoAst', 'IabpWithoutHemoAlt',
                         'CriteriaMcsdMalfunction', 
                         'CriteriaDurableDevSupport',
                         'CriteriaVentEpisode', 'CriteriaLvadSupport',
                         'CriteriaMcsdEndovasSupp', 
                         'CriteriaIabpSupport')]
df_just2$JustId <- trimws(df_just2$JustId)
#df_just2 <- expss::drop_all_labels(df_just2)
# Remove factor labels by converting factors to characters
df_just2[] <- lapply(df_just2, function(x) {
  if (is.factor(x)) {
    return(as.character(x))  # Convert factor to character (removes labels)
  } else {
    return(x)
  }
})
df_just2$CriteriaLvadSupport2 <- df_just2$CriteriaLvadSupport
df_just2$CriteriaLvadSupport <- NULL
df <- merge(df, df_just2, by = 'JustId', all.x = T, 
            all.y = F, suffixes = c('.x', '.y'), no.dups = T)


## Status 3 justification
df_just3 <- df_just3[ ,c('JustId', grep(names(df_just3), pattern = 'Rhf', value=T),
                         'CriteriaVaEcmoSupport', 'CriteriaLvadSupport', 
                         'CriteriaLvadDiscSupport', 'CriteriaPercuSupport', 
                         'CriteriaMcsdWithPump', 'CriteriaMcsdWithHemo', 
                         'CriteriaMcsdMucosalBleed', 'CriteriaMcsdWithAI', 
                         'CriteriaMcsdInfection', 'CriteriaInotropeSupport', 
                         'CriteriaIabpSupport',
                         
                         'InoInotropeSupport','InoInvasiveCatheter', 
                         'InoHemodynamicMonitoring', 
                         'InoCapWedgePressure', 'InoCardiacIndex', 
                         'InoSysBloodPressure',
                         
                         'ExtInoSinDobutamine','ExtInoSinMilrinone', 
                         'ExtInoSinEpinephrine', 'ExtInoMulDobutamine', 
                         'ExtInoMulMilrinone', 'ExtInoMulEpinephrine', 
                         'ExtInoMulDopamine',
                         
                         'InoSinDobutamine', 'InoSinMilrinone', 'InoSinEpinephrine', 
                         'InoMulDobutamine', 'InoMulMilrinone', 
                         'InoMulEpinephrine', 'InoMulDopamine',
                         'ExtMcsdDobutamine', 'ExtMcsdDopamine', 
                         'ExtMcsdEpinephrine', 'ExtMcsdMilrinone')]
df_just3$JustId <- trimws(df_just3$JustId)
#df_just3 <- expss::drop_all_labels(df_just3)
df_just3[] <- lapply(df_just3, function(x) {
  if (is.factor(x)) {
    return(as.character(x))  # Convert factor to character (removes labels)
  } else {
    return(x)
  }
})
df_just3$CriteriaIabpSupport3 <- df_just3$CriteriaIabpSupport
df_just3$CriteriaIabpSupport <- NULL
df_just3$CriteriaLvadSupport3 <- df_just3$CriteriaLvadSupport
df_just3$CriteriaLvadSupport <- NULL
df <- merge(df, df_just3, by = 'JustId', all.x = T, all.y = F, 
            suffixes = c('.x', '.y'), no.dups = T)


## Status 4 justification
df_just4 <- df_just4[ ,c('JustId', 'InotropeDobutamine', 'InotropeDopamine',
                         'InotropeEpinephrine', 'InotropeMilrinone', 'CriteriaLvadSupport',
                         'CriteriaIschemicHeart', 'CriteriaInotropeSupport', 
                         'InotropeCardiacIndex', 'InotropePcwp',
                         'ExtInotropeDobutamine', 'ExtInotropeMilrinone', 
                         'ExtInotropeEpinephrine', 'ExtInotropeDopamine')]
df_just4$JustId <- trimws(df_just4$JustId)
df_just4[] <- lapply(df_just4, function(x) {
  if (is.factor(x)) {
    return(as.character(x))  # Convert factor to character (removes labels)
  } else {
    return(x)
  }
})

df_just4$CriteriaInotropeSupport4 <- df_just4$CriteriaInotropeSupport 
df_just4$CriteriaInotropeSupport <- NULL
df_just4$CriteriaLvadSupport4 <- df_just4$CriteriaLvadSupport
df_just4$CriteriaLvadSupport <- NULL
df <- merge(df, df_just4, by = 'JustId', all.x = T, all.y = F, 
            suffixes = c('.x', '.y'), no.dups = T)


## Inotrope variables
#These lab variables are categorical, with cutoffs based on the 'High' cutoffs in the data dictionary. Lab variables are also 'High' if indicator variables for those variables are fulfilled
#Categories are 'Not High' and 'High'. None includes 0 or no data. The cutoffs for 'High' (inclusive) when numeric: \newline

# * 3 for dopamine
# * 7.5 for dobutamine
# * 0.5 for milrinone
# * 0.02 for epinephrine

df <- df %>%
  mutate(dopamine = case_when(
    
    as.numeric(dopamine_dose) >= 3 | as.numeric(McsdWithRhfDopamine) >= 3 | 
      as.numeric(InotropeDopamine) >= 3 | as.numeric(ExtMcsdDopamine.y) >= 3 | 
      as.numeric(ExtInotropeDopamine.y) >= 3 | 
      ExtInoMulDopamine.y == 1 | InoMulDopamine.y == 1 ~ 'High',
    
    TRUE ~ 'Not High')) 

df %>%
  filter(dopamine == 'High') %>%
  select(PX_ID, dopamine, dopamine_dose, McsdWithRhfDopamine, InotropeDopamine, 
         ExtMcsdDopamine.x, ExtMcsdDopamine.y, 
         ExtInotropeDopamine.x, ExtInotropeDopamine.y,
         ExtInoMulDopamine.x, ExtInoMulDopamine.y,
         InoMulDopamine.x, InoMulDopamine.y)

### Dobutamine
# creates a new variable called dobutamine, and the values are categorized as 'High' or 'Not High' based on the conditions specified for different related variables. It checks for values greater than or equal to 7.5 for certain columns, and if those conditions are met, the dobutamine variable is labeled as 'High'. 
# Otherwise, it defaults to 'Not High'


df <- df %>% 
  mutate(dobutamine = case_when(
    
    as.numeric(dobutamine_dose) >= 7.5 | as.numeric(McsdWithRhfDobutamine) >= 7.5 | 
      as.numeric(InotropeDobutamine) >= 7.5 | as.numeric(ExtMcsdDobutamine.y) >= 7.5 | 
      as.numeric(ExtInotropeDobutamine.y) >= 7.5 | 
      ExtInoSinDobutamine.y == 1 | InoSinDobutamine.y == 1 ~ 'High',
    
    TRUE ~ 'Not High'))

df %>%
  filter(dobutamine == 'High') %>%
  select(PX_ID, dobutamine, dobutamine_dose, McsdWithRhfDobutamine, 
         InotropeDobutamine, 
         ExtMcsdDobutamine.x, ExtMcsdDobutamine.y, 
         ExtInotropeDobutamine.x, ExtInotropeDobutamine.y,
         ExtInoMulDobutamine.x, ExtInoMulDobutamine.y,
         ExtInoSinDobutamine.x, ExtInoSinDobutamine.y,
         InoSinDobutamine.x, InoSinDobutamine.y,
         InoMulDobutamine.x, InoMulDobutamine.y)

### Milrinone
df <- df %>% 
  mutate(milrinone = case_when(
    
    as.numeric(milrinone_dose) >= 0.5 | as.numeric(McsdWithRhfMilrinone) >= 0.5 | 
      as.numeric(InotropeMilrinone) >= 0.5 | as.numeric(ExtMcsdMilrinone.y) >= 0.5 | 
      as.numeric(ExtInotropeMilrinone.y) >= 0.5 | 
      ExtInoSinMilrinone.y == 1 | InoSinMilrinone.y == 1  ~ 'High',
    
    TRUE ~ 'Not High'))

df %>%
  filter(milrinone == 'High') %>%
  select(PX_ID, milrinone, milrinone_dose, 
         McsdWithRhfMilrinone, InotropeMilrinone, 
         ExtMcsdMilrinone.x, ExtMcsdMilrinone.y, 
         ExtInotropeMilrinone.x, ExtInotropeMilrinone.y,
         ExtInoMulMilrinone.x, ExtInoMulMilrinone.y,
         ExtInoSinMilrinone.x, ExtInoSinMilrinone.y,
         InoSinMilrinone.x, InoSinMilrinone.y,
         InoMulMilrinone.x, InoMulMilrinone.y)

### Epinephrine

df <- df %>% 
  mutate(epinephrine = case_when(
    
    as.numeric(epinephrine_dose) >= 0.02 | as.numeric(McsdWithRhfEpinephrine) >= 0.02 | 
      as.numeric(InotropeEpinephrine) >= 0.02 |
      as.numeric(ExtMcsdEpinephrine.y) >= 0.02 | 
      as.numeric(ExtInotropeEpinephrine.y) >= 0.02 | 
      ExtInoSinEpinephrine.y == 1 | InoSinEpinephrine.y == 1 ~ 'High',
    
    TRUE ~ 'Not High'))

df %>%
  filter(epinephrine == 'High') %>%
  select(PX_ID, epinephrine, epinephrine_dose, McsdWithRhfEpinephrine, 
         InotropeEpinephrine, 
         ExtMcsdEpinephrine.x, ExtMcsdEpinephrine.y, 
         ExtInotropeEpinephrine.x, ExtInotropeEpinephrine.y,
         ExtInoMulEpinephrine.x, ExtInoMulEpinephrine.y,
         ExtInoSinEpinephrine.x, ExtInoSinEpinephrine.y,
         InoSinEpinephrine.x, InoSinEpinephrine.y,
         InoMulEpinephrine.x, InoMulEpinephrine.y)

### Remove component variables

df <- df %>%
  select(-c(
    
    ExtInoSingleDose, ExtInoMultiDose, 
    
    dopamine_dose, McsdWithRhfDopamineDt, InotropeDopamine, 
    ExtMcsdDopamine.x, ExtMcsdDopamine.y, 
    ExtInotropeDopamine.x, ExtInotropeDopamine.y,
    ExtInoMulDopamine.x, ExtInoMulDopamine.y,
    InoMulDopamine.x, InoMulDopamine.y,
    
    dobutamine_dose, McsdWithRhfDobutamineDt, InotropeDobutamine, 
    ExtMcsdDobutamine.x, ExtMcsdDobutamine.y, 
    ExtInotropeDobutamine.x, ExtInotropeDobutamine.y,
    ExtInoMulDobutamine.x, ExtInoMulDobutamine.y,
    ExtInoSinDobutamine.x, ExtInoSinDobutamine.y,
    InoSinDobutamine.x, InoSinDobutamine.y,
    InoMulDobutamine.x, InoMulDobutamine.y,
    
    milrinone_dose, McsdWithRhfEpinephrineDt, InotropeMilrinone, 
    ExtMcsdMilrinone.x, ExtMcsdMilrinone.y, 
    ExtInotropeMilrinone.x, ExtInotropeMilrinone.y,
    ExtInoMulMilrinone.x, ExtInoMulMilrinone.y,
    ExtInoSinMilrinone.x, ExtInoSinMilrinone.y,
    InoSinMilrinone.x, InoSinMilrinone.y,
    InoMulMilrinone.x, InoMulMilrinone.y,
    
    epinephrine_dose, McsdWithRhfMilrinoneDt, InotropeEpinephrine, 
    ExtMcsdEpinephrine.x, ExtMcsdEpinephrine.y, 
    ExtInotropeEpinephrine.x, ExtInotropeEpinephrine.y,
    ExtInoMulEpinephrine.x, ExtInoMulEpinephrine.y,
    ExtInoSinEpinephrine.x, ExtInoSinEpinephrine.y,
    InoSinEpinephrine.x, InoSinEpinephrine.y,
    InoMulEpinephrine.x, InoMulEpinephrine.y))


## Other treatment variables

df <- df %>%
  mutate(
    life_arrhythmia = case_when(VentricularEpisode == 1 ~ 1, TRUE ~ 0),
    
    angina = case_when(CriteriaIschemicHeart == 1 ~ 1, TRUE ~ 0),
    
    BiVAD_no_discharge = case_when(CriteriaBivadSupport == 1 ~ 1, TRUE ~ 0),
    
    temp_surg_LVAD = case_when(CriteriaLvadSupport2 == 1 ~ 1, TRUE ~ 0),
    
    v_tach_fib = case_when(CriteriaVentEpisode == 1 ~ 1, TRUE ~ 0),
    
    perc_LVAD = case_when(CriteriaMcsdEndovasSupp == 1 ~ 1, TRUE ~ 0),
    
    
    other_durable_MCSD = case_when(CriteriaDurableDevSupport == 1 ~ 1, TRUE ~ 0),
    
    MCSD_malfunction = case_when(CriteriaMcsdMalfunction == 1 ~ 1, TRUE ~ 0),
    
    MCSD_hemolysis = case_when(CriteriaMcsdWithHemo == 1  ~ 1, TRUE ~ 0),
    
    MCSD_device_infx = case_when(CriteriaMcsdInfection == 1 ~ 1, TRUE ~ 0),
    
    MCSD_bleed = case_when(CriteriaMcsdMucosalBleed == 1 ~ 1, TRUE ~ 0),
    
    MCSD_aortic_insf = case_when(CriteriaMcsdWithAI == 1 ~ 1, TRUE ~ 0),
    
    MCSD_rhf = case_when(
      CriteriaMcsdWithRhf == 1 | !is.na(McsdWithRhfDobutamine) | 
        !is.na(McsdWithRhfDopamine) |
        !is.na(McsdWithRhfMilrinone) | !is.na(McsdWithRhfEpinephrine) | 
        (!is.na(McsdWithRhfNitricOxide) & McsdWithRhfNitricOxide > 0) | 
        (!is.na(McsdWithRhfProstacyclin) & McsdWithRhfProstacyclin > 0) |   
        !is.na(McsdWithRhfPcwp) | !is.na(McsdWithRhfVenPressure) ~ 1,
      TRUE ~ 0),
    
    MCSD_complication = case_when(
      MCSD_malfunction == 1 | MCSD_hemolysis == 1 | 
        MCSD_device_infx == 1 | MCSD_bleed == 1 | 
        MCSD_aortic_insf == 1 | MCSD_rhf == 1 | 
        CriteriaMcsdWithAI == 1 | CriteriaMcsdWithPump == 1 ~ 1,
      TRUE ~ 0),
    
    
    IV_inotropes = case_when(
      CriteriaInotropeSupport == 1 | CriteriaInotropeSupport4 == 1 | 
        InoSingle == 1 | InoMultiple == 1 |  InoInotropeSupport == 1 |
        InoInvasiveCatheter == 1 | InoHemodynamicMonitoring == 1 | 
        !is.na(InoCardiacIndex) | !is.na(InoCapWedgePressure) | 
        !is.na(InoSysBloodPressure) |
        !is.na(InotropePcwp) | !is.na(InotropeCardiacIndex) |
        dopamine == 'High' | dobutamine == 'High' | 
        epinephrine == 'High' | milrinone == 'High' ~ 1,
      
      TRUE ~ 0),
    
    IABP = case_when(
      CriteriaIabpSupport == 1 | CriteriaIabpSupport3 == 1 | IabpWithHemo == 1  | 
        IabpWithoutHemo == 1 |
        !is.na(IabpCapWedgePressure) | !is.na(IabpCardiacIndex) | 
        !is.na(IabpSystolicBloodPressure) | !is.na(IabpWithoutHemoAlt) | 
        !is.na(IabpWithoutHemoArtLac) | !is.na(IabpWithoutHemoAst) |
        !is.na(IabpWithoutHemoCprDt) | !is.na(IabpWithoutHemoSbp) ~ 1,
      
      TRUE ~ 0),
    
    ECMO = case_when(
      CriteriaEcmoSupport == 1 | CriteriaVaEcmoSupport == 1 | 
        EcmoWithHemo == 1 | EcmoWithoutHemo == 1 | 
        !is.na(EcmoCapWedgePressure) | !is.na(EcmoCardiacIndex) | !is.na(EcmoSystolicBloodPressure) |
        !is.na(EcmoWithoutHemoAlt) | !is.na(EcmoWithoutHemoArtLac) | !is.na(EcmoWithoutHemoAst) |
        !is.na(EcmoWithoutHemoCprDt) | !is.na(EcmoWithoutHemoCprDt) | !is.na(EcmoWithoutHemoSbp) ~ 1,
      
      TRUE ~ 0
    )
  ) %>%
  
  
  ### Remove component variables
  select(-c(VentricularEpisode, CriteriaIschemicHeart,
            CriteriaMcsdMalfunction, CriteriaVentEpisode,
            CriteriaMcsdWithHemo, CriteriaMcsdInfection, CriteriaMcsdMucosalBleed, 
            CriteriaMcsdWithAI, CriteriaMcsdWithPump,
            
            CriteriaMcsdWithRhf, McsdWithRhfDobutamine, McsdWithRhfDopamine, 
            McsdWithRhfMilrinone,McsdWithRhfEpinephrine, McsdWithRhfNitricOxide, 
            McsdWithRhfNitricOxideDt, McsdWithRhfProstacyclin, McsdWithRhfProstacyclinDt,
            McsdWithRhfPcwp, McsdWithRhfPcwpDt, McsdWithRhfVenPressure, 
            McsdWithRhfVenPressureDt,
            
            CriteriaInotropeSupport, CriteriaInotropeSupport4, InoSingle, 
            InoMultiple, InoInotropeSupport, InoInvasiveCatheter, 
            InoHemodynamicMonitoring,  InoCardiacIndex, InoCapWedgePressure, 
            InoSysBloodPressure, InotropePcwp, InotropeCardiacIndex,
            
            CriteriaIabpSupport, CriteriaIabpSupport3, IabpWithHemo, IabpWithoutHemo,
            IabpCapWedgePressure, IabpCardiacIndex, IabpSystolicBloodPressure, 
            IabpWithoutHemoAlt, IabpWithoutHemoArtLac, IabpWithoutHemoAst, 
            IabpWithoutHemoCprDt, IabpWithoutHemoSbp,
            
            CriteriaEcmoSupport, CriteriaVaEcmoSupport, EcmoWithHemo, EcmoWithoutHemo,
            EcmoCapWedgePressure, EcmoCardiacIndex, EcmoSystolicBloodPressure, 
            EcmoWithoutHemoAlt, EcmoWithoutHemoArtLac, EcmoWithoutHemoAst, 
            EcmoWithoutHemoCprDt, EcmoWithoutHemoCprDt, EcmoWithoutHemoSbp,
            
            ecmo_hemo, ecmo_without_hemo, iabp_hemo, 
            iabp_without_hemo, iabp_cardiacindex))


setwd("/gpfs/data/massielab/data/srtr/srtr2308/saf/sas") 

df_candthor <- haven::read_sas('./cand_thor.sas7bdat')

setwd("/gpfs/home/fatman01/Rcodes")


# filters the dataset based on the PX_ID values that are present in the postpolicy_id list
#  remove any variable labels from the dataset

df_candthor <- df_candthor[which(df_candthor$PX_ID %in% postpolicy_id), ]
df_candthor <- zap_labels(df_candthor)
df_candthor_raw <- df_candthor

df_candthor <- df_candthor[,c('PX_ID', 'CAN_LISTING_DT', 'PERS_SSA_DEATH_DT',
                              'PERS_OPTN_DEATH_DT', 'CAN_REM_DT', 'CAN_DEATH_DT',
                              'CAN_REM_CD', 'REC_TX_DT', 'CAN_LISTING_CTR_ID',
                              'CAN_LAST_ACT_STAT_DT', 'CAN_LAST_INACT_STAT_DT',
                              'CAN_VAD_TY', 'CAN_VAD1', 'CAN_VAD2', 
                              'CAN_AGE_IN_MONTHS_AT_LISTING', 'CAN_DGN', 
                              'CAN_DIAB_TY', 'CAN_DIAL', 'CAN_INIT_STAT')]


df_candthor <- df_candthor %>% 
  

  mutate(
    transplant_cand_thor = case_when(!is.na(REC_TX_DT) | CAN_REM_CD == '4' ~ 1,
                                     TRUE ~ 0),
    
    CenterId = CAN_LISTING_CTR_ID,
    removal_code_cand_thor = CAN_REM_CD,
    start_date = as.Date(CAN_LISTING_DT),
    transplant_date = as.Date(REC_TX_DT),
    removal_date = as.Date(CAN_REM_DT),
    last_active_date = as.Date(CAN_LAST_ACT_STAT_DT),
    last_inactive_date = as.Date(CAN_LAST_INACT_STAT_DT),
    death_date = as.Date(CAN_DEATH_DT),
    death_date_max = pmax(CAN_DEATH_DT, PERS_SSA_DEATH_DT, PERS_OPTN_DEATH_DT, na.rm=T)) %>%
  
  mutate(transplant_time = ifelse(!is.na(transplant_date), 
                                  as.numeric(transplant_date - start_date, units='days'), 0)) %>%
  mutate(death_time = ifelse(!is.na(death_date_max), as.numeric(death_date_max - start_date, units='days'), 0)) %>%
  mutate(removal_time = as.numeric(pmax(removal_date, last_active_date, last_inactive_date, na.rm = T) - 
                                     start_date, units='days')) %>%
  mutate(
    extra_time = case_when(
      transplant_cand_thor != '1' & CAN_REM_CD != '8'  ~ 
        as.numeric(death_time - removal_time, units = 'days'),
      TRUE ~ 0)) %>%
  
  ## Keep only relevant variables
  select(PX_ID, CenterId, start_date, transplant_cand_thor, transplant_time, death_time, death_date_max, 
         extra_time, transplant_date, removal_date, removal_time,
         removal_code_cand_thor, death_date, death_date_max, CAN_VAD_TY, CAN_VAD1, 
         CAN_VAD2, CAN_AGE_IN_MONTHS_AT_LISTING, CAN_DGN, CAN_DIAB_TY, CAN_DIAL, 
         CAN_INIT_STAT, CAN_LISTING_DT)

df_candthor <- df_candthor %>%
  select(-CAN_INIT_STAT)

df <- merge(df, df_candthor, all.x = T, all.y = F, by = 'PX_ID')
df <- df[order(df$PX_ID, df$t_start), ]
df <- df %>% relocate(CenterId, .after = PX_ID)
df$age <- df$CAN_AGE_IN_MONTHS_AT_LISTING / 12

## Initial status at listing
# 2010 = Status 1A, 2020 = Status 1B, 2030 = (Old) Status 2
df$status <- recode_factor(df$status,
                           '1110' = '1', '1120' = '2', '1130' = '3',
                           '1140' = '4', '1150' = '5', '1160' = '6',
                           '2110' = '1', '2120' = '2', '2130' = '3',
                           '2140' = '4', '2150' = '5', '2160' = '6',
                           
                           '1999' = 'inactive', 
                           '2010' = NA_character_, '2020' = NA_character_, '2030' = '6',
                           '2999' = 'inactive')

df$status_initial <- recode_factor(df$CAN_INIT_STAT,
                                   '1110' = '1', '1120' = '2', '1130' = '3',
                                   '1140' = '4', '1150' = '5', '1160' = '6',
                                   '2110' = '1', '2120' = '2', '2130' = '3',
                                   '2140' = '4', '2150' = '5', '2160' = '6',
                                   
                                   '1999' = 'inactive', 
                                   '2010' = NA_character_, '2020' = NA_character_, '2030' = '6',
                                   '2999' = 'inactive')

df$diagnosis <- as.numeric(df$CAN_DGN)
df <- df %>% mutate(
  diagnosis = case_when(
    diagnosis >= 1000 & diagnosis <= 1049 & diagnosis != 1007 ~ 'Dilated_CM',
    diagnosis >= 1050 & diagnosis <= 1099 & diagnosis != 1051 ~ 'Restricted',
    diagnosis == 1051 ~ 'Amyloid',
    diagnosis >= 1101 & diagnosis <= 1106 ~ 'Re-Transplant',
    diagnosis == 1201 ~ 'Hypertrophic',
    diagnosis == 1202 ~ 'Valvular',
    diagnosis == 1203 | (diagnosis >= 1205 & diagnosis <= 1209) ~ 'Congenital', 
    diagnosis == 1200 | diagnosis == 1007 ~ 'Ischemic',
    TRUE ~ 'Other'), 
  
  diabetes = case_when(
    CAN_DIAB_TY == 1 ~ '0',
    CAN_DIAB_TY == 2 | CAN_DIAB_TY == 3 | CAN_DIAB_TY == 4 | CAN_DIAB_TY == 5 ~ '1',
    TRUE ~ NA_character_),
  
  
  dialysis = case_when(
    CAN_DIAL == 1 ~ '0',
    CAN_DIAL == 2 | CAN_DIAL == 3 | CAN_DIAL == 4 | CAN_DIAL == 5 | CAN_DIAL == 999 ~ '1',
    TRUE ~ NA_character_)
)


## LVAD, RVAD, BiVAD
durable_lvad_types <- c('202', '205', '206', '207', '208', '209', '210', 
                        '212', '213', '214', '223', '224', '233', '236', 
                        '239', '240', '312', '313', '314', '315', '316', '319', '322', 
                        '327', '330', '333', '334')

df <- df %>% mutate(
  durable_LVAD = case_when(
    CriteriaLvadSupport4 == 1 | CriteriaLvadDiscSupport == 1 |
      ((CAN_VAD_TY == 2 & status != '5' & status != '6') & 
         ((CAN_VAD1 %in% durable_lvad_types) |
            (CAN_VAD2 %in% durable_lvad_types))) ~ 1,
    TRUE ~ 0), 
  
  
  RVAD = case_when(
    CriteriaDurableDevSupport == 1 | CriteriaMcsdEndovasSupp == 1 | CriteriaPercuSupport == 1 | 
      (CAN_VAD_TY == 3) |
      (CAN_VAD_TY != 1 & !is.na(CAN_VAD_TY) & 
         !(CAN_VAD1 %in% c('205', '236', '313', '330', '206', '208', '314', 
                           '210', '319', '216', '305', '217', '306', '223', 
                           '312', '224', '316', '230', '324', '231', '325', 
                           '232', '326', '233', '327'))) ~ 1,
    TRUE ~ 0),
  
  
  BiVAD = case_when(
    CriteriaMcsdEndovasSupp == 1 | CriteriaPercuSupport == 1 | CriteriaBivadSupport == 1 |
      (durable_LVAD == 1 & RVAD == 1) |
      bivad == 1 ~ 1,
    TRUE ~ 0))

df <- df %>% select(-c(start_date, transplant_time, death_time, death_date_max, transplant_date,
                       removal_date, removal_time, removal_code_cand_thor, death_date, extra_time,
                       
                       age_m, CAN_AGE_IN_MONTHS_AT_LISTING, CAN_INIT_STAT, CAN_DGN, CAN_DIAB_TY, CAN_DIAL,
                       
                       CriteriaLvadSupport2, CriteriaLvadSupport3, CriteriaLvadSupport4, 
                       CriteriaLvadDiscSupport, CriteriaBivadSupport, 
                       CriteriaMcsdEndovasSupp, CriteriaPercuSupport, CriteriaDurableDevSupport, 
                       CAN_VAD_TY, CAN_VAD1, CAN_VAD2, VAD, VAD_brand, bivad))



setwd("/gpfs/data/massielab/data/srtr/srtr2410/thor_reg/sas") 
df_riskstrat <- haven::read_sas('./RiskStratDataHR.sas7bdat')
setwd("/gpfs/home/fatman01/Rcodes")
# selects relevant columns.
df_riskstrat <- df_riskstrat %>% select(px_id, HrSevFailNtBnpType) 

df$BNP_NT_Pro <- 0
df$BNP_NT_Pro[df$PX_ID %in%
                unique(df_riskstrat$px_id[df_riskstrat$HrSevFailNtBnpType == 'NT Pro BNP'])] <- 1

table(df$BNP_NT_Pro)

### Assume sodium < 100 is a data entry error
df$sodium <- as.numeric(df$sodium)
df$sodium[df$sodium < 100] <- NA


### Deal with inactive statuses
df$status[df$status == 'inactive'] <- NA_character_

df <- df %>% group_by(PX_ID) %>%
  dplyr::mutate(transplant = max(transplant))

### Deal with inactive statuses
df$status[df$status == 'inactive'] <- NA_character_
df_intervals <- df

df_intervals$exception[is.na(df_intervals$exception) == T] <- '0'

### Divide certain variables by 10 or 100
df_intervals <- df_intervals %>% 
  mutate(BNP = as.numeric(BNP) / 100,
         BUN = as.numeric(BUN) / 10,
         AST = as.numeric(AST) / 100,
         heart_rate = as.numeric(heart_rate) / 10)

# 7. Add memory variables

### Treatments: ever on?

df_intervals <- df_intervals %>%
  group_by(PX_ID) %>%
  
  mutate(IV_inotropes_ever = case_when(IV_inotropes == '1' ~ '1', TRUE ~ NA_character_),
         IABP_ever = case_when(IABP == '1' ~ '1', TRUE ~ NA_character_),
         ECMO_ever = case_when(ECMO == '1' ~ '1', TRUE ~ NA_character_),
         BiVAD_ever = case_when(BiVAD == '1' ~ '1', TRUE ~ NA_character_),
         LVAD_ever = case_when(durable_LVAD == '1' ~ '1', TRUE ~ NA_character_),
         RVAD_ever = case_when(RVAD == '1' ~ '1', TRUE ~ NA_character_),
         exception_ever = case_when(exception == '1' ~ '1', TRUE ~ NA_character_),
         temp_surg_ever = case_when(temp_surg_LVAD == '1' ~ '1', TRUE ~ NA_character_),
         BiVAD_no_discharge_ever = case_when(BiVAD_no_discharge == '1' ~ '1', TRUE ~ NA_character_)) %>%
  
  fill(ECMO_ever, .direction = 'down') %>%
  fill(BiVAD_ever, .direction = 'down') %>%
  fill(LVAD_ever, .direction = 'down') %>%
  fill(RVAD_ever, .direction = 'down') %>%
  fill(exception_ever, .direction = 'down') %>%
  fill(temp_surg_ever, .direction = 'down') %>%
  fill(BiVAD_no_discharge_ever, .direction = 'down') %>%
  
  mutate(IV_inotropes_ever = case_when(is.na(IV_inotropes_ever) ~ '0', TRUE ~ IV_inotropes_ever),
         IABP_ever = case_when(is.na(IABP_ever) ~ '0', TRUE ~ IABP_ever),
         ECMO_ever = case_when(is.na(ECMO_ever) ~ '0', TRUE ~ ECMO_ever),
         BiVAD_ever = case_when(is.na(BiVAD_ever) ~ '0', TRUE ~ BiVAD_ever),
         LVAD_ever = case_when(is.na(LVAD_ever) ~ '0', TRUE ~ LVAD_ever),
         RVAD_ever = case_when(is.na(RVAD_ever) ~ '0', TRUE ~ RVAD_ever),
         exception_ever = case_when(is.na(exception_ever) ~ '0', TRUE ~ exception_ever),
         temp_surg_ever = case_when(is.na(temp_surg_ever) ~ '0', TRUE ~ temp_surg_ever),
         BiVAD_no_discharge_ever = case_when(is.na(BiVAD_no_discharge_ever) ~ '0', TRUE ~ BiVAD_no_discharge_ever))


df_intervals$arterial_lactate[is.na(df_intervals$arterial_lactate)] <- 1
df_intervals$LDH[is.na(df_intervals$LDH)] <- 220
df_intervals$hemoglobin[is.na(df_intervals$hemoglobin) & df_intervals$CAN_GENDER == 'M'] <- 14.9
df_intervals$hemoglobin[is.na(df_intervals$hemoglobin) & df_intervals$CAN_GENDER == 'F'] <- 13.3

df_intervals$BNP[is.na(df_intervals$BNP) & df_intervals$BNP_NT_Pro == '0'] <- 
  median(df_intervals$BNP[df_intervals$BNP_NT_Pro == '0'], na.rm=T)
df_intervals$BNP[is.na(df_intervals$BNP) & df_intervals$BNP_NT_Pro == '1'] <- 
  median(df_intervals$BNP[df_intervals$BNP_NT_Pro == '1'], na.rm=T)


df_intervals$sodium[is.na(df_intervals$sodium)] <- median(df_intervals$sodium, na.rm=T)
df_intervals$albumin[is.na(df_intervals$albumin)] <- median(df_intervals$albumin, na.rm=T)
df_intervals$bilirubin[is.na(df_intervals$bilirubin)] <- median(df_intervals$bilirubin, na.rm=T)


setwd("/gpfs/data/massielab/data/srtr/srtr2308/saf/sas") 
df_bsa <- haven::read_sas('./cand_thor.sas7bdat')
setwd("/gpfs/home/fatman01/Rcodes")

df_bsa <- df_bsa %>%
  select(PX_ID, CAN_HGT_CM, CAN_WGT_KG, CAN_GENDER) %>%
  mutate(bsa = 0.007184 * (CAN_HGT_CM ^ 0.425) * (CAN_WGT_KG ^ 0.725)) %>%
  select(PX_ID, bsa, CAN_GENDER)

df_bsa <- df_bsa[!duplicated(df_bsa), ]

# combine df_intervals (which likely contains patient interval data) with df_bsa (which contains the BSA and gender information)

df_intervals <- merge(df_intervals, df_bsa, by = 'PX_ID', all.x = T, all.y = F)
rm(df_bsa)


df_intervals <- df_intervals %>%
  mutate(cardiac_output = ifelse((is.na(cardiac_output) & durable_LVAD == '1'),  2.5*bsa, cardiac_output)) %>%
  mutate(cardiac_output = ifelse((is.na(cardiac_output) & durable_LVAD != '1'),  2.2*bsa, cardiac_output)) %>%
  select(-c(bsa))


df_intervals$central_venous_pressure[is.na(df_intervals$central_venous_pressure) & df_intervals$durable_LVAD == '1'] <- 10
df_intervals$PASP[is.na(df_intervals$PASP) & df_intervals$durable_LVAD == '1'] <- 35
df_intervals$PADP[is.na(df_intervals$PADP) & df_intervals$durable_LVAD == '1'] <- 15
df_intervals$PCWP[is.na(df_intervals$PCWP) & df_intervals$durable_LVAD == '1'] <- 15
df_intervals$systolicBP[is.na(df_intervals$systolicBP) & df_intervals$durable_LVAD == '1'] <- 100
df_intervals$diastolicBP[is.na(df_intervals$diastolicBP) & df_intervals$durable_LVAD == '1'] <- 80
df_intervals$heart_rate[is.na(df_intervals$heart_rate) & df_intervals$durable_LVAD == '1'] <- 8.5

df_intervals$central_venous_pressure[is.na(df_intervals$central_venous_pressure) & df_intervals$durable_LVAD != '1'] <- 12
df_intervals$PASP[is.na(df_intervals$PASP) & df_intervals$durable_LVAD != '1'] <- 38
df_intervals$PADP[is.na(df_intervals$PADP) & df_intervals$durable_LVAD != '1'] <- 18
df_intervals$PCWP[is.na(df_intervals$PCWP) & df_intervals$durable_LVAD != '1'] <- 18
df_intervals$systolicBP[is.na(df_intervals$systolicBP) & df_intervals$durable_LVAD != '1'] <- 110
df_intervals$diastolicBP[is.na(df_intervals$diastolicBP) & df_intervals$durable_LVAD != '1'] <- 80
df_intervals$heart_rate[is.na(df_intervals$heart_rate) & df_intervals$durable_LVAD != '1'] <- 8.5

# For patients with durable LVAD (LVAD == '1'), missing values for central venous pressure, PASP, PADP, PCWP, systolicBP, diastolicBP, and heart_rate are replaced by fixed values (e.g., 10 mm Hg for CVP, 35 mm Hg for PASP, etc.)

df_intervals$central_venous_pressure[is.na(df_intervals$central_venous_pressure) & df_intervals$durable_LVAD != '1'] <- 12
df_intervals$PASP[is.na(df_intervals$PASP) & df_intervals$durable_LVAD != '1'] <- 38
df_intervals$PADP[is.na(df_intervals$PADP) & df_intervals$durable_LVAD != '1'] <- 18
df_intervals$PCWP[is.na(df_intervals$PCWP) & df_intervals$durable_LVAD != '1'] <- 18
df_intervals$systolicBP[is.na(df_intervals$systolicBP) & df_intervals$durable_LVAD != '1'] <- 110
df_intervals$diastolicBP[is.na(df_intervals$diastolicBP) & df_intervals$durable_LVAD != '1'] <- 80
df_intervals$heart_rate[is.na(df_intervals$heart_rate) & df_intervals$durable_LVAD != '1'] <- 8.5



missing_pct <- sapply(df_intervals, function(x) mean(is.na(x))) * 100
missing_pct

missing_pct_sorted <- sort(missing_pct, decreasing = TRUE)
missing_pct_sorted



factor_vars <- c('diagnosis', 'diabetes', 'dialysis', 'IV_inotropes', 'dobutamine', 
                 'dopamine', 'epinephrine', 'milrinone', 'IABP', 'ECMO', 'BiVAD', 'durable_LVAD', 'RVAD',
                 'MCSD_complication', 'life_arrhythmia', 'BiVAD_no_discharge', 
                 'temp_surg_LVAD', 'other_durable_MCSD', 'MCSD_malfunction', 'v_tach_fib', 
                 'perc_LVAD', 'MCSD_hemolysis', 'MCSD_rhf', 'MCSD_device_infx', 
                 'MCSD_bleed', 'MCSD_aortic_insf', 'angina', 'IV_inotropes_ever', 
                 'IABP_ever', 'ECMO_ever', 'BiVAD_ever', 'LVAD_ever', 'RVAD_ever')

numeric_vars <- c('interval', 'interval_start', 'interval_stop', 'age', 
                  'systolicBP', 'diastolicBP', 'PASP', 'PADP', 'heart_rate', 'cardiac_output', 
                  'central_venous_pressure', 'arterial_lactate', 
                  'PCWP', 'hemoglobin', 'albumin', 'bilirubin', 'creatinine', 'eGFR', 'sodium', 
                  
                  'AST', 'BNP', 'BUN', 'INR', 'LDH')

df_intervals$diagnosis <- relevel(factor(df_intervals$diagnosis), ref = 'Dilated_CM')
df_intervals$dobutamine <- relevel(factor(df_intervals$dobutamine), ref = 'Not High')
df_intervals$dopamine <- relevel(factor(df_intervals$dopamine), ref = 'Not High')
df_intervals$epinephrine <- relevel(factor(df_intervals$epinephrine), ref = 'Not High')
df_intervals$milrinone <- relevel(factor(df_intervals$milrinone), ref = 'Not High')


df_intervals$cardiac_output <- as.numeric(df_intervals$cardiac_output)
df_intervals$systolicBP <- as.numeric(df_intervals$systolicBP)
df_intervals$diastolicBP <- as.numeric(df_intervals$diastolicBP)
df_intervals$PASP <- as.numeric(df_intervals$PASP)
df_intervals$PADP <- as.numeric(df_intervals$PADP)
df_intervals$PCWP <- as.numeric(df_intervals$PCWP)
df_intervals$central_venous_pressure <- as.numeric(df_intervals$central_venous_pressure)


df_intervals <- df_intervals %>% 
  mutate(
    cpo = (1/541) * cardiac_output * (((2/3)*systolicBP) + ((1/3)*diastolicBP)), 
    api = (systolicBP - diastolicBP) / PCWP,
    papi = (PASP - PADP) / central_venous_pressure)

df_intervals$api[df_intervals$PCWP == 0] <- NA
df_intervals$papi[df_intervals$central_venous_pressure == 0] <- NA

df_intervals <- df_intervals %>%
  mutate(short_MCS_ever = 
           case_when(
             ECMO_ever == '1' | temp_surg_ever == '1' | BiVAD_no_discharge_ever == '1' ~ 1,
             TRUE ~ 0))


save(df_intervals, file = 'Candidatelongitudinaldataset.RData')

write.csv(df_intervals, file = 'Candidatelongitudinaldataset.csv', row.names = FALSE)



setwd("/gpfs/home/fatman01/PTRdatasetcompilation")

longitudinaldataset <- read.csv("Candidatelongitudinaldataset.csv")


longitudinaldataset <- longitudinaldataset %>%
  select(-c(CAN_MIN_AGE, CAN_MAX_AGE, CAN_GENDER.x)) %>%
  mutate(CAN_GENDER = CAN_GENDER.y) %>%
  select(-CAN_GENDER.y)

longitudinaldataset <- longitudinaldataset %>%
  mutate(date = as.Date(date))


filtered_px_ids <- longitudinaldataset %>%
  filter(str_detect(events, "CAN_LISTING_DT") & 
           date >= as.Date("2021-01-01") & 
           date <= as.Date("2021-03-01")) %>%
  pull(PX_ID) %>%
  unique()

result <- longitudinaldataset %>%
  filter(PX_ID %in% filtered_px_ids)


setwd("/gpfs/home/fatman01/Heart simulator")
candidate_latlong <- read_csv("candidate_center_withlatitudeandlongitude.csv", show_col_types = FALSE)


result1 <- result %>%
  left_join(candidate_latlong, by = 'PX_ID')




num_unique_px_ids <- result1 %>% 
  summarise(count = n_distinct(PX_ID)) %>% 
  pull(count)

print(num_unique_px_ids)

table(result1$status_initial)


for (i in 1:132) {
  column_name <- colnames(result1)[i]
  column_data <- result1[[i]]
  missing_count <- sum(is.na(column_data))
  
  cat("\n----------------------------------\n")
  cat("Column", i, "(", column_name, ") has", missing_count, "missing values.\n")
  cat("Value counts (including NAs):\n")
  #print(table(column_data, useNA = "ifany"))
}

result1 <- result1 %>%
  select(-c(SVO2, CAN_RACE, CAN_MIN_HGT, CAN_MAX_HGT, CAN_ACPT_HIST_CIGARETTE, CAN_ICU, CAN_DRUG_TREAT_COPD, CAN_EXERCISE_O2, CAN_DRUG_TREAT_HYPERTEN , peripheral_vascular, CAN_ANTI_ARRYTHM, CAN_OTHER_TOBACCO_USE,
            CAN_PULM_EMBOL))


result1 <- result1 %>%
  group_by(PX_ID) %>%
  filter(
    all(!is.na(CenterId)) & 
      all(!is.na(cardiac_output)) &
      all(!is.na(cardiac_index)) &
      all(!is.na(AST)) &
      all(!is.na(creatinine)) &
      all(!is.na(BUN)) &
      all(!is.na(INR)) &
      all(!is.na(CAN_HGT_CM)) &
      all(!is.na(CAN_WGT_KG)) &
      all(!is.na(dialysis)) &
      all(!is.na(CAN_BMI)) &
      all(!is.na(cpo)) &
      all(!is.na(api)) &
      all(!is.na(papi))
  ) %>%
  ungroup()


# recheck the missingness

for (i in 1:119) {
  column_name <- colnames(result1)[i]
  column_data <- result1[[i]]
  missing_count <- sum(is.na(column_data))
  
  cat("\n----------------------------------\n")
  cat("Column", i, "(", column_name, ") has", missing_count, "missing values.\n")
  cat("Value counts (including NAs):\n")
  #print(table(column_data, useNA = "ifany"))
}



setdiff(colnames(result1), colnames(candidate_dataset))


table(result1$CAN_ABO)


result1 <- result1 %>%
  group_by(PX_ID) %>%
  filter(!any(CAN_ABO %in% c("A1", "A2"))) %>%
  ungroup()



result1 <- result1 %>%
  relocate(status_initial, status_criteria, status, ExtensionNumber, exception, listing_description, FormStatus_descrip, .after = t_stop)

result2 <- result1 %>%
  filter(t_start != t_stop)



result3 <- result2 %>%
  group_by(PX_ID) %>%
  arrange(t_start, .by_group = TRUE) %>%
  mutate(
    status = as.character(status),
    status_initial = as.character(status_initial),
    status = na.locf(if_else(row_number() == 1, status_initial, status),
                     na.rm = FALSE)
  ) %>%
  ungroup()




result4 <- result3 %>%
  group_by(PX_ID) %>%
  mutate(
    status1 = if_else(
      as.character(status) != as.character(status_initial),
      as.character(status),
      as.character(status_initial)
    )
  ) %>%
  relocate(status1, .after = status) %>%
  ungroup()


setwd("/gpfs/home/fatman01/PTRdatasetcompilation")

save(result4, file = 'Candidatedatasetforsimulation.RData')
write.csv(result4, file = 'Candidatedatasetforsimulation.csv', row.names = FALSE)








