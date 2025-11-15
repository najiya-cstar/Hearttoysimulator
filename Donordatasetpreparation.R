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
library(fuzzyjoin)



setwd("/gpfs/data/massielab/data/srtr/srtr2501/saf/sas")  # 1.	Import datasets from SRT


don_deceased <- read_sas("donor_deceased.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()

tx_hr <- read_sas( "tx_hr.sas7bdat", NULL) %>%  
  zap_formats() %>% zap_labels()


tx_hr1 <- tx_hr %>%
  select(DONOR_ID, PX_ID)


setwd("/gpfs/home/fatman01/PTRdatasetcompilation")

result <- read_csv("Candidatedatasetforsimulation.csv", show_col_types = FALSE)


filtered_px_ids <- result %>%
  distinct(PX_ID) %>%
  pull(PX_ID)

tx_hr2 <- tx_hr1 %>%
  filter(PX_ID %in% filtered_px_ids) %>%
  select(DONOR_ID, PX_ID)

tx_hr3 <- tx_hr2 %>%
  distinct(DONOR_ID) %>%
  pull(DONOR_ID)


don_deceased1 <- don_deceased %>%
  filter(DONOR_ID %in% tx_hr3)


setwd("/gpfs/home/fatman01/Heart simulator")

donor_lat_long <- read_csv("donor_hospital_withlatitudeandlongitude.csv",show_col_types = FALSE)

don_deceased2 <- don_deceased1 %>%
  left_join(donor_lat_long, by = 'DONOR_ID')


save(don_deceased2, file = 'Donordatasetforsimulation.RData')

write.csv(don_deceased2, file = 'Donordatasetforsimulation.csv', row.names = FALSE)
