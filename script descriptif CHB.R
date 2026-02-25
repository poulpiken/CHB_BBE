#----- Script descriptif des données du SRTR nous intéressant dans l'étude du BBE -----

#----- Chargement des packages nécessaires -----

library(broom)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggpubr)
library(gtsummary)
library(gt)
library(haven)
library(lubridate)
library(paletteer)
library(purrr)
library(readxl)
library(stringr)
library(survival)
library(survminer)
library(tidyr)
library(janitor)

#----- Importation du dataset -----

data_source <- read_excel("data CHB BBE.xlsx", sheet = 1, n_max = 3891)
age_data <- read_excel("data CHB BBE.xlsx", sheet = 2, n_max = 3891)

#----- Fusion des datasets selon les colonnes communes REC_TX_DT, TFL_LASTUPDATE et REC_AGE_AT_TX -----
age_data <- age_data %>%
  mutate(
    REC_TX_DT = as.Date(REC_TX_DT),
    TFL_LASTUPDATE = as.Date(TFL_LASTUPDATE),
    REC_AGE_AT_TX = as.numeric(REC_AGE_AT_TX)
  )

data_source <- data_source %>%
  mutate(
    REC_TX_DT = as.Date(REC_TX_DT),
    TFL_LASTUPDATE = as.Date(TFL_LASTUPDATE),
    REC_AGE_AT_TX = as.numeric(REC_AGE_AT_TX)
  )

data_CHB_BBE <- left_join(
  data_source,
  age_data,
  by = c("REC_TX_DT", "TFL_LASTUPDATE", "REC_AGE_AT_TX")
)

#----- Création d'une colonne d'identification TX_ID -----

data_CHB_BBE <- data_CHB_BBE %>%
  arrange(REC_TX_DT, TFL_LASTUPDATE, REC_AGE_AT_TX) %>%
  mutate(TX_ID = row_number())

#----- Sélection et mise en forme des données -----

data_CHB_BBE <- data_CHB_BBE %>%
  janitor::clean_names()

data_CHB_BBE <- data_CHB_BBE %>%
  select(tx_id, rec_tx_dt, tfl_lastupdate, rec_age_at_tx, don_age, graft_surv_days, tfl_graft_status_x,
         graft_failure, rec_gender, rec_birth_dt, mult_tx, rec_hgt_cm, rec_wgt_kg, bmi, graft_cumulated_age,
    don_gender, don_hgt_cm, don_wgt_kg, don_cod)

#exclure les patients avec mult_tx = O
data_CHB_BBE <- data_CHB_BBE %>%
  filter(mult_tx == "O")

