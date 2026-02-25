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

#exclure les patients multitransplantés mult_tx = O
data_CHB_BBE <- data_CHB_BBE %>%
  filter(mult_tx == "N")

#exclure les patients avec graft failure < 30 jours
data_CHB_BBE <- data_CHB_BBE %>%
  filter(graft_surv_days >= 30)

#exclure les patients < 18 ans
data_CHB_BBE <- data_CHB_BBE %>%
  filter(rec_age_at_tx >= 18)

#Division du dataset en période de 4 ans
data_CHB_BBE <- data_CHB_BBE %>%
  mutate(tx_period = year(rec_tx_dt))
data_CHB_BBE <- data_CHB_BBE %>%
  mutate(tx_period = case_when(
    tx_period >= 1993 & tx_period <= 1997 ~ "1993-1997",
    tx_period >= 1998 & tx_period <= 2002 ~ "1998-2002",
    tx_period >= 2003 & tx_period <= 2007 ~ "2003-2007",
    tx_period >= 2008 & tx_period <= 2012 ~ "2008-2012",
    tx_period >= 2013 & tx_period <= 2017 ~ "2013-2017",
    tx_period >= 2018 & tx_period <= 2024 ~ "2018-2024",
    TRUE ~ NA_character_
  ))

#Ajout d'une colonne don_age_grp avec les 5 groupes différents d'âge de donneur : < 20 ans, 20-40 ans
#40 - 60 ans, 60-70 ans et > 70 ans
data_CHB_BBE <- data_CHB_BBE %>%
  mutate(don_age_grp = case_when(
    don_age < 20 ~ "Very young donor",
    don_age >= 20 & don_age < 40 ~ "Young donor",
    don_age >= 40 & don_age < 60 ~ "Middle-aged donor",
    don_age >= 60 & don_age < 70 ~ "Old donor",
    don_age >= 70 ~ "Very old donor",
    TRUE ~ NA_character_
  ))

# créer un facteur pour don_age_grp avec les niveaux dans l'ordre souhaité
data_CHB_BBE$don_age_grp <- factor(data_CHB_BBE$don_age_grp, levels = c("Very young donor", "Young donor", 
                                                                        "Middle-aged donor", "Old donor", "Very old donor"))
data_CHB_BBE$don_age_grp <- factor(
  data_CHB_BBE$don_age_grp,
  levels = rev(levels(data_CHB_BBE$don_age_grp))
)    

#Ajout d'une colonne rec_age_grp avec les 5 groupes différetnts d'âge de receveur : < 20 ans, 20-40 ans
#40 - 60 ans, 60-70 ans et > 70 ans
data_CHB_BBE <- data_CHB_BBE %>%
  mutate(rec_age_grp = case_when(
    rec_age_at_tx < 20 ~ "Very young recipient",
    rec_age_at_tx >= 20 & rec_age_at_tx < 40 ~ "Young recipient",
    rec_age_at_tx >= 40 & rec_age_at_tx < 60 ~ "Middle-aged recipient",
    rec_age_at_tx >= 60 & rec_age_at_tx < 70 ~ "Old recipient",
    rec_age_at_tx >= 70 ~ "Very old recipient",
    TRUE ~ NA_character_
  ))

# créer un facteur pour rec_age_grp avec les niveaux dans l'ordre souhaité
data_CHB_BBE$rec_age_grp <- factor(data_CHB_BBE$rec_age_grp, levels = c("Very young recipient", "Young recipient", 
                                                                        "Middle-aged recipient", "Old recipient", "Very old recipient"))
data_CHB_BBE$rec_age_grp <- factor(
  data_CHB_BBE$rec_age_grp,
  levels = rev(levels(data_CHB_BBE$rec_age_grp))
)

#Renommer les valeurs de CAN_GENDER et DON_GENDER dans l'affichage 
#F = Women et M = Men
data_CHB_BBE <- data_CHB_BBE |> 
  mutate(rec_gender = case_when(
    rec_gender == "F" ~ "Women",
    rec_gender == "M" ~ "Men"
  ))

data_CHB_BBE <- data_CHB_BBE |> 
  mutate(don_gender = case_when(
    don_gender == "F" ~ "Women",
    don_gender == "M" ~ "Men"
  ))

#Renommer les valeurs de don_cod
#Anoxie = Anoxia, Traumatique A.V.P. et Traumatique Non A.V.P. = Head trauma, Vasculaire = Cerebrovascular accident or stroke,
#tous les autres = Other
data_CHB_BBE <- data_CHB_BBE |> 
  mutate(don_cod = case_when(
    don_cod == "Anoxie" ~ "Anoxia",
    don_cod == "Traumatique A.V.P." ~ "Head trauma",
    don_cod == "Traumatique Non A.V.P." ~ "Head trauma",
    don_cod == "Vasculaire" ~ "Cerebrovascular accident or stroke",
    TRUE ~ "Other"
  ))

#----- Squvegarde du dataset final ------
save(data_CHB_BBE, file = "data_CHB_BBE.RData")

#----- Données sur les groupes d'âge des donneurs et des receveurs -----

#Tableau des groupes d'âge des donneurs et des receveurs en nombre et pourcentage
table_age_groups <- data_CHB_BBE %>%
  select(don_age_grp, rec_age_grp) %>%
  tbl_summary(
    by = NULL,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no",
    label = list(don_age_grp ~ "Donor Age Group", rec_age_grp ~ "Recipient Age Group")
  ) %>%
  add_overall() %>%
  modify_header(label ~ "**Age Groups**") %>%
  bold_labels()
#affichage et sauvegarde du tableau
table_age_groups |> as_gt() |> gt::gtsave("table_age_groups.png")

#tableau répartition des groupes de donneurs selon le groupe d'âge des receveurs
table_donor_by_recipient_age <- data_CHB_BBE %>%
  select(don_age_grp, rec_age_grp) %>%
  tbl_summary(
    by = rec_age_grp,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no",
    label = list(don_age_grp ~ "Donor Age Group")
  ) %>%
  add_overall() %>%
  modify_header(label ~ "**Donor Age Groups by Recipient Age Groups**") %>%
  bold_labels()

#affichage et sauvegarde du tableau
table_donor_by_recipient_age |> as_gt() |> gt::gtsave("table_donor_by_recipient_age.png")

#Histogramme pour visualiser les âges des donneurs selon les 5 groupes
#data_CHB_BBE$don_age_grp <- fct_relevel(data_CHB_BBE$don_age_grp, "Very young donor", "Young donor", "Middle-aged donor", "Old donor", "Very old donor")

ggplot(data_CHB_BBE, aes(x = don_age_grp))+
  geom_bar(fill = "#246B45", color = "black")+
  labs(title = "Distribution of Donor Ages by Age Group",
       x = "Donor Age Group",
       y = "Count of Donors")+
  theme_minimal()
ggsave("donor_age_distribution.png", width = 8, height = 6)

#Histogramme pour visualiser les âges des receveurs selon les 5 groupes
#data_CHB_BBE$rec_age_grp <- fct_relevel(data_CHB_BBE$rec_age_grp, "Very young recipient", "Young recipient", "Middle-aged recipient", "Old recipient", "Very old recipient")
ggplot(data_CHB_BBE, aes(x = rec_age_grp))+
  geom_bar(fill = "#ED912F", color = "black")+
  labs(title = "Distribution of Recipient Ages by Age Group",
       x = "Recipient Age Group",
       y = "Count of Recipients")+
  theme_minimal()
ggsave("recipient_age_distribution.png", width = 8, height = 6)

#histogramme pour voir la distribution des ages de donneur selon des periodes de 5 ans de 1993 à 2024

# créer un facteur pour tx_period avec les niveaux dans l'ordre souhaité
data_CHB_BBE$tx_period <- factor(data_CHB_BBE$tx_period, levels = c("1993-1997", "1998-2002", "2003-2007", "2008-2012", "2013-2017", "2018-2024"))

# histogramme en nombre
ggplot(data_CHB_BBE, aes(x = tx_period, fill = don_age_grp)) +
  geom_bar(color = "black")+
  scale_fill_manual(values = paletteer_d("NineteenEightyR::miami2", n = 5))+    #palette trouvée sur Color Palette Finder
  labs(title = "Distribution of Donor Ages by Age Group and Transplant Period",
       x = "Donor Age Group",
       y = "Count of Donors",... = "Transplant Period")+
  theme_minimal()

ggsave("donor_age_by_period.png", width = 10, height = 6)

# histogramme en pourcentages
ggplot(data_CHB_BBE, aes(x = tx_period, fill = don_age_grp)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = paletteer_d("NineteenEightyR::miami2", n = 5))+
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Distribution of donor age groups by transplant period",
    x = "Transplant period",
    y = "Proportion of donors",
    fill = "Donor age group"
  ) +
  theme_minimal()
ggsave("donor_age_by_period_percentage.png", width = 10, height = 6)

#histogramme pour voir la distribution des ages de receveur selon des periodes de 5 ans de 1993 à 2024

#Histogramme en nombre
ggplot(data_CHB_BBE, aes(x = tx_period, fill = rec_age_grp)) +
  geom_bar(color = "black")+
  scale_fill_manual(values = paletteer_d("lisa::FernandoBotero", n = 5))+
  labs(title = "Distribution of Recipient Ages by Age Group and Transplant Period",
       x = "Transplant Period",
       y = "Count of Recipients",... = "Transplant Period")+
  theme_minimal()
ggsave("recipient_age_by_period.png", width = 10, height = 6)

#histogramme en pourcentage
ggplot(data_CHB_BBE, aes(x = tx_period, fill = rec_age_grp)) +
  geom_bar(position = "fill", color = "black") +
  scale_fill_manual(values = paletteer_d("lisa::FernandoBotero", n = 5))+
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Distribution of recipient age groups by transplant period",
    x = "Transplant period",
    y = "Proportion of recipients",
    fill = "Recipient age group"
  ) +
  theme_minimal()
ggsave("recipient_age_by_period_percentage.png", width = 10, height = 6)


# ----- Fin du script -----