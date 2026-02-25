#----- Script analytique -----
# Ce script identifie les trois groupes d'intérêt BB, contrôle positif et contrôle négatif et les compare

#----- Chargement des packages nécessaires -----

library(broom)
library(dplyr)
library(forcats)
library(forestplot)
library(ggalluvial)
library(ggplot2)
library(ggpubr)
library(gtsummary)
library(gt)
library(haven)
library(lubridate)
library(paletteer)
library(purrr)
library(readxl)
library(splines)
library(stringr)
library(survival)
library(survminer)
library(tibble)
library(tidyr)

#----- Création des 3 groupes d'intérêt -----

# Groupe BB contient les very old donor ayant donné aux very young recipient et young recipient
BB_group <- data_CHB_BBE %>%
  filter((don_age >= 70 & rec_age_at_tx < 20) | (don_age >= 70 & rec_age_at_tx >= 20 & rec_age_at_tx < 40))
BB_group1 <- data_CHB_BBE %>%
  filter(don_age >= 70 & rec_age_at_tx < 20)
BB_group2 <- data_CHB_BBE %>%
  filter(don_age >= 70 & rec_age_at_tx >= 20 & rec_age_at_tx < 40)

# Groupe contrôle positif contient les very young donor ayant donné aux very young recipient et young recipient
pos_ctrl_group <- data_CHB_BBE %>%
  filter((don_age < 20 & rec_age_at_tx < 20) | (don_age < 20 & rec_age_at_tx >= 20 & rec_age_at_tx < 40))
pos_ctrl_group1 <- data_CHB_BBE %>%
  filter(don_age < 20 & rec_age_at_tx < 20)
pos_ctrl_group2 <- data_CHB_BBE %>%
  filter(don_age < 20 & rec_age_at_tx >= 20 & rec_age_at_tx < 40)

# Groupe contrôle négatif contient les very old donor ayant donné aux very old recipient et old recipient
neg_ctrl_group <- data_CHB_BBE %>%
  filter((don_age >= 70 & rec_age_at_tx >= 70) | (don_age >= 70 & rec_age_at_tx >= 60 & rec_age_at_tx < 70))
neg_ctrl_group1 <- data_CHB_BBE %>%
  filter(don_age >= 70 & rec_age_at_tx >= 70)
neg_ctrl_group2 <- data_CHB_BBE %>%
  filter(don_age >= 70 & rec_age_at_tx >= 60 & rec_age_at_tx < 70)

#----- Table 1 de nos groupes d'intérêt -----
#Faire un tableau descriptif des variables rec_age_at_tx, don_age, rec_gender, don_gender, don_cod, rec_hgt_cm, 
#rec_wgt_kg, don_hgt_cm, don_wgt_kg
#sur l'effectif total et les trois groupes d'intérêt BB group, Negative control et Positive control avec une
#comparaison entre les 3 groupes et une pvalue sur la colonnes la plus à droite

table1 <- bind_rows(
  BB_group       %>% mutate(Group = "BB group"),
  pos_ctrl_group %>% mutate(Group = "Positive control"),
  neg_ctrl_group %>% mutate(Group = "Negative control"),
  data_CHB_BBE %>% mutate(Group = "All patients")
) %>%
  mutate(
    Group = factor(
      Group,
      levels = c(
        "BB group",
        "Positive control",
        "Negative control",
        "All patients"
      )
    )
  ) %>%
  select(
    Group,
    rec_age_at_tx, don_age, rec_gender, don_gender, don_cod, rec_hgt_cm, rec_wgt_kg, don_hgt_cm, don_wgt_kg
  ) %>%
  tbl_summary(
    by = Group,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{median}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 0,
    label = list(
      rec_age_at_tx ~ "Recipient age at transplant",
      rec_gender ~ "Recipient gender",
      rec_hgt_cm ~ "Recipient height in cm",
      rec_wgt_kg ~ "Recipient weight in kg",
      don_age ~ "Donor age",
      don_gender ~ "Donor gender",
      don_hgt_cm ~ "Donor height in cm",
      don_wgt_kg ~ "Donor weight in kg",
      don_cod ~ "Cause of donor death"
    )
  ) %>%
  add_p() %>%
  modify_header(label = "**Variable**") %>%
  modify_spanning_header(
    all_stat_cols() ~ "**Group**"
  )
# Affichage du tableau
table1_gt <- as_gt(table1)
print(table1_gt)

#----- Comparaison des groupes d'intérêt -----
comparison_data <- bind_rows(
  BB_group1       %>% mutate(Group = "Very old donor -> Very young recipient"),
  BB_group2       %>% mutate(Group = "Very old donor -> Young recipient"),
  pos_ctrl_group1 %>% mutate(Group = "Very young donor -> Very young recipient"),
  pos_ctrl_group2 %>% mutate(Group = "Very young donor -> Young recipient"),
  neg_ctrl_group1 %>% mutate(Group = "Very old donor -> Very old recipient"),
  neg_ctrl_group2 %>% mutate(Group = "Very old donor -> Old recipient")
)

# Calcul médiane et quartiles des durées graft_surv_years pour les 3 groupes
summary_df <- comparison_data %>%
  group_by(Group) %>%
  summarise(
    median = median(graft_surv_years, na.rm = TRUE),
    q1     = quantile(graft_surv_years, 0.25, na.rm = TRUE),
    q3     = quantile(graft_surv_years, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
summary_df$Group <- factor(
  summary_df$Group,
  levels = c("Very young donor -> Young recipient", "Very young donor -> Very young recipient", "Very old donor -> Old recipient", "Very old donor -> Very old recipient", "Very old donor -> Young recipient", "Very old donor -> Very young recipient")
)

# Forest plot
ggplot(summary_df, aes(x = median, y = Group, color = Group)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(xmin = q1, xmax = q3),
    width = 0.2
  ) +
  scale_color_manual(
    values = c(
      "Very old donor -> Very young recipient" = "#2e15d1",
      "Very old donor -> Young recipient" = "#2e15d1",
      "Very young donor -> Very young recipient" = "#15D12E",
      "Very young donor -> Young recipient" = "#15D12E",
      "Very old donor -> Very old recipient" = "#d12e15",
      "Very old donor -> Old recipient" = "#d12e15"
    ),
    breaks = c(
      "Very old donor -> Very young recipient",
      "Very young donor -> Very young recipient",
      "Very old donor -> Very old recipient"
    ),
    labels = c(
      "BB group",
      "Positive control",
      "Negative control"
    )
  ) +
  labs(
    x = "Graft survival (years)",
    y = "",
    title = "Graft survival - Median + IQR",
    color = ""
  ) +
  theme_minimal()

# Sauvegarde du graphique
ggsave("graft_survival_forest_plot.png", width = 8, height = 6)


#----- Etude survie patients selon les 3 groupes -----
#créer un nouveau dataframe avec les groupes Bb group, pos et neg control
comparison_data <- bind_rows(
  BB_group       %>% mutate(Group = "BB group"),
  pos_ctrl_group %>% mutate(Group = "Positive control"),
  neg_ctrl_group %>% mutate(Group = "Negative control")
)
comparison_data <- comparison_data %>%
  mutate(
    Group = factor(
      Group,
      levels = c(
        "BB group",
        "Positive control",
        "Negative control"
      )
    )
  )

# Kaplan Meier plot sur graft_surv_years
km_fit_graft <- survfit(Surv(graft_surv_years, graft_event) ~ Group, data = comparison_data)
km_plot_graft <- ggsurvplot(
  km_fit_graft,
  data = comparison_data,
  surv.median.line = "hv",
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Time in years",
  ylab = "Graft survival probability",
  title = "Kaplan-Meier Graft Survival Curve by Group",
  legend.title = "Group",
  legend.labs = c(
    "BB group",
    "Positive control",
    "Negative control"
  ),
  palette = c(
    "#2e15d1",
    "#15D12E",
    "#d12e15"
  )
)
# Affichage du graphique
print(km_plot_graft)
# Sauvegarde du graphique
ggsave("km_graft_survival.png", plot = km_plot_graft$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table.png", plot = km_plot_graft$table, width = 8, height = 4)

#----- Survie greffon BB vs negative control -----
#Courbe de Kaplan Meier comparant uniquement le groupe BB et le groupe contrôle négatif
bb_neg_ctrl_data_graft <- bind_rows(
  BB_group       %>% mutate(Group = "BB group"),
  neg_ctrl_group %>% mutate(Group = "Negative control")
)
bb_neg_ctrl_data_graft <- bb_neg_ctrl_data_graft %>%
  mutate(
    Group = factor(
      Group,
      levels = c(
        "BB group",
        "Negative control"
      )
    )
  )
#Kaplan Meier plot sur graft_surv_years
km_fit_bb_neg_graft <- survfit(Surv(graft_surv_years, graft event) ~ Group, data = bb_neg_ctrl_data_graft)
km_plot_bb_neg_graft <- ggsurvplot(
  km_fit_bb_neg_graft,
  data = bb_neg_ctrl_data_graft,
  surv.median.line = "hv",
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Time in years",
  ylab = "Graft survival probability",
  title = "Kaplan-Meier Graft Survival Curve: BB group vs Negative control",
  legend.title = "Group",
  legend.labs = c(
    "BB group",
    "Negative control"
  ),
  palette = c(
    "#2e15d1",
    "#d12e15"
  )
)
# Affichage du graphique
print(km_plot_bb_neg_graft)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_vs_neg_control.png", plot = km_plot_bb_neg_graft$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_bb_vs_neg_control.png", plot = km_plot_bb_neg_graft$table, width = 8, height = 4)

#Comparaison BB group vs negative control à partir de 5 ans
bb_neg_ctrl_data_graft_5yrs <- bb_neg_ctrl_data_graft %>%
  filter(graft_surv_years >= 5) %>%
  mutate(time_since_5 = graft_surv_years - 5)

#Kaplan Meier plot sur graft_surv_years à partir de 5 ans
km_fit_bb_neg_graft_5yrs <- survfit(
  Surv(time_since_5, graft_event) ~ Group,
  data = bb_neg_ctrl_data_graft_5yrs
)
km_plot_bb_neg_graft_5yrs <- ggsurvplot(
  km_fit_bb_neg_graft_5yrs,
  data = bb_neg_ctrl_data_graft_5yrs,
  surv.median.line = "hv",
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Time since 5 years (years)",
  ylab = "Graft survival probability",
  title = "Kaplan–Meier Graft survival from 5 years: BB group vs Negative control",
  legend.title = "Group",
  legend.labs = c("BB group", "Negative control"),
  palette = c("#2e15d1", "#d12e15")
)
# Affichage du graphique
print(km_plot_bb_neg_graft_5yrs)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_vs_neg_control_5yrs.png", plot = km_plot_bb_neg_graft_5yrs$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_bb_vs_neg_control_10yrs.png", plot = km_plot_bb_neg_graft_5yrs$table, width = 8, height = 4)

#----- Survie greffon BB group vs positive control -----
#Courbe de Kaplan Meier comparant uniquement le groupe BB et le groupe contrôle positif
bb_pos_ctrl_data_graft <- bind_rows(
  BB_group       %>% mutate(Group = "BB group"),
  pos_ctrl_group %>% mutate(Group = "Positive control")
)
bb_pos_ctrl_data_graft <- bb_pos_ctrl_data_graft %>%
  mutate(
    Group = factor(
      Group,
      levels = c(
        "BB group",
        "Positive control"
      )
    )
  )
#Kaplan Meier plot sur graft_surv_years
km_fit_bb_pos_graft <- survfit(Surv(graft_surv_years, graft_event) ~ Group, data = bb_pos_ctrl_data_graft)
km_plot_bb_pos_graft <- ggsurvplot(
  km_fit_bb_pos_graft,
  data = bb_pos_ctrl_data_graft,
  surv.median.line = "hv",
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Time in years",
  ylab = "Graft survival probability",
  title = "Kaplan-Meier Graft Survival Curve: BB group vs Positive control",
  legend.title = "Group",
  legend.labs = c(
    "BB group",
    "Positive control"
  ),
  palette = c(
    "#2e15d1",
    "#15D12E"
  )
)
# Affichage du graphique
print(km_plot_bb_pos_graft)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_vs_pos_control.png", plot = km_plot_bb_pos_graft$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_bb_vs_pos_control.png", plot = km_plot_bb_pos_graft$table, width = 8, height = 4)

#Comparaison BB group vs positive control à partir de 5 ans
bb_pos_ctrl_data_graft_5yrs <- bb_pos_ctrl_data_graft %>%
  filter(graft_surv_years >= 5) %>%
  mutate(time_since_5 = graft_surv_years - 5)

#Kaplan Meier plot sur graft_surv_years à partir de 5 ans
km_fit_bb_pos_graft_5yrs <- survfit(
  Surv(time_since_5, graft_event) ~ Group,
  data = bb_pos_ctrl_data_graft_5yrs
)
km_plot_bb_pos_graft_5yrs <- ggsurvplot(
  km_fit_bb_pos_graft_5yrs,
  data = bb_pos_ctrl_data_graft_5yrs,
  surv.median.line = "hv",
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = "Time since 5 years (years)",
  ylab = "Graft survival probability",
  title = "Kaplan–Meier Graft survival from 5 years: BB group vs Positive control",
  legend.title = "Group",
  legend.labs = c("BB group", "Positive control"),
  palette = c("#2e15d1", "#15D12E")
)
# Affichage du graphique
print(km_plot_bb_pos_graft_5yrs)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_vs_pos_control_5yrs.png", plot = km_plot_bb_pos_graft_5yrs$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_bb_vs_pos_control_5yrs.png", plot = km_plot_bb_pos_graft_5yrs$table, width = 8, height = 4)


#-----Barchart probabilité de survie des greffons dans les 3 groupes à 5 ans-----
km_fit_graft_surv <- survfit(
  Surv(graft_surv_years, graft_event) ~ Group,
  data = comparison_data
)

s5 <- summary(km_fit_graft_surv, times = 5)

graft_survival_5yrs_df <- data.frame(
  Group = gsub("Group=", "", s5$strata),
  Survival_Probability_5yrs = s5$surv,
  lower = s5$lower,
  upper = s5$upper
)

# Optionnel mais recommandé
graft_survival_5yrs_df$Group <- factor(
  graft_survival_5yrs_df$Group,
  levels = c("BB group", "Positive control", "Negative control")
)

logrank <- survdiff(
  Surv(graft_surv_years, graft_event) ~ Group,
  data = comparison_data
)

pval <- pchisq(logrank$chisq,
               df = length(logrank$n) - 1,
               lower.tail = FALSE)

# Création du barchart avec ggplot2
ggplot(
  graft_survival_5yrs_df,
  aes(x = Group, y = Survival_Probability_5yrs, fill = Group)
) +
  geom_col(width = 0.6) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15
  ) +
  scale_fill_manual(
    values = c(
      "BB group" = "#2e15d1",
      "Positive control" = "#15D12E",
      "Negative control" = "#d12e15"
    )
  ) +
  annotate(
    "text",
    x = 2,
    y = max(graft_survival_5yrs_df$upper) + 0.05,
    label = paste0("Log-rank p = ", signif(pval, 10))
  ) +
  labs(
    x = "Group",
    y = "Graft survival probability at 5 years",
    title = "Graft survival probability at 5 years"
  ) +
  theme_minimal()

# Sauvegarde du graphique
ggsave("graft_survival_5yrs_barchart.png", width = 8, height = 6)

