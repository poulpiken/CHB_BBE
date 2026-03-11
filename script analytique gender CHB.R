#Script analytique genre
#----- Etude survie des greffons selon le genre et le mismatch entre donneur et receveur -----

#----- Groupe BB -----
BB_group <- BB_group |>
  mutate(
    gender_group = case_when(
      don_gender == "Men" & rec_gender == "Men" ~ "MM match",
      don_gender == "Women" & rec_gender == "Women" ~ "FF match",
      don_gender == "Women" & rec_gender == "Men" ~ "F donor → M recipient",
      don_gender == "Men" & rec_gender == "Women" ~ "M donor → F recipient"
    ))

#courbe de survie du greffon Kaplan Meier pour les 4 catégories de gender_group
km_fit_bb_gender <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group, data = BB_group)
km_plot_bb_gender <- ggsurvplot(
  km_fit_bb_gender,
  data = BB_group,
  pval = TRUE,
  conf.int = ,
  risk.table = TRUE,
  legend.title = "Group",
  legend.labs = c("MM match", "FF match", "F donor → M recipient", "M donor → F recipient"),
  xlab = "Time in years",
  ylab = "Graft survival probability in BB group by donor and recipient gender",
  title = "Kaplan-Meier curve of graft survival in BB group by donor and recipient gender",
  palette = c(
    "#4EC26D",
    "#4E69C2",
    "#C24EA3",
    "#C2844E"
  ),
  risk.table.height = 0.3,  # plus d'espace pour la table
  risk.table.fontsize = 4
)

#Affichage du graphique
print(km_plot_bb_gender)
#Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_gender.png", plot = km_plot_bb_gender$plot, width = 8, height = 6, dpi = 300)
#Sauvegarde du tableau des risques
ggsave("km_graft_survival_bb_gender_risk_table.png", plot = km_plot_bb_gender$table, width = 8, height = 4, dpi = 300)


#Comparaison des 2 groupes mismatch F donor → M recipient et M donor → F recipient
BB_group_mismatch <- BB_group |>
  filter(gender_group %in% c("F donor → M recipient", "M donor → F recipient"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group, data = BB_group_mismatch, p.adjust.method = "BH")


km_fit_bb_mismatch <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group, data = BB_group_mismatch)
km_plot_bb_mismatch <- ggsurvplot(
  km_fit_bb_mismatch,
  data = BB_group_mismatch,
  pval = TRUE,
  conf.int = ,
  risk.table = TRUE,
  legend.title = "Group",
  legend.labs = c("F donor → M recipient", "M donor → F recipient"),
  xlab = "Time in years",
  ylab = "Graft survival probability in BB group by donor and recipient gender",
  title = "Kaplan-Meier curve of graft survival in BB group with gender mismatch",
  palette = c(
    "#C24EA3",
    "#C2844E"
  ))

#Affichage du graphique
print(km_plot_bb_mismatch)
#Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_mismatch.png", plot = km_plot_bb_mismatch$plot, width = 8, height = 6, dpi = 300)
#Sauvegarde du tableau des risques
ggsave("km_graft_survival_bb_mismatch_risk_table.png", plot = km_plot_bb_mismatch$table, width = 8, height = 4, dpi = 300)



#Comparaison des 2 groupes match MM et FF
BB_group_match <- BB_group |>
  filter(gender_group %in% c("FF match", "MM match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group, data = BB_group_match, p.adjust.method = "BH")


km_fit_bb_match <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group, data = BB_group_match)
km_plot_bb_match <- ggsurvplot(
  km_fit_bb_match,
  data = BB_group_match,
  pval = TRUE,
  conf.int = ,
  risk.table = TRUE,
  legend.title = "Group",
  legend.labs = c("FF match", "MM match"),
  xlab = "Time in years",
  ylab = "Graft survival probability in BB group by donor and recipient gender",
  title = "Kaplan-Meier curve of graft survival in BB group with gender match",
  palette = c(
    "#4E69C2",
    "#4EC26D"
  ))

#Affichage du graphique
print(km_plot_bb_match)
#Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_match.png", plot = km_plot_bb_match$plot, width = 8, height = 6, dpi = 300)
#Sauvegarde du tableau des risques
ggsave("km_graft_survival_bb_match_risk_table.png", plot = km_plot_bb_match$table, width = 8, height = 4, dpi = 300)



#Comparaison entre Mismatch F donor → M recipient vs Match MM
bb_mismatch_FM_match_M_data <- BB_group %>%
  filter(gender_group %in% c("F donor → M recipient", "MM match"))
pairwise_survdiff(  Surv(graft_surv_years, graft_event) ~ gender_group,  data = bb_mismatch_FM_match_M_data,  
                    p.adjust.method = "BH")
# Le test de log-rank indique une différence statistiquement significative entre les deux groupes ( p = 0.01).
#Graphique Kaplan Meier des 2 groupes mismatch F donor → M recipient vs Match MM
km_fit_bb_mismatch_FM_match_M <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                         data = bb_mismatch_FM_match_M_data)
km_plot_bb_mismatch_FM_match_M <- ggsurvplot(
  km_fit_bb_mismatch_FM_match_M,
  data = bb_mismatch_FM_match_M_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in BB group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for BB group: F donor → M recipient vs MM match",
  legend.title = "Group",
  legend.labs = c("F donor → M recipient", "MM match"),
  palette = c("#C24EA3", "#4EC26D")
)
# Affichage du graphique
print(km_plot_bb_mismatch_FM_match_M)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_mismatch_FM_match_M.png", plot = km_plot_bb_mismatch_FM_match_M$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_bb_mismatch_FM_match_M.png", plot = km_plot_bb_mismatch_FM_match_M$table, width = 8, height = 4)
 


#Comparaison entre Mismatch F donor → M recipient vs Match FF
bb_mismatch_FM_match_F_data <- BB_group %>%
  filter(gender_group %in% c("F donor → M recipient", "FF match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = bb_mismatch_FM_match_F_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch F donor → M recipient vs Match FF
km_fit_bb_mismatch_FM_match_F <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                         data = bb_mismatch_FM_match_F_data)
km_plot_bb_mismatch_FM_match_F <- ggsurvplot(
  km_fit_bb_mismatch_FM_match_F,
  data = bb_mismatch_FM_match_F_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in BB group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for BB group: F donor → M recipient vs FF match",
  legend.title = "Group",
  legend.labs = c("F donor → M recipient", "FF match"),
  palette = c("#C24EA3", "#4E69C2")
)
# Affichage du graphique
print(km_plot_bb_mismatch_FM_match_F)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_mismatch_FM_match_F.png", plot = km_plot_bb_mismatch_FM_match_F$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_bb_mismatch_FM_match_F.png", plot = km_plot_bb_mismatch_FM_match_F$table, width = 8, height = 4)



#Comparaison entre Mismatch M donor → F recipient vs Match MM
bb_mismatch_MF_match_M_data <- BB_group %>%
  filter(gender_group %in% c("M donor → F recipient", "MM match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = bb_mismatch_MF_match_M_data,  
                  p.adjust.method = "BH")
# Le test de log-rank indique 

#Graphique Kaplan Meier des 2 groupes mismatch M donor → F recipient vs Match MM
km_fit_bb_mismatch_MF_match_M <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                         data = bb_mismatch_MF_match_M_data)
km_plot_bb_mismatch_MF_match_M <- ggsurvplot(
  km_fit_bb_mismatch_MF_match_M,
  data = bb_mismatch_MF_match_M_data,
  risk.table = TRUE,
  pval = ,
  conf.int = TRUE,
  xlab = "Time in years",
  ylab = "Graft survival probability in BB group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for BB group: M donor → F recipient vs MM match",
  legend.title = "Group",
  legend.labs = c("M donor → F recipient", "MM match"),
  palette = c("#C2844E", "#4EC26D")
)
# Affichage du graphique
print(km_plot_bb_mismatch_MF_match_M)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_mismatch_MF_match_M.png", plot = km_plot_bb_mismatch_MF_match_M$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_bb_mismatch_MF_match_M.png", plot = km_plot_bb_mismatch_MF_match_M$table, width = 8, height = 4)



#Comparaison entre Mismatch M donor → F recipient vs Match FF
bb_mismatch_MF_match_F_data <- BB_group %>%
  filter(gender_group %in% c("M donor → F recipient", "FF match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = bb_mismatch_MF_match_F_data,  
                  p.adjust.method = "BH")
# Le test de log-rank indique 

#Graphique Kaplan Meier des 2 groupes mismatch M donor → F recipient vs Match FF
km_fit_bb_mismatch_MF_match_F <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                         data = bb_mismatch_MF_match_F_data)
km_plot_bb_mismatch_MF_match_F <- ggsurvplot(
  km_fit_bb_mismatch_MF_match_F,
  data = bb_mismatch_MF_match_F_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in BB group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for BB group: M donor → F recipient vs FF match",
  legend.title = "Group",
  legend.labs = c("M donor → F recipient", "FF match"),
  palette = c("#C2844E", "#4E69C2")
)
# Affichage du graphique
print(km_plot_bb_mismatch_MF_match_F)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_bb_mismatch_MF_match_F.png", plot = km_plot_bb_mismatch_MF_match_F$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_bb_mismatch_MF_match_F.png", plot = km_plot_bb_mismatch_MF_match_F$table, width = 8, height = 4)




#----- Groupe positive control-----
pos_ctrl_group <- pos_ctrl_group |>
  mutate(
    gender_group = case_when(
      don_gender == "Men" & rec_gender == "Men" ~ "MM match",
      don_gender == "Women" & rec_gender == "Women" ~ "FF match",
      don_gender == "Women" & rec_gender == "Men" ~ "F donor → M recipient",
      don_gender == "Men" & rec_gender == "Women" ~ "M donor → F recipient"
    ))


#Courbe de survie du greffon Kaplan Meier pour les 4 groupes de gender_group
km_fit_pos_gender <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_ctrl_group)
km_plot_pos_gender <- ggsurvplot(
  km_fit_pos_gender,
  data = pos_ctrl_group,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the positive control group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the positive control group by gender",
  legend.title = "Group",
  legend.labs = c(
    "MM Match",
    "FF Match",
    "F donor → M recipient",
    "M donor → F recipient"
  ),
  palette = c(
    "#4EC26D",
    "#4E69C2",
    "#C24EA3",
    "#C2844E"),
  linewidth = 1.2,                # lignes plus épaisses
  censor.size = 2.5,
  break.time.by = 5,         # graduations plus régulières
  risk.table.height = 0.3,  # plus d'espace pour la table
  risk.table.fontsize = 4
)

# Affichage du graphique Affichage du graphique
print(km_plot_pos_gender)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_pos_gender.png", plot = km_plot_pos_gender$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_pos_gender_risk_table.png", plot = km_plot_pos_gender$table, width = 8, height = 4)




# Comparaison des 2 groupes mismatch F donor → M recipient vs M donor → F recipient
pos_mismatch_data <- pos_ctrl_group %>%
  filter(gender_group %in% c("F donor → M recipient", "M donor → F recipient"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_mismatch_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch
km_fit_pos_mismatch <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_mismatch_data)
km_plot_pos_mismatch <- ggsurvplot(
  km_fit_pos_mismatch,
  data = pos_mismatch_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the positive control 
  group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the positive control group: 
          F donor → M recipient vs M donor → F recipient",
  legend.title = "Group",
  legend.labs = c("F donor → M recipient",  "M donor → F recipient"),
  palette = c("#C24EA3","#C2844E"),
  linewidth = 1.2,
  censor.size = 2.5,
  break.time.by = 5
)

# Affichage du graphique
print(km_plot_pos_mismatch)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_pos_mismatch.png", plot = km_plot_pos_mismatch$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_pos_mismatch.png", plot = km_plot_pos_mismatch$table, width = 8, height = 4)



#Comparaison des 2 groupes match MM match vs FF match 
pos_match_data <- pos_ctrl_group %>%
  filter(gender_group %in% c("MM match", "FF match"))
pairwise_survdiff(  Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_match_data,  
                    p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes match
km_fit_pos_match <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_match_data)
km_plot_pos_match <- ggsurvplot(
  km_fit_pos_match,
  data = pos_match_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "    Graft survival probability in the positive control 
  group by donor and recipient gender match",
  title = "Kaplan-Meier Graft Survival Curve for the positive control group: 
                                MM match vs FF match",
  legend.title = "Group",
  legend.labs = c("MM match", "FF match"),
  palette = c(  "#4EC26D", "#4E69C2"))

# Affichage du graphique
print(km_plot_pos_match)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_pos_match.png", plot = km_plot_pos_match$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_pos_match.png", plot = km_plot_pos_match$table, width = 8, height = 4)



#Comparaison entre Mismatch F donor → M recipient vs Match MM
pos_mismatch_FM_match_M_data <- pos_ctrl_group %>%
  filter(gender_group %in% c("F donor → M recipient", "MM match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_mismatch_FM_match_M_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch F donor → M recipient vs Match MM
km_fit_pos_mismatch_FM_match_M <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                          data = pos_mismatch_FM_match_M_data)
km_plot_pos_mismatch_FM_match_M <- ggsurvplot(
  km_fit_pos_mismatch_FM_match_M,
  data = pos_mismatch_FM_match_M_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the positive 
  control group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the positive control group: 
            F donor → M recipient vs MM match",
  legend.title = "Group",
  legend.labs = c("F donor → M recipient", "MM match"),
  palette = c("#C24EA3", "#4EC26D")
)
# Affichage du graphique
print(km_plot_pos_mismatch_FM_match_M)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_pos_mismatch_FM_match_M.png", plot = km_plot_pos_mismatch_FM_match_M$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_pos_mismatch_FM_match_M.png", plot = km_plot_pos_mismatch_FM_match_M$table, width = 8, height = 4)


#Comparaison entre Mismatch F donor → M recipient vs Match FF
pos_mismatch_FM_match_F_data <- pos_ctrl_group %>%
  filter(gender_group %in% c("F donor → M recipient", "FF match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_mismatch_FM_match_F_data,  
                  p.adjust.method = "BH")


#Graphique Kaplan Meier des 2 groupes mismatch F donor → M recipient vs Match FF
km_fit_pos_mismatch_FM_match_F <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                          data = pos_mismatch_FM_match_F_data)
km_plot_pos_mismatch_FM_match_F <- ggsurvplot(
  km_fit_pos_mismatch_FM_match_F,
  data = pos_mismatch_FM_match_F_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the positive control 
  group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the positive control group: 
                    F donor → M recipient vs FF match",
  legend.title = "Group",
  legend.labs = c("F donor → M recipient", "FF match"),
  palette = c("#C24EA3", "#4E69C2")
)
# Affichage du graphique
print(km_plot_pos_mismatch_FM_match_F)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_pos_mismatch_FM_match_F.png", plot = km_plot_pos_mismatch_FM_match_F$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_pos_mismatch_FM_match_F.png", plot = km_plot_pos_mismatch_FM_match_F$table, width = 8, height = 4)


#Comparaison entre Mismatch M donor → F recipient vs Match MM
pos_mismatch_MF_match_M_data <- pos_ctrl_group %>%
  filter(gender_group %in% c("M donor → F recipient", "MM match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_mismatch_MF_match_M_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch M donor → F recipient vs Match MM
km_fit_pos_mismatch_MF_match_M <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                          data = pos_mismatch_MF_match_M_data)
km_plot_pos_mismatch_MF_match_M <- ggsurvplot(
  km_fit_pos_mismatch_MF_match_M,
  data = pos_mismatch_MF_match_M_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the positive control 
  group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the positive control group: 
                   M donor → F recipient vs MM match",
  legend.title = "Group",
  legend.labs = c("M donor → F recipient", "MM match"),
  palette = c("#C2844E", "#4EC26D")
)
# Affichage du graphique
print(km_plot_pos_mismatch_MF_match_M)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_pos_mismatch_MF_match_M.png", plot = km_plot_pos_mismatch_MF_match_M$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_pos_mismatch_MF_match_M.png", plot = km_plot_pos_mismatch_MF_match_M$table, width = 8, height = 4)


#Comparaison entre Mismatch M donor → F recipient vs Match FF
pos_mismatch_MF_match_F_data <- pos_ctrl_group %>%
  filter(gender_group %in% c("M donor → F recipient", "FF match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = pos_mismatch_MF_match_F_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch M donor → F recipient vs Match FF
km_fit_pos_mismatch_MF_match_F <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                          data = pos_mismatch_MF_match_F_data)
km_plot_pos_mismatch_MF_match_F <- ggsurvplot(
  km_fit_pos_mismatch_MF_match_F,
  data = pos_mismatch_MF_match_F_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the positive control group 
  by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the positive control group: 
                      M donor → F recipient vs FF match",
  legend.title = "Group",
  legend.labs = c("M donor → F recipient", "FF match"),
  palette = c("#C2844E", "#4E69C2")
)
# Affichage du graphique
print(km_plot_pos_mismatch_MF_match_F)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_pos_mismatch_MF_match_F.png", plot = km_plot_pos_mismatch_MF_match_F$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_pos_mismatch_MF_match_F.png", plot = km_plot_pos_mismatch_MF_match_F$table, width = 8, height = 4)


#----- Groupe negative control -----
#Création variable gender_group qui rassemble les 4 variables de match et mismatch de genre
neg_ctrl_group <- neg_ctrl_group |>
  mutate(
    gender_group = case_when(
      don_gender == "Men" & rec_gender == "Men" ~ "MM match",
      don_gender == "Women" & rec_gender == "Women" ~ "FF match",
      don_gender == "Women" & rec_gender == "Men" ~ "F donor → M recipient",
      don_gender == "Men" & rec_gender == "Women" ~ "M donor → F recipient"
    ))

#Courbe de survie du greffon Kaplan Meier pour les 4 groupes de gender_group
km_fit_neg_gender <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_ctrl_group)
km_plot_neg_gender <- ggsurvplot(
  km_fit_neg_gender,
  data = neg_ctrl_group,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the negative control 
  group by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve 
  for the negative control group by gender",
  legend.title = "Group",
  legend.labs = c(
    "MM Match",
    "FF Match",
    "F donor → M recipient",
    "M donor → F recipient"
  ),
  palette = c(
    "#4EC26D",
    "#4E69C2",
    "#C24EA3",
    "#C2844E"),
  linewidth = 1.2,                # lignes plus épaisses
  censor.size = 2.5,
  break.time.by = 5,         # graduations plus régulières
  risk.table.height = 0.3,  # plus d'espace pour la table
  risk.table.fontsize = 4
)

# Affichage du graphique Affichage du graphique
print(km_plot_neg_gender)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_neg_gender.png", plot = km_plot_neg_gender$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_neg_gender_risk_table.png", plot = km_plot_neg_gender$table, width = 8, height = 4)



# Comparaison des 2 groupes mismatch F donor → M recipient vs M donor → F recipient
neg_mismatch_data <- neg_ctrl_group %>%
  filter(gender_group %in% c("F donor → M recipient", "M donor → F recipient"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_mismatch_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch
km_fit_neg_mismatch <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_mismatch_data)
km_plot_neg_mismatch <- ggsurvplot(
  km_fit_neg_mismatch,
  data = neg_mismatch_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the negative control group 
  by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the negative control group: 
  F donor → M recipient vs M donor → F recipient",
  legend.title = "Group",
  legend.labs = c("F donor → M recipient",  "M donor → F recipient"),
  palette = c("#C24EA3","#C2844E"),
  linewidth = 1.2,
  censor.size = 2.5,
  break.time.by = 5
)

# Affichage du graphique
print(km_plot_neg_mismatch)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_neg_mismatch.png", plot = km_plot_neg_mismatch$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_neg_mismatch.png", plot = km_plot_neg_mismatch$table, width = 8, height = 4)



#Comparaison des 2 groupes match MM match vs FF match 
neg_match_data <- neg_ctrl_group %>%
  filter(gender_group %in% c("MM match", "FF match"))
pairwise_survdiff(  Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_match_data,  
                    p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes match
km_fit_neg_match <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_match_data)
km_plot_neg_match <- ggsurvplot(
  km_fit_neg_match,
  data = neg_match_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the negative control group 
  by donor and recipient gender match",
  title = "Kaplan-Meier Graft Survival Curve for the negative control group: 
                             MM match vs FF match",
  legend.title = "Group",
  legend.labs = c("MM match", "FF match"),
  palette = c(  "#4EC26D", "#4E69C2"))

# Affichage du graphique
print(km_plot_neg_match)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_neg_match.png", plot = km_plot_neg_match$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_neg_match.png", plot = km_plot_neg_match$table, width = 8, height = 4)



#Comparaison entre Mismatch F donor → M recipient vs Match MM
neg_mismatch_FM_match_M_data <- neg_ctrl_group %>%
  filter(gender_group %in% c("F donor → M recipient", "MM match"))
pairwise_survdiff(  Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_mismatch_FM_match_M_data,  
                    p.adjust.method = "BH")
# Le test de log-rank indique une différence statistiquement significative entre les deux groupes ( p = 0.01).

#Graphique Kaplan Meier des 2 groupes mismatch F donor → M recipient vs Match MM
km_fit_neg_mismatch_FM_match_M <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                          data = neg_mismatch_FM_match_M_data)
km_plot_neg_mismatch_FM_match_M <- ggsurvplot(
  km_fit_neg_mismatch_FM_match_M,
  data = neg_mismatch_FM_match_M_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the negative control group 
  by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the negative control group: 
                   F donor → M recipient vs MM match",
  legend.title = "Group",
  legend.labs = c("F donor → M recipient", "MM match"),
  palette = c("#C24EA3", "#4EC26D")
)
# Affichage du graphique
print(km_plot_neg_mismatch_FM_match_M)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_neg_mismatch_FM_match_M.png", plot = km_plot_neg_mismatch_FM_match_M$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_neg_mismatch_FM_match_M.png", plot = km_plot_neg_mismatch_FM_match_M$table, width = 8, height = 4)



#Comparaison entre Mismatch F donor → M recipient vs Match FF
neg_mismatch_FM_match_F_data <- neg_ctrl_group %>%
  filter(gender_group %in% c("F donor → M recipient", "FF match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_mismatch_FM_match_F_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch F donor → M recipient vs Match FF
km_fit_neg_mismatch_FM_match_F <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                          data = neg_mismatch_FM_match_F_data)
km_plot_neg_mismatch_FM_match_F <- ggsurvplot(
  km_fit_neg_mismatch_FM_match_F,
  data = neg_mismatch_FM_match_F_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the negative control group 
  by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the negative control group: 
                       F donor → M recipient vs FF match",
  legend.title = "Group",
  legend.labs = c("F donor → M recipient", "FF match"),
  palette = c("#C24EA3", "#4E69C2")
)
# Affichage du graphique
print(km_plot_neg_mismatch_FM_match_F)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_pos_mismatch_FM_match_F.png", plot = km_plot_neg_mismatch_FM_match_F$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_pos_mismatch_FM_match_F.png", plot = km_plot_neg_mismatch_FM_match_F$table, width = 8, height = 4)


#Comparaison entre Mismatch M donor → F recipient vs Match MM
neg_mismatch_MF_match_M_data <- neg_ctrl_group %>%
  filter(gender_group %in% c("M donor → F recipient", "MM match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_mismatch_MF_match_M_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch M donor → F recipient vs Match MM
km_fit_neg_mismatch_MF_match_M <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                          data = neg_mismatch_MF_match_M_data)
km_plot_neg_mismatch_MF_match_M <- ggsurvplot(
  km_fit_neg_mismatch_MF_match_M,
  data = neg_mismatch_MF_match_M_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the negative control group 
  by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the negative control group:
                    M donor → F recipient vs MM match",
  legend.title = "Group",
  legend.labs = c("M donor → F recipient", "MM match"),
  palette = c("#C2844E", "#4EC26D")
)
# Affichage du graphique
print(km_plot_neg_mismatch_MF_match_M)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_neg_mismatch_MF_match_M.png", plot = km_plot_neg_mismatch_MF_match_M$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_neg_mismatch_MF_match_M.png", plot = km_plot_neg_mismatch_MF_match_M$table, width = 8, height = 4)


#Comparaison entre Mismatch M donor → F recipient vs Match FF
neg_mismatch_MF_match_F_data <- neg_ctrl_group %>%
  filter(gender_group %in% c("M donor → F recipient", "FF match"))
pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ gender_group,  data = neg_mismatch_MF_match_F_data,  
                  p.adjust.method = "BH")

#Graphique Kaplan Meier des 2 groupes mismatch M donor → F recipient vs Match FF
km_fit_neg_mismatch_MF_match_F <- survfit(Surv(graft_surv_years, graft_event) ~ gender_group,  
                                          data = neg_mismatch_MF_match_F_data)
km_plot_neg_mismatch_MF_match_F <- ggsurvplot(
  km_fit_neg_mismatch_MF_match_F,
  data = neg_mismatch_MF_match_F_data,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = ,
  xlab = "Time in years",
  ylab = "Graft survival probability in the negative control group 
  by donor and recipient gender",
  title = "Kaplan-Meier Graft Survival Curve for the negative control group: 
                  M donor → F recipient vs FF match",
  legend.title = "Group",
  legend.labs = c("M donor → F recipient", "FF match"),
  palette = c("#C2844E", "#4E69C2")
)
# Affichage du graphique
print(km_plot_neg_mismatch_MF_match_F)
# Sauvegarde du graphique
ggsave("km_graft_survival_plot_neg_mismatch_MF_match_F.png", plot = km_plot_neg_mismatch_MF_match_F$plot, width = 8, height = 6)
# Sauvegarde du tableau des risques
ggsave("km_graft_survival_risk_table_neg_mismatch_MF_match_F.png", plot = km_plot_neg_mismatch_MF_match_F$table, width = 8, height = 4)

      