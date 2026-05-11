#####----- Analyse de survie des greffonss et des greffons selon la cause de décès du donneur -----#####

#----- Survie greffonss -----#
#Modification de don_cod
comparison_data$don_cod <- as.factor(comparison_data$don_cod)
comparison_data$COD_label <- factor(
  comparison_data$don_cod,
  levels = c("Anoxia", "Cerebrovascular accident or stroke",
             "Head trauma", "Other"),
  labels = c("Anoxia", "AVC/Stroke", "Head trauma", "Other")
)

BB_group$don_cod <- as.factor(BB_group$don_cod)
BB_group$COD_label <- factor(
  BB_group$don_cod,
  levels = c("Anoxia", "Cerebrovascular accident or stroke",
             "Head trauma", "Other"),
  labels = c("Anoxia", "AVC/Stroke", "Head trauma", "Other")
)

pos_ctrl_group$don_cod <- as.factor(pos_ctrl_group$don_cod)
pos_ctrl_group$COD_label <- factor(
  pos_ctrl_group$don_cod,
  levels = c("Anoxia", "Cerebrovascular accident or stroke",
             "Head trauma", "Other"),
  labels = c("Anoxia", "AVC/Stroke", "Head trauma", "Other")
)

neg_ctrl_group$don_cod <- as.factor(neg_ctrl_group$don_cod)
neg_ctrl_group$COD_label <- factor(
  neg_ctrl_group$don_cod,
  levels = c("Anoxia", "Cerebrovascular accident or stroke",
             "Head trauma", "Other"),
  labels = c("Anoxia", "AVC/Stroke", "Head trauma", "Other")
)

#####Global#####
#Objet de survie
surv_obj <- Surv(time   = comparison_data$graft_surv_years,
                 event  = comparison_data$graft_event)

#Kaplan-Meier par groupe
km_fit <- survfit(Surv(graft_surv_years, graft_event) ~ COD_label, data = comparison_data)

km_plot <- ggsurvplot(km_fit,data = comparison_data,
                      # P-value
                      pval             = TRUE,
                      pval.method      = TRUE,
                      pval.size        = 4,
                      pval.coord       = c(1, 0.12),       # position manuelle en bas à gauche
                      # Intervalles de confiance : désactivés pour plus de lisibilité
                      conf.int         = FALSE,
                      # Table des greffonss à risque
                      risk.table       = TRUE,
                      risk.table.height= 0.28,             # hauteur de la table
                      risk.table.fontsize = 3.5,
                      tables.theme     = theme_cleantable(),
                      # Axe X : limité à 35 ans (peu de greffonss après)
                      xlim             = c(0, 35),
                      break.time.by    = 5,                # graduations tous les 5 ans
                      # Légende
                      legend.title     = "Cause of death",
                      legend.labs      = levels(comparison_data$COD_label),
                      legend           = c(0.75, 0.75),    # position dans le graphique (x, y)
                      # Étiquettes
                      xlab             = "Time (years)",
                      ylab             = "Survival probability",
                      title            = "Graft survival by donor cause of death",
                      # Couleurs distinctes et accessibles
                      palette          = c("#E69F00", "#CC0000", "#0072B2", "#009E73"),
                      # Taille des lignes
                      size             = 0.8,
                      
                      ggtheme          = theme_bw(base_size = 11) +
                        theme(
                          plot.title      = element_text(hjust = 0.5, face = "bold", size = 11),
                          legend.background = element_rect(fill = "white", color = "grey80"),
                          legend.key.width  = unit(1.5, "cm")
                        ))

#Affichage
print(km_plot)
#Sauvegarde du graphique et de la risk table
ggsave("km_plot_cod_global.png", plot = km_plot$plot, width = 8, height = 6)
ggsave("km_plot_cod_global_risk_table.png", plot = km_plot$table, width = 8, height = 4)


#Log-Rank test global

#Calcul
pairwise_results <- pairwise_survdiff(
  Surv(graft_surv_years, graft_event) ~ COD_label,
  data = comparison_data,
  p.adjust.method = "holm"
)

#Création de pair_df 
pair_df <- as.data.frame(as.table(pairwise_results$p.value)) %>%
  setNames(c("Groupe 1", "Groupe 2", "p_val")) %>%
  filter(!is.na(p_val)) %>%
  mutate(
    p_holm = case_when(
      p_val < 0.0001 ~ "< 0.0001",
      p_val < 0.001  ~ "< 0.001",
      p_val < 0.01   ~ formatC(p_val, digits = 3, format = "f"),
      TRUE           ~ formatC(p_val, digits = 3, format = "f")
    ),
    Significance = case_when(
      p_val < 0.0001 ~ "****",
      p_val < 0.001  ~ "***",
      p_val < 0.01   ~ "**",
      p_val < 0.05   ~ "*",
      TRUE           ~ "ns"
    )
  )

#Table affichée (sans colonne Significance)
table_data <- data.frame(
  "Group 1"        = pair_df$`Groupe 1`,
  "Group 2"        = pair_df$`Groupe 2`,
  "pval (Holm)" = pair_df$p_holm,
  check.names = FALSE
)

#Couleurs de fond
row_fills <- case_when(
  pair_df$Significance == "****" ~ "#FFCCCC",
  pair_df$Significance == "***"  ~ "#FFD6D6",
  pair_df$Significance == "**"   ~ "#FFE4CC",
  pair_df$Significance == "*"    ~ "#FFF3CC",
  TRUE                           ~ "#F5F5F5"
)

#Thème du tableau avec hauteur de lignes augmentée
tt <- ttheme_default(
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 11),
    bg_params = list(fill = "#2C3E50", col = "white"),
    padding   = unit(c(8, 6), "mm")   # ← padding horizontal, vertical header
  ),
  core = list(
    bg_params = list(fill = row_fills, col = "white"),
    fg_params = list(fontsize = 10, col = "grey10"),
    padding   = unit(c(8, 5), "mm")   # ← padding horizontal, vertical lignes
  )
)

#Assemblage
grob_table <- tableGrob(table_data, rows = NULL, theme = tt)

title <- textGrob(
  "Pairwise comparisons of survival rates by cause of death of the donor",
  gp = gpar(fontsize = 12, fontface = "bold", col = "#2C3E50")
)

footnote <- textGrob(
  "Signif. codes: **** p<0.0001 | *** p<0.001 | ** p<0.01 | * p<0.05 | ns = not significant",
  gp = gpar(fontsize = 8, col = "grey40", fontface = "italic")
)

final_table <- arrangeGrob(
  title, grob_table, footnote,
  nrow    = 3,
  heights = unit(c(1, 6, 0.7), "cm")  # ← hauteur totale augmentée
)

#Affichage
grid.newpage()
grid.draw(final_table)

#Export PNG
ggsave("pairwise_comparisons_cod.png", plot = final_table, width = 8, height = 5, dpi = 300, bg = "white")




#####Groupe BB#####

#Objet de survie
surv_obj_BB <- Surv(time   = BB_group$graft_surv_years,
                    event  = BB_group$graft_event)

#Kaplan-Meier par groupe
km_fit_BB <- survfit(Surv(graft_surv_years, graft_event) ~ COD_label, data = BB_group)

km_plot_BB <- ggsurvplot(km_fit_BB,data = BB_group,
                         # P-value
                         pval             = TRUE,
                         pval.method      = TRUE,
                         pval.size        = 4,
                         pval.coord       = c(1, 0.12),       # position manuelle en bas à gauche
                         # Intervalles de confiance : désactivés pour plus de lisibilité
                         conf.int         = FALSE,
                         # Table des greffonss à risque
                         risk.table       = TRUE,
                         risk.table.height= 0.28,             # hauteur de la table
                         risk.table.fontsize = 3.5,
                         tables.theme     = theme_cleantable(),
                         # Axe X : limité à 35 ans (peu de greffonss après)
                         xlim             = c(0, 35),
                         break.time.by    = 5,                # graduations tous les 5 ans
                         # Légende
                         legend.title     = "Cause of death",
                         legend.labs      = levels(BB_group$COD_label),
                         legend           = c(0.75, 0.75),    # position dans le graphique (x, y)
                         # Étiquettes
                         xlab             = "Time (years)",
                         ylab             = "Survival probability",
                         title            = "Graft survival by donor cause of death - BB group",
                         # Couleurs distinctes et accessibles
                         palette          = c("#E69F00", "#CC0000", "#0072B2", "#009E73"),
                         # Taille des lignes
                         size             = 0.8,
                         
                         ggtheme          = theme_bw(base_size = 11) +
                           theme(
                             plot.title      = element_text(hjust = 0.5, face = "bold", size = 11),
                             legend.background = element_rect(fill = "white", color = "grey80"),
                             legend.key.width  = unit(1.5, "cm")
                           ))

#Affichage
print(km_plot_BB)
#Sauvegarde du graphique et de la risk table
ggsave("km_plot_BB_cod_global.png", plot = km_plot_BB$plot, width = 8, height = 6)
ggsave("km_plot_BB_cod_global_risk_table.png", plot = km_plot_BB$table, width = 8, height = 4)


#Log-Rank test global

#Calcul
pairwise_results_BB <- pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ COD_label,
                                         data = BB_group, p.adjust.method = "holm")

#Création de pair_df_BB 
pair_df_BB <- as.data.frame(as.table(pairwise_results_BB$p.value)) %>%
  setNames(c("Groupe 1", "Groupe 2", "p_val")) %>%
  filter(!is.na(p_val)) %>%
  mutate(
    p_holm = case_when(
      p_val < 0.0001 ~ "< 0.0001",
      p_val < 0.001  ~ "< 0.001",
      p_val < 0.01   ~ formatC(p_val, digits = 3, format = "f"),
      TRUE           ~ formatC(p_val, digits = 3, format = "f")
    ),
    Significance = case_when(
      p_val < 0.0001 ~ "****",
      p_val < 0.001  ~ "***",
      p_val < 0.01   ~ "**",
      p_val < 0.05   ~ "*",
      TRUE           ~ "ns"
    )
  )

#Table affichée (sans colonne Significance)
table_data_BB <- data.frame(
  "Group 1"        = pair_df_BB$`Groupe 1`,
  "Group 2"        = pair_df_BB$`Groupe 2`,
  "pval (Holm)" = pair_df_BB$p_holm,
  check.names = FALSE
)

#Couleurs de fond
row_fills <- case_when(
  pair_df_BB$Significance == "****" ~ "#FFCCCC",
  pair_df_BB$Significance == "***"  ~ "#FFD6D6",
  pair_df_BB$Significance == "**"   ~ "#FFE4CC",
  pair_df_BB$Significance == "*"    ~ "#FFF3CC",
  TRUE                           ~ "#F5F5F5"
)

#Thème du tableau avec hauteur de lignes augmentée
tt_BB <- ttheme_default(
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 11),
    bg_params = list(fill = "#2C3E50", col = "white"),
    padding   = unit(c(8, 6), "mm")   # ← padding horizontal, vertical header
  ),
  core = list(
    bg_params = list(fill = row_fills, col = "white"),
    fg_params = list(fontsize = 10, col = "grey10"),
    padding   = unit(c(8, 5), "mm")   # ← padding horizontal, vertical lignes
  )
)

#Assemblage
grob_table_BB <- tableGrob(table_data_BB, rows = NULL, theme = tt_BB)

title_BB <- textGrob(
  "Pairwise comparisons of survival rates by cause of death of the donor in BB group",
  gp = gpar(fontsize = 10, fontface = "bold", col = "#2C3E50")
)

footnote_BB <- textGrob(
  "Signif. codes: **** p<0.0001 | *** p<0.001 | ** p<0.01 | * p<0.05 | ns = not significant",
  gp = gpar(fontsize = 8, col = "grey40", fontface = "italic")
)

final_table_BB <- arrangeGrob(
  title_BB, grob_table_BB, footnote_BB,
  nrow    = 3,
  heights = unit(c(1, 6, 0.7), "cm")  # ← hauteur totale augmentée
)

#Affichage
grid.newpage()
grid.draw(final_table_BB)

#Export PNG
ggsave("pairwise_comparisons_BB_cod.png", plot = final_table_BB, width = 8, height = 5, dpi = 300, bg = "white")




#####Groupe contrôle positif#####
#Objet de survie
surv_obj_pos <- Surv(time   = pos_ctrl_group$graft_surv_years,
                     event  = pos_ctrl_group$graft_event)

#Kaplan-Meier par groupe
km_fit_pos <- survfit(Surv(graft_surv_years, graft_event) ~ COD_label, data = pos_ctrl_group)

km_plot_pos <- ggsurvplot(km_fit_pos,data = pos_ctrl_group,
                          # P-value
                          pval             = TRUE,
                          pval.method      = TRUE,
                          pval.size        = 4,
                          pval.coord       = c(1, 0.12),       # position manuelle en bas à gauche
                          # Intervalles de confiance : désactivés pour plus de lisibilité
                          conf.int         = FALSE,
                          # Table des greffonss à risque
                          risk.table       = TRUE,
                          risk.table.height= 0.28,             # hauteur de la table
                          risk.table.fontsize = 3.5,
                          tables.theme     = theme_cleantable(),
                          # Axe X : limité à 35 ans (peu de greffonss après)
                          xlim             = c(0, 35),
                          break.time.by    = 5,                # graduations tous les 5 ans
                          # Légende
                          legend.title     = "Cause of death",
                          legend.labs      = levels(pos_ctrl_group$COD_label),
                          legend           = c(0.75, 0.75),    # position dans le graphique (x, y)
                          # Étiquettes
                          xlab             = "Time (years)",
                          ylab             = "Survival probability",
                          title            = "Graft survival by donor cause of death - positive control group",
                          # Couleurs distinctes et accessibles
                          palette          = c("#E69F00", "#CC0000", "#0072B2", "#009E73"),
                          # Taille des lignes
                          size             = 0.8,
                          
                          ggtheme          = theme_bw(base_size = 11) +
                            theme(
                              plot.title      = element_text(hjust = 0.5, face = "bold", size = 11),
                              legend.background = element_rect(fill = "white", color = "grey80"),
                              legend.key.width  = unit(1.5, "cm")
                            ))

#Affichage
print(km_plot_pos)
#Sauvegarde du graphique et de la risk table
ggsave("km_plot_pos_cod_global.png", plot = km_plot_pos$plot, width = 8, height = 6)
ggsave("km_plot_pos_cod_global_risk_table.png", plot = km_plot_pos$table, width = 8, height = 4)


#Log-Rank test global

#Calcul
pairwise_results_pos <- pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ COD_label,
                                          data = pos_ctrl_group, p.adjust.method = "holm")

#Création de pair_df_pos
pair_df_pos <- as.data.frame(as.table(pairwise_results_pos$p.value)) %>%
  setNames(c("Groupe 1", "Groupe 2", "p_val")) %>%
  filter(!is.na(p_val)) %>%
  mutate(
    p_holm = case_when(
      p_val < 0.0001 ~ "< 0.0001",
      p_val < 0.001  ~ "< 0.001",
      p_val < 0.01   ~ formatC(p_val, digits = 3, format = "f"),
      TRUE           ~ formatC(p_val, digits = 3, format = "f")
    ),
    Significance = case_when(
      p_val < 0.0001 ~ "****",
      p_val < 0.001  ~ "***",
      p_val < 0.01   ~ "**",
      p_val < 0.05   ~ "*",
      TRUE           ~ "ns"
    )
  )

#Table affichée (sans colonne Significance)
table_data_pos <- data.frame(
  "Group 1"        = pair_df_pos$`Groupe 1`,
  "Group 2"        = pair_df_pos$`Groupe 2`,
  "pval (Holm)" = pair_df_pos$p_holm,
  check.names = FALSE
)

#Couleurs de fond
row_fills <- case_when(
  pair_df_pos$Significance == "****" ~ "#FFCCCC",
  pair_df_pos$Significance == "***"  ~ "#FFD6D6",
  pair_df_pos$Significance == "**"   ~ "#FFE4CC",
  pair_df_pos$Significance == "*"    ~ "#FFF3CC",
  TRUE                           ~ "#F5F5F5"
)

#Thème du tableau avec hauteur de lignes augmentée
tt_pos <- ttheme_default(
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 11),
    bg_params = list(fill = "#2C3E50", col = "white"),
    padding   = unit(c(8, 6), "mm")   # ← padding horizontal, vertical header
  ),
  core = list(
    bg_params = list(fill = row_fills, col = "white"),
    fg_params = list(fontsize = 10, col = "grey10"),
    padding   = unit(c(8, 5), "mm")   # ← padding horizontal, vertical lignes
  )
)

#Assemblage
grob_table_pos <- tableGrob(table_data_pos, rows = NULL, theme = tt_pos)

title_pos <- textGrob(
  "Pairwise comparisons of survival rates by cause of death of the donor - positive control group",
  gp = gpar(fontsize = 10, fontface = "bold", col = "#2C3E50")
)

footnote_pos <- textGrob(
  "Signif. codes: **** p<0.0001 | *** p<0.001 | ** p<0.01 | * p<0.05 | ns = not significant",
  gp = gpar(fontsize = 8, col = "grey40", fontface = "italic")
)

final_table_pos <- arrangeGrob(
  title_pos, grob_table_pos, footnote_pos,
  nrow    = 3,
  heights = unit(c(1, 6, 0.7), "cm")  # ← hauteur totale augmentée
)

#Affichage
grid.newpage()
grid.draw(final_table_pos)

#Export PNG
ggsave("pairwise_comparisons_pos_cod.png", plot = final_table_pos, width = 8, height = 5, dpi = 300, bg = "white")




#####Groupe contrôle négatif#####
#Objet de survie
surv_obj_neg <- Surv(time   = neg_ctrl_group$graft_surv_years,
                     event  = neg_ctrl_group$graft_event)

#Kaplan-Meier par groupe
km_fit_neg <- survfit(Surv(graft_surv_years, graft_event) ~ COD_label, data = neg_ctrl_group)

km_plot_neg <- ggsurvplot(km_fit_neg,data = neg_ctrl_group,
                          # P-value
                          pval             = TRUE,
                          pval.method      = TRUE,
                          pval.size        = 4,
                          pval.coord       = c(1, 0.12),       # position manuelle en bas à gauche
                          # Intervalles de confiance : désactivés pour plus de lisibilité
                          conf.int         = FALSE,
                          # Table des greffonss à risque
                          risk.table       = TRUE,
                          risk.table.height= 0.28,             # hauteur de la table
                          risk.table.fontsize = 3.5,
                          tables.theme     = theme_cleantable(),
                          # Axe X : limité à 35 ans (peu de greffonss après)
                          xlim             = c(0, 30),
                          break.time.by    = 5,                # graduations tous les 5 ans
                          # Légende
                          legend.title     = "Cause of death",
                          legend.labs      = levels(neg_ctrl_group$COD_label),
                          legend           = c(0.75, 0.75),    # position dans le graphique (x, y)
                          # Étiquettes
                          xlab             = "Time (years)",
                          ylab             = "Survival probability",
                          title            = "Graft survival by donor cause of death - negative control group",
                          # Couleurs distinctes et accessibles
                          palette          = c("#E69F00", "#CC0000", "#0072B2", "#009E73"),
                          # Taille des lignes
                          size             = 0.8,
                          
                          ggtheme          = theme_bw(base_size = 11) +
                            theme(
                              plot.title      = element_text(hjust = 0.5, face = "bold", size = 11),
                              legend.background = element_rect(fill = "white", color = "grey80"),
                              legend.key.width  = unit(1.5, "cm")
                            ))

#Affichage
print(km_plot_neg)
#Sauvegarde du graphique et de la risk table
ggsave("km_plot_neg_cod_global.png", plot = km_plot_neg$plot, width = 8, height = 6)
ggsave("km_plot_neg_cod_global_risk_table.png", plot = km_plot_neg$table, width = 8, height = 4)


#Log-Rank test global

#Calcul
pairwise_results_neg <- pairwise_survdiff(Surv(graft_surv_years, graft_event) ~ COD_label,
                                          data = neg_ctrl_group, p.adjust.method = "holm")

#Création de pair_df_neg
pair_df_neg <- as.data.frame(as.table(pairwise_results_neg$p.value)) %>%
  setNames(c("Groupe 1", "Groupe 2", "p_val")) %>%
  filter(!is.na(p_val)) %>%
  mutate(
    p_holm = case_when(
      p_val < 0.0001 ~ "< 0.0001",
      p_val < 0.001  ~ "< 0.001",
      p_val < 0.01   ~ formatC(p_val, digits = 3, format = "f"),
      TRUE           ~ formatC(p_val, digits = 3, format = "f")
    ),
    Significance = case_when(
      p_val < 0.0001 ~ "****",
      p_val < 0.001  ~ "***",
      p_val < 0.01   ~ "**",
      p_val < 0.05   ~ "*",
      TRUE           ~ "ns"
    )
  )

#Table affichée (sans colonne Significance)
table_data_neg <- data.frame(
  "Group 1"        = pair_df_neg$`Groupe 1`,
  "Group 2"        = pair_df_neg$`Groupe 2`,
  "pval (Holm)" = pair_df_neg$p_holm,
  check.names = FALSE
)

#Couleurs de fond
row_fills <- case_when(
  pair_df_neg$Significance == "****" ~ "#FFCCCC",
  pair_df_neg$Significance == "***"  ~ "#FFD6D6",
  pair_df_neg$Significance == "**"   ~ "#FFE4CC",
  pair_df_neg$Significance == "*"    ~ "#FFF3CC",
  TRUE                           ~ "#F5F5F5"
)

#Thème du tableau avec hauteur de lignes augmentée
tt_neg <- ttheme_default(
  colhead = list(
    fg_params = list(col = "white", fontface = "bold", fontsize = 11),
    bg_params = list(fill = "#2C3E50", col = "white"),
    padding   = unit(c(8, 6), "mm")   # ← padding horizontal, vertical header
  ),
  core = list(
    bg_params = list(fill = row_fills, col = "white"),
    fg_params = list(fontsize = 10, col = "grey10"),
    padding   = unit(c(8, 5), "mm")   # ← padding horizontal, vertical lignes
  )
)

#Assemblage
grob_table_neg <- tableGrob(table_data_neg, rows = NULL, theme = tt_neg)

title_neg <- textGrob(
  "Pairwise comparisons of survival rates by cause of death of the donor - negative control group",
  gp = gpar(fontsize = 10, fontface = "bold", col = "#2C3E50")
)

footnote_neg <- textGrob(
  "Signif. codes: **** p<0.0001 | *** p<0.001 | ** p<0.01 | * p<0.05 | ns = not significant",
  gp = gpar(fontsize = 8, col = "grey40", fontface = "italic")
)

final_table_neg <- arrangeGrob(
  title_neg, grob_table_neg, footnote_neg,
  nrow    = 3,
  heights = unit(c(1, 6, 0.7), "cm")  # ← hauteur totale augmentée
)

#Affichage
grid.newpage()
grid.draw(final_table_neg)

#Export PNG
ggsave("pairwise_comparisons_neg_cod.png", plot = final_table_neg, width = 8, height = 5, dpi = 300, bg = "white")
