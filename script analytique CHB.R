#----- Script analytique -----
# Ce script identifie les trois groupes d'intérêt BB, contrôle positif et contrôle négatif et les compare


#----- Etude survie greffons selon les 3 groupes -----

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
km_fit_bb_neg_graft <- survfit(Surv(graft_surv_years, graft_event) ~ Group, data = bb_neg_ctrl_data_graft)
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
  Group                     = gsub("Group=", "", s5$strata),
  Survival_Probability_5yrs = s5$surv,
  lower                     = s5$lower,
  upper                     = s5$upper
)

graft_survival_5yrs_df$Group <- factor(
  graft_survival_5yrs_df$Group,
  levels = c("BB group", "Positive control", "Negative control")
)

# ---- Log-rank global ----
logrank <- survdiff(
  Surv(graft_surv_years, graft_event) ~ Group,
  data = comparison_data
)
pval_global <- pchisq(logrank$chisq,
                      df = length(logrank$n) - 1,
                      lower.tail = FALSE)

format_pval <- function(p) {
  if (p < 0.0001) return("p < 0.0001")
  if (p < 0.001)  return("p < 0.001")
  if (p < 0.01)   return("p < 0.01")
  if (p < 0.05)   return("p < 0.05")
  return(paste0("p = ", round(p, 3)))
}

pval_label <- paste0("Log-rank ", format_pval(pval_global))

# ---- Comparaisons pairwise ----
pw <- pairwise_survdiff(
  Surv(graft_surv_years, graft_event) ~ Group,
  data            = comparison_data,
  p.adjust.method = "bonferroni"
)
pw_matrix <- pw$p.value

get_pw_pval <- function(mat, g1, g2) {
  v <- tryCatch(mat[g2, g1], error = function(e) NA)
  if (is.na(v)) v <- tryCatch(mat[g1, g2], error = function(e) NA)
  v
}

p_bb_pos  <- get_pw_pval(pw_matrix, "BB group", "Positive control")
p_bb_neg  <- get_pw_pval(pw_matrix, "BB group", "Negative control")
p_pos_neg <- get_pw_pval(pw_matrix, "Positive control", "Negative control")

label_pval_only <- function(p) {
  if (is.na(p) || p >= 0.05) return(NULL)
  if (p < 0.0001) return("p < 0.0001")
  if (p < 0.001)  return("p < 0.001")
  if (p < 0.01)   return("p < 0.01")
  return("p < 0.05")
}

label_bb_pos  <- label_pval_only(p_bb_pos)
label_bb_neg  <- label_pval_only(p_bb_neg)
label_pos_neg <- label_pval_only(p_pos_neg)

# ---- Labels axe X avec % en dessous ----
pct_df <- graft_survival_5yrs_df %>%
  arrange(Group) %>%
  mutate(axis_label = paste0(as.character(Group), "\n",
                             scales::percent(Survival_Probability_5yrs, accuracy = 0.1)))

label_map <- setNames(pct_df$axis_label, as.character(pct_df$Group))

graft_survival_5yrs_df$Group_label <- factor(
  label_map[as.character(graft_survival_5yrs_df$Group)],
  levels = label_map[levels(graft_survival_5yrs_df$Group)]
)

# ---- Couleurs ----
group_colors <- c(
  "BB group"         = "#2166AC",
  "Positive control" = "#4DAC26",
  "Negative control" = "#D01C1C"
)
names(group_colors) <- label_map[names(group_colors)]

# ---- Graphique de base ----
p <- ggplot(
  graft_survival_5yrs_df,
  aes(x = Group_label, y = Survival_Probability_5yrs, fill = Group_label)
) +
  geom_col(width = 0.50, color = "white", linewidth = 0.4) +
  
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width     = 0.10,
    linewidth = 0.75,
    color     = "grey25"
  ) +
  
  scale_fill_manual(values = group_colors) +
  
  scale_y_continuous(
    limits = c(0, 1.35),                        # espace suffisant tout en haut
    breaks = seq(0, 1, by = 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  
  # P-value globale tout en haut, bien séparée
  annotate(
    "text",
    x        = 2,
    y        = 1.30,
    label    = pval_label,
    size     = 3.8,
    color    = "grey30",
    fontface = "italic"
  ) +
  
  labs(
    title    = "Graft survival probability at 5 years",
    subtitle = "Kaplan-Meier estimate · Pairwise log-rank with Bonferroni correction",
    x        = NULL,
    y        = "Survival probability at 5 years",
    fill     = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0.5, color = "grey10"),
    plot.subtitle      = element_text(size = 10, hjust = 0.5, color = "grey40", margin = margin(b = 12)),
    axis.text.x        = element_text(size = 11, color = "grey15", lineheight = 1.4),
    axis.text.y        = element_text(size = 11, color = "grey30"),
    axis.title.y       = element_text(size = 12, margin = margin(r = 10), color = "grey20"),
    axis.line          = element_line(color = "grey60", linewidth = 0.5),
    axis.ticks         = element_line(color = "grey60"),
    legend.position    = "none",
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(15, 20, 10, 15)
  )

# ---- Brackets pairwise conditionnels ----
# Niveaux d'hauteur bien séparés :
#   bracket adjacent (bb_pos ou pos_neg) : 1.03
#   bracket large (bb_neg)               : 1.14
#   log-rank global                      : 1.30

if (!is.null(label_bb_pos)) {
  p <- p + geom_signif(
    comparisons = list(c(label_map["BB group"], label_map["Positive control"])),
    annotations = label_bb_pos,
    y_position  = 1.03,
    tip_length  = 0.01,
    textsize    = 3.5,
    vjust       = -0.2,
    color       = "grey20"
  )
}

if (!is.null(label_pos_neg)) {
  p <- p + geom_signif(
    comparisons = list(c(label_map["Positive control"], label_map["Negative control"])),
    annotations = label_pos_neg,
    y_position  = 1.03,
    tip_length  = 0.01,
    textsize    = 3.5,
    vjust       = -0.2,
    color       = "grey20"
  )
}

if (!is.null(label_bb_neg)) {
  p <- p + geom_signif(
    comparisons = list(c(label_map["BB group"], label_map["Negative control"])),
    annotations = label_bb_neg,
    y_position  = 1.14,           # toujours AU-DESSUS des brackets adjacents
    tip_length  = 0.01,
    textsize    = 3.5,
    vjust       = -0.2,
    color       = "grey20"
  )
}

print(p)

# Sauvegarde du graphique
ggsave("graft_survival_5yrs_barchart.png", width = 8, height = 6)



#----- Barchart probabilité de survie des greffons dans les 3 groupes à 10 ans -----
s10 <- summary(km_fit_graft_surv, times = 10)

graft_survival_10yrs_df <- data.frame(
  Group                     = gsub("Group=", "", s10$strata),
  Survival_Probability_10yrs = s10$surv,
  lower                     = s10$lower,
  upper                     = s10$upper
)

graft_survival_10yrs_df$Group <- factor(
  graft_survival_10yrs_df$Group,
  levels = c("BB group", "Positive control", "Negative control")
)

# ---- Log-rank global ----
logrank <- survdiff(
  Surv(graft_surv_years, graft_event) ~ Group,
  data = comparison_data
)
pval_global <- pchisq(logrank$chisq,
                      df = length(logrank$n) - 1,
                      lower.tail = FALSE)

format_pval <- function(p) {
  if (p < 0.0001) return("p < 0.0001")
  if (p < 0.001)  return("p < 0.001")
  if (p < 0.01)   return("p < 0.01")
  if (p < 0.05)   return("p < 0.05")
  return(paste0("p = ", round(p, 3)))
}

pval_label <- paste0("Log-rank ", format_pval(pval_global))

# ---- Comparaisons pairwise ----
pw <- pairwise_survdiff(
  Surv(graft_surv_years, graft_event) ~ Group,
  data            = comparison_data,
  p.adjust.method = "bonferroni"
)
pw_matrix <- pw$p.value

get_pw_pval <- function(mat, g1, g2) {
  v <- tryCatch(mat[g2, g1], error = function(e) NA)
  if (is.na(v)) v <- tryCatch(mat[g1, g2], error = function(e) NA)
  v
}

p_bb_pos  <- get_pw_pval(pw_matrix, "BB group", "Positive control")
p_bb_neg  <- get_pw_pval(pw_matrix, "BB group", "Negative control")
p_pos_neg <- get_pw_pval(pw_matrix, "Positive control", "Negative control")

label_pval_only <- function(p) {
  if (is.na(p) || p >= 0.05) return(NULL)
  if (p < 0.0001) return("p < 0.0001")
  if (p < 0.001)  return("p < 0.001")
  if (p < 0.01)   return("p < 0.01")
  return("p < 0.05")
}

label_bb_pos  <- label_pval_only(p_bb_pos)
label_bb_neg  <- label_pval_only(p_bb_neg)
label_pos_neg <- label_pval_only(p_pos_neg)

# ---- Labels axe X avec % en dessous ----
pct_df <- graft_survival_10yrs_df %>%
  arrange(Group) %>%
  mutate(axis_label = paste0(as.character(Group), "\n",
                             scales::percent(Survival_Probability_10yrs, accuracy = 0.1)))

label_map <- setNames(pct_df$axis_label, as.character(pct_df$Group))

graft_survival_10yrs_df$Group_label <- factor(
  label_map[as.character(graft_survival_10yrs_df$Group)],
  levels = label_map[levels(graft_survival_10yrs_df$Group)]
)

# ---- Couleurs ----
group_colors <- c(
  "BB group"         = "#2166AC",
  "Positive control" = "#4DAC26",
  "Negative control" = "#D01C1C"
)
names(group_colors) <- label_map[names(group_colors)]

# ---- Graphique de base ----
p <- ggplot(
  graft_survival_10yrs_df,
  aes(x = Group_label, y = Survival_Probability_10yrs, fill = Group_label)
) +
  geom_col(width = 0.50, color = "white", linewidth = 0.4) +
  
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width     = 0.10,
    linewidth = 0.75,
    color     = "grey25"
  ) +
  
  scale_fill_manual(values = group_colors) +
  
  scale_y_continuous(
    limits = c(0, 1.35),                        # espace suffisant tout en haut
    breaks = seq(0, 1, by = 0.25),
    labels = scales::percent_format(accuracy = 1),
    expand = c(0, 0)
  ) +
  
  # P-value globale tout en haut, bien séparée
  annotate(
    "text",
    x        = 2,
    y        = 1.30,
    label    = pval_label,
    size     = 3.8,
    color    = "grey30",
    fontface = "italic"
  ) +
  
  labs(
    title    = "Graft survival probability at 10 years",
    subtitle = "Kaplan-Meier estimate · Pairwise log-rank with Bonferroni correction",
    x        = NULL,
    y        = "Survival probability at 10 years",
    fill     = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0.5, color = "grey10"),
    plot.subtitle      = element_text(size = 10, hjust = 0.5, color = "grey40", margin = margin(b = 12)),
    axis.text.x        = element_text(size = 11, color = "grey15", lineheight = 1.4),
    axis.text.y        = element_text(size = 11, color = "grey30"),
    axis.title.y       = element_text(size = 12, margin = margin(r = 10), color = "grey20"),
    axis.line          = element_line(color = "grey60", linewidth = 0.5),
    axis.ticks         = element_line(color = "grey60"),
    legend.position    = "none",
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.margin        = margin(15, 20, 10, 15)
  )

# ---- Brackets pairwise conditionnels ----
# Niveaux d'hauteur bien séparés :
#   bracket adjacent (bb_pos ou pos_neg) : 1.03
#   bracket large (bb_neg)               : 1.14
#   log-rank global                      : 1.30

if (!is.null(label_bb_pos)) {
  p <- p + geom_signif(
    comparisons = list(c(label_map["BB group"], label_map["Positive control"])),
    annotations = label_bb_pos,
    y_position  = 1.03,
    tip_length  = 0.01,
    textsize    = 3.5,
    vjust       = -0.2,
    color       = "grey20"
  )
}

if (!is.null(label_pos_neg)) {
  p <- p + geom_signif(
    comparisons = list(c(label_map["Positive control"], label_map["Negative control"])),
    annotations = label_pos_neg,
    y_position  = 1.03,
    tip_length  = 0.01,
    textsize    = 3.5,
    vjust       = -0.2,
    color       = "grey20"
  )
}

if (!is.null(label_bb_neg)) {
  p <- p + geom_signif(
    comparisons = list(c(label_map["BB group"], label_map["Negative control"])),
    annotations = label_bb_neg,
    y_position  = 1.14,           # toujours AU-DESSUS des brackets adjacents
    tip_length  = 0.01,
    textsize    = 3.5,
    vjust       = -0.2,
    color       = "grey20"
  )
}

print(p)

# Sauvegarde du graphique
ggsave("graft_survival_10yrs_barchart.png", width = 8, height = 6)

