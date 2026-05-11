#----- Script analyse multivariée -----#

comparison_data <- comparison_data |>
  mutate(
   don_cod_simplified = case_when(
     don_cod == "Head trauma" ~ "Head trauma",
     don_cod == "Cerebrovascular accident or stroke" ~ "CVA/Stroke",
     don_cod %in% c("Anoxia", "Other") ~ "Anoxia/Other",
      TRUE ~ NA_character_
    ),
    don_cod_simplified = relevel(
      factor(don_cod_simplified), ref = "Head trauma")
  )

# Vérifier les effectifs
table(comparison_data$don_cod_simplified, useNA = "always")


# ---- SURVIE DU GREFFON ----
cox_greffon_strat <- coxph(
  Surv(graft_surv_years, graft_event) ~
    Group +
    tt(Group) +
    rec_gender +
    don_gender +
    don_cod_simplified +
    strata(tx_period),
  data = comparison_data |> filter(!is.na(don_cod_simplified)),
  tt = function(x, t, ...) model.matrix(~ x * log(t))[, -1]
)

summary(cox_greffon_strat)


# Fonction pour calculer les HR à différents temps
HR_at_time <- function(model, time_points) {
  
  # Extraire les coefficients
  coef_pos  <- coef(model)["GroupPositive control"]
  coef_neg  <- coef(model)["GroupNegative control"]
  coef_pos_t <- coef(model)["tt(Group)xPositive control:log(t)"]
  coef_neg_t <- coef(model)["tt(Group)xNegative control:log(t)"]
  
  results <- data.frame(
    Temps = time_points,
    HR_Positive_control = exp(coef_pos + coef_pos_t * log(time_points)),
    HR_Negative_control = exp(coef_neg + coef_neg_t * log(time_points))
  ) |>
    mutate(across(starts_with("HR"), ~ round(., 3)))
  
  return(results)
}

# Calculer à 1, 5, 10, 15, 20 ans
time_points <- c(1, 5, 10, 15, 20)

cat("=== SURVIE DU GREFFON — HR par rapport au BB group ===\n")
HR_at_time(cox_greffon_strat, time_points)

#Ajout IC à 95% pour les HR à différents temps
HR_at_time_CI <- function(model, time_points, label) {
  
  cat("===", label, "===\n")
  
  coef_pos   <- coef(model)["GroupPositive control"]
  coef_neg   <- coef(model)["GroupNegative control"]
  coef_pos_t <- coef(model)["tt(Group)xPositive control:log(t)"]
  coef_neg_t <- coef(model)["tt(Group)xNegative control:log(t)"]
  
  # Matrice de variance-covariance
  vcov_mat <- vcov(model)
  
  results <- lapply(time_points, function(t) {
    lt <- log(t)
    
    # HR et SE pour Positive control
    hr_pos <- exp(coef_pos + coef_pos_t * lt)
    se_pos <- sqrt(
      vcov_mat["GroupPositive control", "GroupPositive control"] +
        lt^2 * vcov_mat["tt(Group)xPositive control:log(t)", "tt(Group)xPositive control:log(t)"] +
        2 * lt * vcov_mat["GroupPositive control", "tt(Group)xPositive control:log(t)"]
    )
    
    # HR et SE pour Negative control
    hr_neg <- exp(coef_neg + coef_neg_t * lt)
    se_neg <- sqrt(
      vcov_mat["GroupNegative control", "GroupNegative control"] +
        lt^2 * vcov_mat["tt(Group)xNegative control:log(t)", "tt(Group)xNegative control:log(t)"] +
        2 * lt * vcov_mat["GroupNegative control", "tt(Group)xNegative control:log(t)"]
    )
    
    data.frame(
      Temps                = t,
      HR_Positive          = round(hr_pos, 3),
      IC_low_Positive      = round(exp(log(hr_pos) - 1.96 * se_pos), 3),
      IC_high_Positive     = round(exp(log(hr_pos) + 1.96 * se_pos), 3),
      HR_Negative          = round(hr_neg, 3),
      IC_low_Negative      = round(exp(log(hr_neg) - 1.96 * se_neg), 3),
      IC_high_Negative     = round(exp(log(hr_neg) + 1.96 * se_neg), 3)
    )
  })
  
  print(do.call(rbind, results))
  cat("\n")
}

# Lancer les deux analyses
HR_at_time_CI(cox_greffon_strat, c(1, 5, 10, 15, 20), "SURVIE DU GREFFON")








# ================================================================
# 1. DONNÉES : HR continus sur une grille de temps fine
# ================================================================
time_grid <- seq(0.5, 20, by = 0.1)

vcov_mat  <- vcov(cox_greffon_strat)

coef_pos   <- coef(cox_greffon_strat)["GroupPositive control"]
coef_neg   <- coef(cox_greffon_strat)["GroupNegative control"]
coef_pos_t <- coef(cox_greffon_strat)["tt(Group)xPositive control:log(t)"]
coef_neg_t <- coef(cox_greffon_strat)["tt(Group)xNegative control:log(t)"]

compute_hr_ribbon <- function(t) {
  lt <- log(t)
  
  hr_pos <- exp(coef_pos + coef_pos_t * lt)
  se_pos <- sqrt(
    vcov_mat["GroupPositive control",       "GroupPositive control"] +
      lt^2 * vcov_mat["tt(Group)xPositive control:log(t)", "tt(Group)xPositive control:log(t)"] +
      2*lt * vcov_mat["GroupPositive control", "tt(Group)xPositive control:log(t)"]
  )
  
  hr_neg <- exp(coef_neg + coef_neg_t * lt)
  se_neg <- sqrt(
    vcov_mat["GroupNegative control",       "GroupNegative control"] +
      lt^2 * vcov_mat["tt(Group)xNegative control:log(t)", "tt(Group)xNegative control:log(t)"] +
      2*lt * vcov_mat["GroupNegative control", "tt(Group)xNegative control:log(t)"]
  )
  
  data.frame(
    time     = t,
    group    = c("Positive control", "Negative control"),
    hr       = c(hr_pos, hr_neg),
    hr_low   = c(exp(log(hr_pos) - 1.96*se_pos), exp(log(hr_neg) - 1.96*se_neg)),
    hr_high  = c(exp(log(hr_pos) + 1.96*se_pos), exp(log(hr_neg) + 1.96*se_neg))
  )
}

ribbon_df <- bind_rows(lapply(time_grid, compute_hr_ribbon)) |>
  mutate(group = factor(group, levels = c("Positive control", "Negative control")))

# ================================================================
# 2. DONNÉES : Forest plot aux temps clés
# ================================================================
time_points <- c(1, 5, 10, 15, 20)

forest_df <- bind_rows(lapply(time_points, compute_hr_ribbon)) |>
  mutate(
    group = factor(group, levels = c("Positive control", "Negative control")),
    label = paste0(round(hr, 2),
                   " (", round(hr_low, 2), "–", round(hr_high, 2), ")"),
    time_label = paste0(time, " yr")
  )

# ================================================================
# 3. COULEURS ET FORMES
# ================================================================
pal <- c(
  "Positive control" = "#4DAC26",
  "Negative control" = "#D01C1C"
)

# Formes distinctes : cercle plein vs carré plein
# (lisibles en N&B et différenciables sans couleur)
shapes <- c(
  "Positive control" = 16,   # ● cercle
  "Negative control" = 15    # ■ carré
)

linetypes <- c(
  "Positive control" = "solid",
  "Negative control" = "dashed"   # trait pointillé en plus pour le N&B
)


# ================================================================
# 4. PANNEAU A — avec légende déplacée + formes + linetypes
# ================================================================
p_ribbon <- ggplot(ribbon_df, aes(x = time, color = group,
                                  fill = group, shape = group,
                                  linetype = group)) +
  
  annotate("rect", xmin = 0.5, xmax = 20, ymin = 0, ymax = 1,
           fill = "grey96", alpha = 1) +
  
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  
  geom_ribbon(aes(ymin = hr_low, ymax = hr_high), alpha = 0.15,
              color = NA) +
  
  # Lignes avec linetype différent par groupe
  geom_line(aes(y = hr), linewidth = 1) +
  
  # Points aux temps clés avec forme distincte
  geom_point(
    data = forest_df,
    aes(x = time, y = hr, shape = group),
    size = 3
  ) +
  
  annotate("text", x = 19.5, y = 0.75, label = "Favorable vs BB group",
           size = 3, color = "grey50", hjust = 1, fontface = "italic") +
  annotate("text", x = 19.5, y = 1.30, label = "Unfavorable vs BB group",
           size = 3, color = "grey50", hjust = 1, fontface = "italic") +
  
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes) +
  
  # Fusionner les 4 aesthetics en une seule entrée de légende
  guides(
    color    = guide_legend(override.aes = list(
      shape    = unname(shapes),
      linetype = unname(linetypes),
      fill     = NA
    )),
    fill     = "none",
    shape    = "none",
    linetype = "none"
  ) +
  
  scale_x_continuous(
    breaks = time_points,
    labels = paste0(time_points, " yr"),
    expand = c(0.01, 0)
  ) +
  scale_y_log10(
    breaks = c(0.25, 0.5, 0.75, 1, 1.5, 2, 3),
    labels = c("0.25", "0.50", "0.75", "1.00", "1.50", "2.00", "3.00"),
    limits = c(0.15, 3.5)
  ) +
  
  labs(
    title    = "A — Time-varying hazard ratio for graft loss",
    subtitle = "Cox model with tt() interaction · Stratified by transplant period\nAdjusted for recipient sex, donor sex, and cause of death · Reference: BB group",
    x        = "Time post-transplant",
    y        = "Hazard ratio (log scale)",
    color    = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, color = "grey10"),
    plot.subtitle      = element_text(size = 9, color = "grey40", lineheight = 1.3,
                                      margin = margin(b = 10)),
    axis.text          = element_text(size = 11, color = "grey25"),
    axis.title         = element_text(size = 12, color = "grey20"),
    axis.line          = element_line(color = "grey60"),
    axis.ticks         = element_line(color = "grey60"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
    
    # ---- Légende déplacée en bas à droite, hors des courbes ----
    legend.position    = c(0.82, 0.18),
    legend.background  = element_rect(fill = "white", color = "grey85",
                                      linewidth = 0.4),
    legend.text        = element_text(size = 11),
    legend.key.size    = unit(1.2, "lines"),
    legend.key.width   = unit(1.8, "lines"),   # plus large pour voir le linetype
    plot.margin        = margin(15, 20, 10, 15)
  )

# ================================================================
# 5. PANNEAU B — forest plot avec formes cohérentes
# ================================================================
p_forest <- ggplot(
  forest_df,
  aes(x = hr, y = factor(time, levels = rev(time_points)),
      color = group, shape = group, linetype = group)
) +
  
  geom_vline(xintercept = 1, linetype = "dashed",
             color = "grey40", linewidth = 0.6) +
  
  geom_errorbarh(aes(xmin = hr_low, xmax = hr_high),
                 height = 0.25, linewidth = 0.75,
                 position = position_dodge(width = 0.6)) +
  
  geom_point(size = 3.5,
             position = position_dodge(width = 0.6)) +
  
  geom_text(aes(label = label),
            position = position_dodge(width = 0.6),
            hjust    = -0.1,
            size     = 3,
            color    = "grey20") +
  
  scale_color_manual(values = pal) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = linetypes) +
  
  guides(color = "none", shape = "none", linetype = "none") +
  
  scale_x_log10(
    breaks = c(0.25, 0.5, 1, 2, 4),
    labels = c("0.25", "0.50", "1.00", "2.00", "4.00"),
    limits = c(0.1, 10),
    expand = c(0, 0)
  ) +
  
  scale_y_discrete(labels = paste0(rev(time_points), " yr")) +
  
  labs(
    title = "B — HR at key time points",
    x     = "Hazard ratio (log scale)",
    y     = NULL
  ) +
  
  theme_classic(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, color = "grey10"),
    axis.text          = element_text(size = 11, color = "grey25"),
    axis.title.x       = element_text(size = 12, color = "grey20"),
    axis.line          = element_line(color = "grey60"),
    axis.ticks         = element_line(color = "grey60"),
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.4),
    legend.position    = "none",
    plot.margin        = margin(15, 20, 10, 15)
  )


# ================================================================
# PANNEAU C — Table des effectifs à risque
# ================================================================

# Extraire les n à risque depuis le km_fit déjà calculé
risk_table_raw <- summary(km_fit_graft_surv, times = time_points)

risk_df <- data.frame(
  time  = risk_table_raw$time,
  group = gsub("Group=", "", as.character(risk_table_raw$strata)),
  n_risk   = risk_table_raw$n.risk,
  n_event  = risk_table_raw$n.event,
  n_censor = risk_table_raw$n.censor
) |>
  mutate(
    group = factor(group,
                   levels = c("Positive control", "BB group", "Negative control")),
    label = paste0(n_risk,
                   "\n(", n_event, " ev. / ", n_censor, " cens.)")
  )

p_risk <- ggplot(risk_df,
                 aes(x = time,
                     y = group,
                     color = group)) +
  
  # Fond alterné pour faciliter la lecture ligne par ligne
  annotate("rect",
           xmin = 0.5, xmax = 20.5,
           ymin = 0.5, ymax = 1.5,
           fill = "grey96", alpha = 1) +
  annotate("rect",
           xmin = 0.5, xmax = 20.5,
           ymin = 2.5, ymax = 3.5,
           fill = "grey96", alpha = 1) +
  
  # Effectifs
  geom_text(aes(label = n_risk),
            fontface = "bold",
            size     = 3.8) +
  
  # Sous-label : événements et censurés
  geom_text(aes(label = paste0("(", n_event, " / ", n_censor, ")")),
            nudge_y  = -0.28,
            size     = 2.8,
            color    = "grey45") +
  
  scale_color_manual(
    values = c(
      "BB group"         = "#2166AC",
      "Positive control" = "#4DAC26",
      "Negative control" = "#D01C1C"
    )
  ) +
  
  scale_x_continuous(
    breaks = time_points,
    labels = paste0(time_points, " yr"),
    limits = c(0.5, 20.5),
    expand = c(0, 0)
  ) +
  
  scale_y_discrete(
    limits = c("Negative control", "BB group", "Positive control")
  ) +
  
  labs(
    title = "C — Number at risk  (events / censored)",
    x     = NULL,
    y     = NULL
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, color = "grey10"),
    axis.text.x      = element_text(size = 10, color = "grey30"),
    axis.text.y      = element_text(size = 11, face = "bold", color = "grey15"),
    axis.line        = element_blank(),
    axis.ticks       = element_blank(),
    legend.position  = "none",
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3,
                                      linetype = "dotted"),
    panel.grid.major.y = element_blank(),
    plot.margin      = margin(5, 20, 10, 15)
  )


# ================================================================
# ASSEMBLAGE FINAL avec les 3 panneaux
# ================================================================
p_ribbon / p_forest / p_risk +
  plot_layout(heights = c(2, 1.2, 0.9)) +
  plot_annotation(
    caption = paste0(
      "HR: Hazard ratio · CI: 95% confidence interval · Reference group: BB group\n",
      "● Positive control (solid line)  ■ Negative control (dashed line)\n",
      "Risk table: n at risk (events / censored) at each time point"
    ),
    theme = theme(
      plot.caption = element_text(size = 9, color = "grey45",
                                  hjust = 0, margin = margin(t = 8))
    )
  )
