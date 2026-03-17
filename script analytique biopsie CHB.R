#----- Script analytique biopsies -----
#Analyse des données anapath selon les 3 groupes 

#----- Groupe BB -----

#Evolution fibrose

data_long <- BB_group %>%
  pivot_longer(
    cols = starts_with("fibrosis_bx"),
    names_to = "biopsy_number",
    values_to = "fibrosis"
  ) %>%
  mutate(
    biopsy_number = as.numeric(gsub("fibrosis_bx", "", biopsy_number))
  ) %>%
  arrange(tx_id, biopsy_number)

ggplot(data_long, aes(x = biopsy_number, y = factor(tx_id, levels = unique(tx_id)), fill = fibrosis)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "white") +
  labs(x = "Biopsies", y = "Patients", fill = "Fibrose")
ggsave("BB_fibrosis_evolution.png", width = 10, height = 6)


#Evolution stéatose
data_long_steat <- BB_group %>%
  pivot_longer(
    cols = starts_with("steatosis_bx"),
    names_to = "biopsy_number",
    values_to = "steatosis"
  ) %>%
  mutate(
    biopsy_number = as.numeric(gsub("steatosis_bx", "", biopsy_number))
  ) %>%
  arrange(tx_id, biopsy_number) %>%
  filter(!is.na(steatosis))

data_long_steat <- data_long_steat %>%
  group_by(tx_id) %>%
  arrange(biopsy_number) %>%
  mutate(
    prev_steatosis = lag(steatosis),
    transition = case_when(
      is.na(prev_steatosis) ~ "baseline",
      prev_steatosis == 0 & steatosis == 1 ~ "apparition",
      prev_steatosis == 1 & steatosis == 0 ~ "disparition",
      prev_steatosis == 1 & steatosis == 1 ~ "persistante",
      prev_steatosis == 0 & steatosis == 0 ~ "absente"
    )
  ) %>%
  ungroup()

ggplot(data_long_steat,
       aes(x = biopsy_number, y = factor(tx_id), fill = factor(steatosis))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "#A8E8FF", "1" = "#F56527"),
                    labels = c("Absente", "Présente")) +
  labs(x = "Biopsies", y = "Patients", fill = "Stéatose")
ggsave("BB_steatosis_evolution.png", width = 10, height = 6)

#Evolution ductopénie avec compte
data_long_ducto <- BB_group %>%
  pivot_longer(
    cols = starts_with("ducto_count_bx"),
    names_to = "biopsy_number",
    values_to = "ducto_count"
  ) %>%
  mutate(
    biopsy_number = as.numeric(gsub("ducto_count_bx", "", biopsy_number))
  ) %>%
  arrange(tx_id, biopsy_number)

ggplot(data_long_ducto, aes(x = biopsy_number, y = factor(tx_id), fill = ducto_count)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "white") +
  labs(x = "Biopsies", y = "Patients", fill = "Ductopénie")
ggsave("BB_ductopenia_evolution.png", width = 10, height = 6)


#Evolution endothélite
data_long_endo <- BB_group %>%
  pivot_longer(
    cols = starts_with("endothelitis_bx"),
    names_to = "biopsy_number",
    values_to = "endothelitis"
  ) %>%
  mutate(
    biopsy_number = as.numeric(gsub("endothelitis_bx", "", biopsy_number))
  ) %>%
  arrange(tx_id, biopsy_number)

ggplot(data_long_endo, aes(x = biopsy_number, y = factor(tx_id), fill = endothelitis)) +
  geom_tile() +
  scale_fill_viridis_c(na.value = "white") +
  labs(x = "Biopsies", y = "Patients", fill = "Endothélite")
ggsave("BB_endothelitis_evolution.png", width = 10, height = 6)

#Evolution HNR
data_long_hnr <- BB_group %>%
  pivot_longer(
    cols = starts_with("hnr_bx"),
    names_to = "biopsy_number",
    values_to = "hnr"
  ) %>%
  mutate(
    biopsy_number = as.numeric(gsub("hnr_bx", "", biopsy_number))
  ) %>%
  arrange(tx_id, biopsy_number)

ggplot(data_long_hnr,
       aes(x = biopsy_number, y = factor(tx_id), fill = factor(hnr))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "#A8E8FF", "1" = "#F56527"),
                    labels = c("Absente", "Présente")) +
  labs(x = "Biopsies", y = "Patients", fill = "HNR")
ggsave("BB_HNR_evolution.png", width = 10, height = 6)

#Evolution thrombose de l'artère hépatique hep_art_thrombosis
data_long_thromb <- BB_group %>%
  pivot_longer(
    cols = starts_with("hep_art_thrombosis_bx"),
    names_to = "biopsy_number",
    values_to = "hep_art_thrombosis"
  ) %>%
  mutate(
    biopsy_number = as.numeric(gsub("hep_art_thrombosis_bx", "", biopsy_number))
  ) %>%
  arrange(tx_id, biopsy_number)

ggplot(data_long_thromb,
       aes(x = biopsy_number, y = factor(tx_id), fill = factor(hep_art_thrombosis))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "#A8E8FF", "1" = "#F56527"),
                    labels = c("Absente", "Présente")) +
  labs(x = "Biopsies", y = "Patients", fill = "Thrombose de l'artère hépatique")
ggsave("BB_hep_art_thrombosis_evolution.png", width = 10, height = 6)

