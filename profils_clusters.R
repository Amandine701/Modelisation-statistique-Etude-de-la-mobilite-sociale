library(dplyr)
library(tidyr)
library(stringr)

################################################################################
#           Tableau corrélation avec  variable active                          #
###############################################################################

#. Préparer le tableau avec toutes les variables (dim1, dim2 + indicateurs)
mobilite_pourcents <- df_clean %>%
  group_by(pays, mobilite) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pays) %>%
  mutate(pourcent = 100 * n / sum(n)) %>%
  select(-n) %>%
  tidyr::pivot_wider(
    names_from = mobilite,
    values_from = pourcent,
    values_fill = 0,
    names_prefix = "mob_"
  )

pays_cluster <- classification_pays %>%
  select(pays_complet, cah_cluster) %>% 
  rename("pays"="pays_complet")
rownames(pays_cluster) <- NULL

correspondance_pays <- tibble::tibble(
  code_pays = c("AL", "AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GB",
                "GE", "GR", "HR", "HU", "IE", "IS", "IL", "IT", "LT", "LU", "LV", "ME", "MK", "NL",
                "NO", "PL", "PT", "RO", "RS", "RU", "SE", "SI", "SK", "TR", "UA", "XK"),
  pays = c("Albanie", "Autriche", "Belgique", "Bulgarie", "Suisse", "Chypre", "Tchéquie", "Allemagne",
           "Danemark", "Estonie", "Espagne", "Finlande", "France", "Grande-Bretagne", "Georgie", "Grèce",
           "Croatie", "Hongrie", "Irelande", "Iselande", "Israel", "Italie", "Lituanie", "Luxembourg",
           "Lettonie", "Monténégro", "Macédoine du Nord", "Pays-Bas", "Norvège", "Pologne", "Portugal",
           "Roumanie", "Serbie", "Russie", "Suède", "Slovenie", "Slovaquie", "Turquie",
           "Ukraine", "Kosovo")
)

mobilite_pourcents <- mobilite_pourcents %>%
  rename(code_pays = pays)
mobilite_pourcents <- mobilite_pourcents %>%
  left_join(correspondance_pays, by = "code_pays")
mobilite_pourcents <- mobilite_pourcents %>%
  ungroup() %>%
  select(-code_pays) %>%
  rename(pays = pays)


mobilite_cluster <- mobilite_pourcents %>%
  left_join(pays_cluster, by = c("pays" ))


# Moyenne par cluster
mobilite_summary <- mobilite_cluster %>%
  group_by(cah_cluster) %>%
  summarise(across(starts_with("mob_"), \(x) mean(x, na.rm = TRUE))) %>%
  pivot_longer(-cah_cluster, names_to = "modality", values_to = "prop")


# Moyenne globale
mobilite_global <- mobilite_cluster %>%
  summarise(across(starts_with("mob_"), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "modality", values_to = "global_prop")

# Fusion
table_full <- mobilite_summary %>%
  left_join(mobilite_global, by = "modality") %>%
  mutate(color_prop = case_when(
    prop > global_prop ~ paste0("\\textcolor{blue}{", round(prop), "}"),
    prop <= 5 ~ paste0("\\textcolor{gray}{", round(prop), "}"),
    TRUE ~ paste0("\\textcolor{pink}{", round(prop), "}")
  ))

table_latex <- table_full %>%
  select(cah_cluster, modality, color_prop) %>%
  pivot_wider(names_from = cah_cluster, values_from = color_prop) %>%
  left_join(mobilite_global, by = "modality") %>%
  mutate(global_prop = paste0("\\textbf{", round(global_prop), "}")) %>%
  select(modality, global_prop, everything())


latex_rows <- apply(table_latex, 1, function(row) {
  paste(
    row["modality"], "&", row["global_prop"], "&",
    row["1"], "&", row["2"], "&", row["3"], "&", row["4"], "&", "\\\\"
  )
})

latex_table <- c(
  "\\begin{table}[H]",
  "\\footnotesize",
  "\\begin{tabular}{p{6cm}|c|cccc}",
  "\\multicolumn{1}{c}{} & \\textbf{Ensemble} & Cluster 1 & Cluster 2 & Cluster 3 & Cluster 4\\\\",
  latex_rows,
  "\\end{tabular}",
  "\\caption{Répartition des types de mobilité par cluster.\\newline \\textit{Note de lecture : Les valeurs supérieures (resp. inférieures) à la valeur globale sont en bleu (resp. en rose). Les valeurs inférieures ou égales à 5\\% sont en gris.}}",
  "\\label{tab:mob_clusters}",
  "\\end{table}"
)

cat(latex_table, sep = "\n")


################################################################################
#           Tableau corrélation avec les caractéristiques socio-éco            #
################################################################################

# Joindre les données socio-éco avec les clusters
df_pourcentages_par_pays  <- df_pourcentages_par_pays %>%
  rename("code_pays"="pays") %>%
  left_join(correspondance_pays, by = "code_pays")

socioeco_cluster <- df_pourcentages_par_pays %>%
  select(-starts_with("pays.")) %>%  # Optionnel, pour éviter les doublons s'il y a pays.y
  left_join(pays_cluster, by = "pays")

# Moyenne par cluster
socioeco_summary <- socioeco_cluster %>%
  group_by(cah_cluster) %>%
  summarise(across(
    all_of(setdiff(names(select(., where(is.numeric))), "cah_cluster")),
    ~mean(.x, na.rm = TRUE)
  )) %>%
  pivot_longer(-cah_cluster, names_to = "variable", values_to = "moyenne_cluster")


#Moyenne pour l'ensemble des clusters
socioeco_global <- socioeco_cluster %>%
  ungroup() %>%  # <- très important
  select(where(is.numeric), -cah_cluster) %>%  # sélectionne uniquement les colonnes numériques
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "moyenne_globale")

table_socioeco <- socioeco_summary %>%
  left_join(socioeco_global, by = "variable") %>%
  mutate(color_val = case_when(
    moyenne_cluster > moyenne_globale ~ paste0("\\textcolor{blue}{", round(moyenne_cluster), "}"),
    moyenne_cluster <= 5 ~ paste0("\\textcolor{gray}{", round(moyenne_cluster), "}"),
    TRUE ~ paste0("\\textcolor{pink}{", round(moyenne_cluster), "}")
  ))

# Génère les lignes LaTeX une par une
latex_rows_se <- apply(table_latex_se, 1, function(row) {
  paste(
    row["variable"], "&", row["moyenne_globale"], "&",
    row["1"], "&", row["2"], "&", row["3"], "&", row["4"], "\\\\"
  )
})

# Crée le tableau complet
table_latex_se <- table_socioeco %>%
  select(cah_cluster, variable, color_val) %>%
  pivot_wider(names_from = cah_cluster, values_from = color_val) %>%
  left_join(socioeco_global, by = "variable") %>%
  mutate(moyenne_globale = paste0("\\textbf{", round(moyenne_globale), "}")) %>%
  select(variable, moyenne_globale, everything())

# Trouver dynamiquement les noms de colonnes des clusters (ex: "1", "2", "3", "4")
cluster_cols <- setdiff(colnames(table_latex_se), c("variable", "moyenne_globale"))

# Générer les lignes LaTeX
latex_rows_se <- apply(table_latex_se, 1, function(row) {
  paste(
    row["variable"], "&", row["moyenne_globale"], "&",
    paste(row[cluster_cols], collapse = " & "), "\\\\"
  )
})

latex_table_se <- c(
  "\\begin{table}[H]",
  "\\footnotesize",
  paste0("\\begin{tabular}{p{5cm}|c|", paste(rep("c", length(cluster_cols)), collapse = ""), "}"),
  paste0("\\multicolumn{1}{c}{} & \\textbf{Ensemble} & ", paste0("Cluster ", cluster_cols, collapse = " & "), " \\\\"),
  "\\hline",
  latex_rows_se,
  "\\end{tabular}",
  "\\caption{Comparaison des moyennes des variables socio-économiques par cluster.\\newline \\textit{Note : En bleu les valeurs supérieures à la moyenne globale, en rose les inférieures, en gris celles ≤ 5\\%.}}",
  "\\label{tab:socioeco_clusters}",
  "\\end{table}"
)

cat(latex_table_se, sep = "\n")

latex_table_se <- c(
  "\\begin{table}[H]",
  "\\footnotesize",
  "\\begin{tabular}{p{5cm}|c|cccc}",
  "\\multicolumn{1}{c}{} & \\textbf{Ensemble} & Cluster 1 & Cluster 2 & Cluster 3 & Cluster 4 \\\\",
  "\\hline",
  latex_rows_se,
  "\\end{tabular}",
  "\\caption{Comparaison des moyennes des variables socio-économiques par cluster.\\newline \\textit{Note : En bleu les valeurs supérieures à la moyenne globale, en rose les inférieures, en gris celles ≤ 5\\%.}}",
  "\\label{tab:socioeco_clusters}",
  "\\end{table}"
)

cat(latex_table_se, sep = "\n")


