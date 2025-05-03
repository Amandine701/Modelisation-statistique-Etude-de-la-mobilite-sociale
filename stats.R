library(dplyr)

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

# Proportion pondérée par CSP et par pays
df_prop_pays <- df_ACM %>%
  group_by(pays, csp_avec_A) %>%
  summarise(Proportion = sum(poids, na.rm = TRUE) / sum(df_ACM$poids[df_ACM$pays == unique(pays)], na.rm = TRUE), .groups = "drop")

# Proportion moyenne pondérée par CSP (pour tous les pays)
df_prop_global <- df_ACM %>%
  group_by(csp_avec_A) %>%
  summarise(Pop_Proportion = sum(poids, na.rm = TRUE) / sum(df_ACM$poids, na.rm = TRUE))

# Ajouter la proportion globale à chaque ligne (via jointure sur csp)

df_graph <- df_prop_pays %>%
  left_join(df_prop_global, by = "csp_avec_A") %>%
  left_join(correspondance_pays, by = c("pays" = "code_pays")) %>%
  mutate(pays = pays.y) %>%  # remplacer le code par le nom
  select(-pays.y)

df_graph <- df_graph %>%
  filter(!pays %in% c("Slovenie", "Serbie", "Suède", "Iselande", "Italie", "Croatie"))


#df_graph <- df_graph %>%
#  mutate(
#    csp_avec_A = case_when(
#      csp_avec_A  == "A"   ~ "Agriculteurs",
#      csp_avec_A  == "OQ"  ~ "Ouvriers qualifiés",
#      csp_avec_A  == "ONQ" ~ "Ouvriers non qualifiés",
#      csp_avec_A  == "E"   ~ "Employés",
#      csp_avec_A  == "C"   ~ "Cadres / Prof. intellectuelles",
#      TRUE         ~   csp_avec_A   # laisser les autres inchangés si besoin
#    )
#  )



# Optionnel : pour régler la hauteur automatiquement
y_max <- max(df_graph$Proportion, df_graph$Pop_Proportion, na.rm = TRUE) * 1.1

# Nom de la variable CSP (comme dans ton code)
var <- "csp"

# Création du graphique
library(ggplot2)
aes(x = factor(csp))
graph <- ggplot(df_graph, aes(x = factor(csp_avec_A), y = Proportion)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  geom_point(aes(y = Pop_Proportion), color = "red", size = 0.8) +  # points plus petits
  facet_wrap(~ pays, nrow = 3, scales = "free_x") +  # 3 lignes de pays
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold", size = 10),   # noms de pays
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # CSP sous les barres
    axis.title = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(10, 10, 10, 10)
  )

