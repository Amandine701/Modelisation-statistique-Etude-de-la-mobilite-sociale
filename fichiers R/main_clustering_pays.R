library(cluster)
library(factoextra)
library(mclust)
library(dplyr)

# Extraire les coordonnées MCA et ajouter le pays
coord_mca <- as.data.frame(res_acm$ind$coord[, 1:5])
coord_mca$pays <- df_clean$pays
coord_mca$poids <- df_clean$poids


# Calcul des centroïdes par pays
centroids <- coord_mca %>%
  group_by(pays) %>%
  summarise(
    across(
      starts_with("Dim"),
      ~ weighted.mean(.x, w = poids),
      .names = "{.col}"
    ),
    .groups = "drop"
  )

# Sélection des axes
data_centroids <- centroids %>% select(pays,`Dim 1`, `Dim 2`)

data_centroids <- data_centroids %>%
  rename(code_pays = pays)

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

data_centroids <- data_centroids %>% 
  left_join(correspondance_pays, by="code_pays") %>%
  select(-code_pays)

# ACM proprement dite
# Conversion en data.frame
data_centroids_df <- as.data.frame(data_centroids)

#  noms de ligne = noms de pays
rownames(data_centroids_df) <- data_centroids_df$pays

# Supprimer la colonne "pays" pour ne garder que les dimensions numériques
data_centroids_df <- data_centroids_df[, c("Dim 1", "Dim 2")]

# Calcul de la matrice de distance
mat_dist <- dist(data_centroids_df, method = "euclidean")

#  Classification hiérarchique
arbre <- hclust(mat_dist, method = "ward.D2")

#  Affichage
fviz_dend(arbre, k = 4, rect = TRUE, rect_border = "steelblue", 
          rect_fill = TRUE, main = "Dendrogramme de la classification hiérarchique")


# Tableau récapitulatif

# Créer un data frame avec noms des pays et leur cluster

clusters <- cutree(arbre, k = 4)
classification_pays <- data.frame(
  pays_complet = rownames(data_centroids_df),
  cah_cluster = clusters
)

tableau_clusters <- classification_pays %>%
  group_by(cah_cluster) %>%
  summarise(
    pays = paste(pays_complet, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(cah_cluster)










