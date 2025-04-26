library(cluster)
library(factoextra)
library(mclust)
library(dplyr)

# 1. Extraire les coordonnées MCA et ajouter le pays
#coord_mca <- as.data.frame(res_acm$ind$coord,"Dim 3"[, 1:3])
#coord_mca$pays <- df_clean$pays
coord_mca <- as.data.frame(res_acm$ind$coord[, 1:5])
coord_mca$pays <- df_clean$pays
coord_mca$poids <- df_clean$poids


# 2. Calcul des centroïdes par pays
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

# Sélectionner les axes que tu veux utiliser
data_centroids <- centroids %>% select(`Dim 1`, `Dim 2`)

# 4. Gaussian Mixture Models (GMM) sur les centroïdes
gmm_result <- Mclust(data_centroids)
centroids$gmm_cluster <- gmm_result$classification

# Nombre de clusters choisi automatiquement par BIC
print(gmm_result$G)  # Nombre de clusters optimal selon GMM

fviz_cluster(list(data = data_centroids, cluster = centroids$gmm_cluster),
             geom = "point", ellipse.type = "convex", main = "Classification des pays avec méthode GMM")

#Dataframe avec classification des pays par clusters
# 5. Joindre les noms complets des pays (si nécessaire)
table_correspondance <- tibble(
  code = c("AT", "CY", "FI", "GR", "IE", "LT", "PL", "SE", "BE", "DE", 
           "FR", "HR", "IS", "NL", "PT", "SI", "CH", "ES", "GB", "HU", 
           "IT", "NO", "RS", "SK","CZ", "DK","IL" , "LU"),
  pays_complet = c("Autriche", "Chypre", "Finlande", "Grèce", "Irlande", 
                   "Lituanie", "Pologne", "Suède", "Belgique", "Allemagne", 
                   "France", "Croatie", "Islande", "Pays-Bas", "Portugal", 
                   "Slovénie", "Suisse", "Espagne", "Grande-Bretagne", 
                   "Hongrie", "Italie", "Norvège", "Serbie", "Slovaquie",
                   "République tchèque","Danemark","Israël", "Luxembourg")
)

# 1. Joindre les noms de pays
classification_pays <- centroids %>%
  left_join(table_correspondance, by = c("pays" = "code")) %>%
  select(pays, pays_complet, gmm_cluster)


classification_pays <- classification_pays %>%
  arrange(gmm_cluster)

tableau_clusters <- classification_pays %>%
  group_by(gmm_cluster) %>%
  summarise(
    pays = paste(pays_complet, collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(gmm_cluster)










# 4. Appliquer PAM avec le nombre optimal (remplacer si besoin)
optimal_k <- 3
set.seed(123)
pam_pays <- pam(centroids[, c("Dim 1", "Dim 2")], k = optimal_k)

centroids$cluster <- pam_pays$clustering



centroids <- centroids %>%
  left_join(table_correspondance, by = c("pays" = "code"))

# 6. Visualisation finale des clusters
fviz_cluster(
  list(data = centroids[, c("Dim 1", "Dim 2")], cluster = centroids$cluster),
  geom = "point",
  label = centroids$pays,
  ellipse.type = "convex",
  ggtheme = theme_minimal()
) +
  ggtitle(paste("Clustering des pays avec PAM (k =", 4, ")"))

clusters_par_pays <- centroids %>%
  group_by(cluster) %>%
  summarise(
    codes = paste(pays, collapse = ", "),
    .groups = "drop"
  )

print(clusters_par_pays)




# 3. PAM sur les centroïdes
set.seed(123)
pam_pays <- pam(centroids[, c("Dim 1","Dim 2")], k = 3)

centroids$cluster <- pam_pays$clustering

# 4. Visualisation
fviz_cluster(
  list(
    data    = centroids[, c("Dim 1", "Dim 2")],
    cluster = centroids$cluster
  ),
  geom         = "point",
  label        = centroids$pays,
  ellipse.type = "convex",
  ggtheme      = theme_minimal()
) +
  ggtitle("Cluster des pays selon leurs centroïdes MCA")


# Créer une table de correspondance avec les codes pays et les noms complets
table_correspondance <- tibble(
  code = c("AT", "CY", "FI", "GR", "IE", "LT", "PL", "SE", "BE", "DE", 
           "FR", "HR", "IS", "NL", "PT", "SI", "CH", "ES", "GB", "HU", 
           "IT", "NO", "RS", "SK","DK","IL"),
  pays_complet = c("Autriche", "Chypre", "Finlande", "Grèce", "Irlande", 
                   "Lituanie", "Pologne", "Suède", "Belgique", "Allemagne", 
                   "France", "Croatie", "Islande", "Pays-Bas", "Portugal", 
                   "Slovénie", "Suisse", "Espagne", "Grande-Bretagne", 
                   "Hongrie", "Italie", "Norvège", "Serbie", "Slovaquie",
                   "Danemark","Israël")
)

# Joindre la table de correspondance avec le dataframe centroids pour avoir les noms complets
centroids_with_names <- centroids %>%
  left_join(table_correspondance, by = c("pays" = "code"))

# Afficher les clusters avec les noms complets des pays
table_clusters_with_names <- centroids_with_names %>%
  select(pays_complet, cluster) %>%
  arrange(cluster)

# Afficher la table des pays avec leurs clusters et noms complets
print(table_clusters_with_names)

