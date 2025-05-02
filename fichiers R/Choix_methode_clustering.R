library(cluster)
library(factoextra)
library(mclust)
library(dplyr)

# ----------------------------------------
# 1. Calcul des centroïdes pondérés
centroids <- coord_mca %>%
  group_by(pays) %>%
  summarise(
    across(starts_with("Dim"), ~ weighted.mean(.x, w = poids)),
    .groups = "drop"
  )

# Sélectionner les axes que tu veux utiliser
data_centroids <- centroids %>% select(`Dim 1`, `Dim 2`)

# ----------------------------------------
# 2. PAM sur les centroïdes
set.seed(123)
pam_result <- pam(data_centroids, k = 4)  # Choisis k provisoirement
centroids$pam_cluster <- pam_result$clustering

# ----------------------------------------
# 3. Clustering hiérarchique (CAH) sur les centroïdes
hc_result <- hclust(dist(data_centroids), method = "ward.D2")
centroids$cah_cluster <- cutree(hc_result, k = 4)


# Afficher le dendrogramme de la CAH
fviz_dend(hc_result, k = 4, rect = TRUE, rect_border = "steelblue", 
          rect_fill = TRUE, main = "")


# ----------------------------------------
# 4. Gaussian Mixture Models (GMM) sur les centroïdes
gmm_result <- Mclust(data_centroids)
centroids$gmm_cluster <- gmm_result$classification

# Nombre de clusters choisi automatiquement par BIC
print(gmm_result$G)  # Nombre de clusters optimal selon GMM

# ----------------------------------------
# 5. Visualiser les 3 résultats

# PAM
fviz_cluster(list(data = data_centroids, cluster = centroids$pam_cluster),
             geom = "point", ellipse.type = "convex", main = "")

# CAH
fviz_cluster(list(data = data_centroids, cluster = centroids$cah_cluster),
             geom = "point", ellipse.type = "convex", main = "")

# GMM
fviz_cluster(list(data = data_centroids, cluster = centroids$gmm_cluster),
             geom = "point", ellipse.type = "convex", main = "")


# ----------------------------------------

# Dataframe avec les classifications pour chaque méthode
classification_df <- centroids %>%
  select(pays, pam_cluster, cah_cluster, gmm_cluster)

# Afficher le dataframe des classifications
print(classification_df)
