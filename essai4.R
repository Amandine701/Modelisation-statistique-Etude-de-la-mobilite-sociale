library(FactoMineR)
library(factoextra)
library(dplyr)
library(cluster)


###############################################################################
#                         Préparation du dataframe                            #
###############################################################################

# Préparation du dataframe 
df_ACM_essai <- df_ACM %>%
#  filter(genre == 2) %>%                              # garder genre = 2
  droplevels() %>%                        #tous les niveaux de facteur qui ne sont plus présents dans les données.
  # normalisation des poids entre 0 et 1
  mutate(poids = poids / mean(poids))  %>% 
  # tout le reste en facteur
  mutate(across(-c(identifiant, poids), as.factor))

# 2. Nettoyage final
df_clean <- df_ACM_essai %>%
  # on enlève toute ligne contenant NA
  filter(
    !is.na(mobilite),
    !is.na(education_pere),
    !is.na(education_mere),
    !is.na(pays),
    !is.na(genre)
  ) 

###############################################################################
#                         Réalisation de l'ACM                                #
###############################################################################

# 3. définition des variables actives
vars_actives <- c("mobilite", "pays")
#"education_pere","education_mere")

# 4. Lancer l’ACM sans erreur
res_mca <- MCA(
  df_clean[, vars_actives],
  row.w = df_clean$poids,
  graph = FALSE
)

###############################################################################
#                      Choix du nombre de clusters                           #
###############################################################################

# Extraire les coordonnées des individus pour la PCA/MCA
coord_mca <- as.data.frame(res_mca$ind$coord)

# Vérifier que les coordonnées sont numériques (dans la plupart des cas elles devraient l'être)
# Si nécessaire, convertis-les en numériques
coord_mca <- as.data.frame(lapply(coord_mca, as.numeric))

library(factoextra)

fviz_nbclust(res_mca$ind$coord[, 1:2], pam, method = "wss") +
  ggtitle("Nombre optimal de clusters") +
  xlab("Nombre de clusters") +
  ylab("Somme des carrés intra-cluster (WSS)")



###############################################################################
#            Réalisation de pam et visualisation                              #
###############################################################################

# PAM sur les coordonnées MCA
pam_res <- pam(res_mca$ind$coord[, 1:3], k = 4) 


# Visualiser les clusters
fviz_cluster(pam_res, geom = "point", ellipse.type = "convex", 
             palette = "jco", ggtheme = theme_minimal())


  # screeplot avec labels, thème épuré et rotation des étiquettes
fviz_screeplot(res_mca, addlabels = TRUE) +
  ggtitle("Inertie expliquée par chaque axe") +
  ylab("Pourcentage d'inertie") +
  xlab("Axes") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(hjust = 0.5)
  )

fviz_nbclust(coord_mca, kmeans, method = "silhouette")



df_clean$cluster <- pam_res$clustering

library(dplyr)

df_clean %>%
  group_by(pays, cluster) %>%
  summarise(n = n()) %>%
  slice_max(order_by = n, n = 1) %>%
  arrange(pays)

df_clean %>%
  count(pays, cluster) %>%
  tidyr::pivot_wider(names_from = cluster, values_from = n, values_fill = 0)

df_clean %>%
  count(pays, cluster) %>%
  ggplot(aes(x = reorder(pays, -n), y = n, fill = as.factor(cluster))) +
  geom_col() +
  coord_flip() +
  labs(title = "Répartition des clusters par pays", fill = "Cluster") +
  theme_minimal()

df_clean$cluster <- pam_res$clustering

library(dplyr)

# Nombre d'individus par pays
total_par_pays <- df_clean %>%
  group_by(pays) %>%
  summarise(total = n())

# Nombre d'individus par pays et par cluster
par_pays_cluster <- df_clean %>%
  group_by(pays, cluster) %>%
  summarise(n = n(), .groups = "drop")

# Joindre pour obtenir les pourcentages
pourcent_par_pays_cluster <- par_pays_cluster %>%
  left_join(total_par_pays, by = "pays") %>%
  mutate(pourcentage = 100 * n / total) %>%
  arrange(pays, cluster)

library(ggplot2)

ggplot(pourcent_par_pays_cluster, aes(x = reorder(pays, -pourcentage), y = pourcentage, fill = as.factor(cluster))) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(title = "Pourcentage d’individus par pays et par cluster", y = "Pourcentage", x = "Pays", fill = "Cluster") +
  theme_minimal()


