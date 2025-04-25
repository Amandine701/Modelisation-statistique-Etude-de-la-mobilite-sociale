library(FactoMineR)
library(factoextra)
library(dplyr)
library(cluster)
library(ggplot2)

###############################################################################
#                         Préparation du dataframe                            #
###############################################################################

# Préparation du dataframe 
df_ACM_essai <- df_ACM %>%
  filter(genre == 1) %>%                             
  droplevels() %>%                        
  # normalisation des poids entre 0 et 1
  mutate(poids = poids / mean(poids))  %>% 
  # tout le reste en facteur
  mutate(across(-c(identifiant, poids), as.factor)) 

#On filtre les pays avec plus de 100 observations
effectifs <- df_ACM_essai %>%
    count(pays, name = "n") 
  
bons_pays <- effectifs %>%
    filter(n >= 100) %>%
    pull(pays)
  
df_ACM_essai <- df_ACM_essai %>%
    filter(pays %in% bons_pays)

#  Nettoyage final du dataframe
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

# Définition des variables actives
vars_actives <- c("mobilite")
vars_supp <- c("pays", "education")

# Combiner les variables actives et supplémentaires
vars_mca <- c(vars_actives, vars_supp)

# Créer le sous-ensemble de données pour l'ACM
df_mca <- df_clean[, vars_mca]

# Identifier les colonnes supplémentaires dans ce sous-ensemble
cols_supp <- which(names(df_mca) %in% vars_supp)

# Lancer l’ACM proprement
res_acm <- MCA(
  df_mca,
  row.w = df_clean$poids,
  quali.sup = cols_supp,
  graph = FALSE
)




###############################################################################
#                          Choix des axes                                     #
###############################################################################

# screeplot avec labels, thème épuré et rotation des étiquettes
fviz_screeplot(res_acm, addlabels = TRUE) +
  ggtitle("Inertie expliquée par chaque axe") +
  ylab("Pourcentage d'inertie") +
  xlab("Axes") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(hjust = 0.5)
  )











