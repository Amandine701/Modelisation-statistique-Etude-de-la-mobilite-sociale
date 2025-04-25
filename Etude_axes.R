library(factorextra)
library(ggplot2)
library(FactoMineR)
library(dplyr)
library(ggrepel)
library(dplyr)
library(tidyr)


################################################################################
#      Représentation des variables dans l'espace factoriel                    #
################################################################################

# 2. Extraire et préparer les coords des variables actives
var_coord <- as.data.frame(res_acm$var$coord[, 1:2])
colnames(var_coord) <- c("Dim1","Dim2")
var_coord$Name <- rownames(res_acm$var$coord)
var_coord$Type <- "Active"

# 3. Extraire et préparer les coords des variables sup.
sup_coord <- as.data.frame(res_acm$quali.sup$coord[, 1:2])
colnames(sup_coord) <- c("Dim1","Dim2")
sup_coord$Name <- rownames(res_acm$quali.sup$coord)
sup_coord$Type <- "Supplémentaire"

# 4. Calculer le facteur d'étirement pour détailler les suppl.
ran_var1 <- diff(range(var_coord$Dim1))
ran_sup1 <- diff(range(sup_coord$Dim1))
ran_var2 <- diff(range(var_coord$Dim2))
ran_sup2 <- diff(range(sup_coord$Dim2))
alpha   <- min(ran_var1/ran_sup1, ran_var2/ran_sup2, na.rm = TRUE) * 1.2

sup_coord <- sup_coord %>%
  mutate(
    Dim1 = Dim1 * alpha,
    Dim2 = Dim2 * alpha
  )

# 5. Fusionner
all_coord <- bind_rows(var_coord, sup_coord)

# 6. Tracer
ggplot(all_coord, aes(x = Dim1, y = Dim2, color = Type)) +
  geom_point(aes(size = Type), alpha = 0.7) +
  geom_text_repel(aes(label = Name),
                  size = ifelse(all_coord$Type == "Active", 3.5, 3),
                  max.overlaps = 50) +
  scale_color_manual(values = c("Active" = "black", "Supplémentaire" = "blue")) +
  scale_size_manual(values = c("Active" = 2, "Supplémentaire" = 3)) +
  labs(
    title = "Modalités actives et supplémentaires dans l'espace factoriel",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "",
    size  = "",
    caption = "Note : Les variables actives sont définies sous la forme CSP de l'enquêtée_CSP de la mère_genre."
  ) +
  theme_minimal()
   
################################################################################
#        Corrélation des différents axes avec les caractéristiques             #
#                      socio-économiques des pays                              #
################################################################################


# 1. Pourcentage des modalités de csp par pays
csp_pourcents <- df_clean %>%
  group_by(pays, csp) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pays) %>%
  mutate(pourcent = 100 * n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = csp, values_from = pourcent, values_fill = 0, names_prefix = "csp_")

# 2. Pourcentage des modalités de education
edu_pourcents <- df_clean %>%
  group_by(pays, education) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pays) %>%
  mutate(pourcent = 100 * n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = education, values_from = pourcent, values_fill = 0, names_prefix = "edu_")

# 3. education_pere
edu_pere_pourcents <- df_clean %>%
  group_by(pays, education_pere) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pays) %>%
  mutate(pourcent = 100 * n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = education_pere, values_from = pourcent, values_fill = 0, names_prefix = "edu_pere_")

# 4. education_mere
edu_mere_pourcents <- df_clean %>%
  group_by(pays, education_mere) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(pays) %>%
  mutate(pourcent = 100 * n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = education_mere, values_from = pourcent, values_fill = 0, names_prefix = "edu_mere_")

# 5. Joindre tous les résultats par pays
df_pourcentages_par_pays <- csp_pourcents %>%
  full_join(edu_pourcents, by = "pays") %>%
  full_join(edu_pere_pourcents, by = "pays") %>%
  full_join(edu_mere_pourcents, by = "pays")

# Voir le résultat
print(df_pourcentages_par_pays)

         