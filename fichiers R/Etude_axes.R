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

#  Extraire et préparer les coords des variables actives
var_coord <- as.data.frame(res_acm$var$coord[, 1:2])
colnames(var_coord) <- c("Dim1","Dim2")
var_coord$Name <- rownames(res_acm$var$coord)
var_coord$Type <- "Active"

#  Extraire et préparer les coords des variables sup.
sup_coord <- as.data.frame(res_acm$quali.sup$coord[, 1:2])
colnames(sup_coord) <- c("Dim1","Dim2")
sup_coord$Name <- rownames(res_acm$quali.sup$coord)
sup_coord$Type <- "Supplémentaire"


#  Calculer le facteur d'étirement pour détailler les suppl.
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

#  Fusionner
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
    x = "Dimension 1",
    y = "Dimension 2",
    color = "",
    size  = "",
) +
  theme_minimal()
   
