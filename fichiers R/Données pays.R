library(stringr)
library(dplyr)
library(corrplot)


################################################################################
#                       Indicateurs socio-économiques                          #
################################################################################

indicateurs <- tibble::tibble(
   pays = c("Autriche", "Danemark", "Belgique", "Allemagne", "Grèce", "Pologne", 
            "Suède", "Suisse", "Grande-Bretagne", "Italie", "Pays-Bas", "Suisse", "Chypre",
            "Espagne", "Finlande", "France", "Grèce",  "Hongrie","Irelande","Lituanie",
            "Norvège",  "Portugal", "Slovaquie"),
   gini = c(30.7, 28.3, 26.6, 32.4, 32.9, 28.5, 29.8, 33.7, 32.4, 34.8, 25.7, 33.7, 31.3, 
            33.9, 27.7 , 31.5, 32.9, 29.2, NA,36.7, 27.7,34.6, 24.1),
   pib_par_hab = c(56033.6, 68453.9, 54700.9, 54343.2, 23400.7, 22056.7,
                   55516.8, 99564.7, 49463.9, 39003.3, 64572.0, 99564.7, 36551.4, 
                   33509.0, 52925.7, 44690.9, 23400.7, 22141.9,NA, 27786,
                   87925.1,27331.2,24491.4),
 #  emploi_femmes = c(54.0, 56.5, 48.1, 54.8, 38.4, 50.5, 59.3, 59.9, 56.0, 37.9, 61.7, 62, 62,
 #                    53, 57, 52, 44, 54, NA,58, 62,55,55),
   chômage= c(5.4, 5.6, 5.5, 3.4, 10.1, 2.5,8.5, 4.1, 4.1, 6.8, 3.6, 4.1, 5.6, 
             11.4, 8.3, 7.4, 10.1, 4.4, NA, 7.5,4.0, 6.4, 5.2 ),
 )
 
 
 ################################################################################
 #        Corrélation des différents axes avec les caractéristiques             #
 #                      socio-économiques des pays                              #
 ################################################################################
 
 
 # 1. Pourcentage des modalités de csp par pays
 
 csp_pourcents <- df_clean %>%
   group_by(pays, csp_avec_A) %>%
   summarise(n = n(), .groups = "drop") %>%
   group_by(pays) %>%
   mutate(pourcent = 100 * n / sum(n)) %>%
   select(-n) %>%
   pivot_wider(names_from = csp_avec_A, values_from = pourcent, values_fill = 0, names_prefix = "csp_")
 
 
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
 
 #Répartition par tranche d'âge par pays pour les hommes
 ages <- df_clean %>%
   group_by(pays, age_cat) %>%
   summarise(n = n(), .groups = "drop") %>%
   group_by(pays) %>%
   mutate(prop = 100 * n / sum(n))  %>%
   select(-n) %>%
   tidyr::pivot_wider(names_from = age_cat, values_from = prop, names_prefix = "Age_")
 

 #Calcul de la répartition de la variable mobilite pour les hommes pour chaque pays
 csp_hommes <- df_clean %>%
   filter(genre == 1) %>%
   group_by(pays, csp_avec_A) %>%
   summarise(n = n(), .groups = "drop") %>%
   group_by(pays) %>%
   mutate(prop = 100 * n / sum(n)) %>%
   select(-n) %>%
   tidyr::pivot_wider(names_from = csp_avec_A, values_from = prop, names_prefix = "CSP_")


# Répartition par niveau d'éducation par pays pour les hommes
 education_hommes <- df_clean %>%
   filter(genre == 1) %>%
   group_by(pays, education) %>%
   summarise(n = n(), .groups = "drop") %>%
   group_by(pays) %>%
   mutate(prop = 100 * n / sum(n))  %>%
   select(-n) %>%
   tidyr::pivot_wider(names_from = education, values_from = prop, names_prefix = "Edu_")
 
 #Calculs des indicateurs mobilite ascendante, mobilite_descendante, immobilité par pays
 df_mobilite <- df_clean %>%
   filter(genre==1) %>%
   mutate(
     origine = str_split_fixed(mobilite, "_", 2)[, 1],
     destination_sexe = str_split_fixed(mobilite, "_", 2)[, 2],
     destination = str_split_fixed(destination_sexe, "_", 2)[, 1],
     sexe = str_split_fixed(destination_sexe, "_", 2)[, 2],
     
     type_mobilite = case_when(
       destination == origine ~ "immobilité",
       destination %in% c("C", "PI") & origine %in% c("ONQ", "OQ") ~ "ascendante",
       destination == "C" & origine == "PI" ~ "ascendante",
       destination == "OQ" & origine == "ONQ" ~ "ascendante",
       destination == "ONQ" & origine %in% c("OQ", "C", "PI") ~ "déclassement",
       destination == "OQ" & origine %in% c("C", "PI") ~ "déclassement",
       destination == "PI" & origine == "C" ~ "déclassement",
       TRUE ~ "autre"
     )
   )
 
 mobilite_par_pays <- df_mobilite %>%
   filter(type_mobilite %in% c("ascendante", "immobilité", "déclassement")) %>%
   group_by(pays, type_mobilite) %>%
   summarise(n = n(), .groups = "drop") %>%
   group_by(pays) %>%
   mutate(pourcentage = round(100 * n / sum(n), 1)) %>%
   select(-n) %>%
   tidyr::pivot_wider(names_from = type_mobilite, values_from = pourcentage,
                      values_fill = 0)
 
 #Fusion des différents dataframes
 
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
 
 indicateurs <- indicateurs %>%
   left_join(correspondance_pays, by = "pays")
 
 
 
 #  Joindre tous les résultats par pays
 df_pourcentages_par_pays <- csp_pourcents %>%
   full_join(edu_pourcents, by = "pays") %>%
 #  full_join(edu_pere_pourcents, by = "pays") %>%
 #  full_join(edu_mere_pourcents, by = "pays") %>%
   full_join(mobilite_par_pays, by = "pays")  %>%
   full_join(ages, by = "pays")  %>%
   left_join(indicateurs, by = c("pays" = "code_pays"))
 
   
 #coordonnées factoriels des pays 
 coord_axes <- centroids %>%
   select(pays, `Dim 1`, `Dim 2`)
 

 coord_pays_avec_indic<-centroids %>% 
   left_join(indicateurs_final, by="pays")
 
 df_corr <- coord_axes %>%
   left_join(df_pourcentages_par_pays, by = "pays") %>%
   select(where(is.numeric))  # on garde uniquement les colonnes numériques
 

 
 #Matrice de corrélation
 cor_matrix <- cor(df_corr, use = "pairwise.complete.obs", method = "pearson")
 
 #  Extraire seulement les lignes pour Dim 1 et Dim 2
 cor_sub <- cor_matrix[c("Dim 1", "Dim 2"), ]
 
 #  Affichage visuel : uniquement lignes Dim 1 et Dim 2, toutes les colonnes

corrplot(cor_sub, is.corr = FALSE, method = "color", tl.cex = 0.7, cl.cex = 0.7)

# Séparation de la matrice de corrélation en 2 puis affichage
n_col <- ncol(cor_sub)
mid <- ceiling(n_col / 2)

cor_part1 <- cor_sub[, 1:mid]
cor_part2 <- cor_sub[, (mid + 1):n_col]

par(mfrow = c(1, 2))  # 1 ligne, 2 colonnes

corrplot(
  cor_part1,
  is.corr = FALSE,
  method = "color",
  cl.cex = 0.7,
  tl.cex = 0.7,
  cl.pos = "n",     
  mar = c(1, 1, 2, 1)  # marges resserrées
)

corrplot(
  cor_part2,
  is.corr = FALSE,
  method = "color",
  cl.cex = 0.7,
  tl.cex = 0.7,
#  cl.pos = "r",
  mar = c(1, 1, 2, 1)
)

par(mfrow = c(1, 1))  # réinitialise l'affichage




 
 