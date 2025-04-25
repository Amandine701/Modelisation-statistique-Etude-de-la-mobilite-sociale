library(stringr)
library(dplyr)
library(corrplot)

 indicateurs <- tibble::tibble(
   pays = c("Autriche", "Danemark", "Belgique", "Allemagne", "Grèce", "Pologne", 
            "Suède", "Suisse", "Grande-Bretagne", "Italie", "Pays-Bas"),
   gini = c(30.7, 28.3, 26.6, 32.4, 32.9, 28.5, 29.8, 33.7, 32.4, 34.8, 25.7),
   pib_par_hab = c(56033.6, 68453.9, 54700.9, 54343.2, 23400.7, 22056.7,
                   55516.8, 99564.7, 49463.9, 39003.3, 64572.0),
   emploi_femmes = c(54.0, 56.5, 48.1, 54.8, 38.4, 50.5, 59.3, 59.9, 56.0, 37.9, 61.7)
 )
 
 #Calcul de la répartition de la variable mobilite pour les femmes pour chaque pays
 csp_femmes <- df_clean %>%
   filter(genre == 1) %>%
   group_by(pays, csp_avec_A) %>%
   summarise(n = n(), .groups = "drop") %>%
   group_by(pays) %>%
   mutate(prop = 100 * n / sum(n)) %>%
   select(-n) %>%
   tidyr::pivot_wider(names_from = csp_avec_A, values_from = prop, names_prefix = "CSP_")

#Répartition par tranche d'âge par pays pour les femmes
 age_femmes <- df_clean %>%
   filter(genre == 1) %>%
   group_by(pays, age_cat) %>%
   summarise(n = n(), .groups = "drop") %>%
   group_by(pays) %>%
   mutate(prop = 100 * n / sum(n))  %>%
   select(-n) %>%
   tidyr::pivot_wider(names_from = age_cat, values_from = prop, names_prefix = "Age_")

# Répartition par niveau d'éducation par pays pour les femmes
 education_femmes <- df_clean %>%
   filter(genre == 1) %>%
   group_by(pays, education) %>%
   summarise(n = n(), .groups = "drop") %>%
   group_by(pays) %>%
   mutate(prop = 100 * n / sum(n))  %>%
   select(-n) %>%
   tidyr::pivot_wider(names_from = education, values_from = prop, names_prefix = "Edu_")
 
 #Calculs des indicateurs mobilite ascendante, mobilite_descendante, immobilité par pays
 df_mobilite <- df_ACM %>%
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
 
 indicateurs <- indicateurs %>%
   mutate(code_pays = c("AT", "DK", "BE", "DE", "GR", "PL", "SE", "CH", "GB", "IT", "NL"))
 
 indicateurs_final <- indicateurs %>%
   left_join(csp_femmes, by = c("code_pays" = "pays")) %>%
   left_join(age_femmes,   by = c("code_pays" = "pays")) %>%
   left_join(education_femmes, by = c("code_pays" = "pays")) %>%
   left_join(mobilite_par_pays, by = c("pays"))

 
 #Matrice de corrélation
  cor_matrix <- cor(coord_pays[, c("Dim 1", "Dim 2")],
                   coord_pays[, c("pib_par_hab", "emploi_femmes", "gini", "CSP_C",
                                  "CSP_ONQ","CSP_OQ","CSP_PI","Age_>65","Age_14-25",
                                  "Age_26-40","Age_41-65","Edu_1","Edu_2","Edu_3",
                                  "Edu_5","Edu_4" ,"ascendante","déclassement","immobilité")],
                   use = "pairwise.complete.obs")
 
 print(cor_matrix)
 
 corrplot(cor_matrix, is.corr = TRUE, method = "color", 
          addCoef.col = "black", tl.cex = 0.8, number.cex = 0.7)
 
 title("Corrélations entre dimensions de l'ACM et les caractéristiques
       socio-économiques des pays", line = 2)
 
 