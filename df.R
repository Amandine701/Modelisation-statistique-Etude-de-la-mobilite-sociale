library(dplyr)
library(stringr)

####################A suppr pour faciliter select ##############################


# "C"= Professions de cadres et professions libérales
#"PI" = Professions intermédiaires
# "OQ"= "Ouvriers ou employés qualifiés"
# A= Agriculteurs, pêcheurs
# ONQ "Ouvriers ou employés non qualifiés"

################################################################################
#                         Choix de l'édition                                   #
################################################################################

df_summary <- ESS_Theme_Socio_demographics %>%
  group_by(edition) %>%
  summarise(
    nb_lignes = n(),
    nb_idno_uniques = n_distinct(idno),
    na_iscoco = sum(is.na(iscoco)),
    na_occf14 = sum(is.na(occf14)),
    na_occm14 = sum(is.na(occm14))
  ) %>%
  arrange(edition)

print(df_summary)

################################################################################
#     Création un sous-dataframe avec sélection des colonnes et renommage.     #
################################################################################

# Table de correspondance variables/ noms de variables explicites
table_cores <- tibble(
  anciens_noms = c("idno","gndr", "cntry",  "anweight", "iscoco", "occf14", "occm14"),
  nouveaux_noms = c("identifiant","genre", "pays",  "poids", "csp", "csp_père", "csp_mère")
)

df_ACM_1 <- ESS_Theme_Socio_demographics %>%
  select(idno, cntry, anweight, iscoco, occf14, occm14, edition) %>%
  filter(edition==6.7)

df_ACM_2 <- ESS_Theme_Personal_and_household_characteristics%>%
  select(idno,gndr, edition)%>%
  filter(edition==6.7)

#On regroupe les données par identifiant répondant
df_ACM_1_unique <- df_ACM_1 %>%
  group_by(idno) %>%
  summarise(across(everything(), ~first(na.omit(.))))

df_ACM_2_unique <- df_ACM_2 %>%
  group_by(idno) %>%
  summarise(across(everything(), ~first(na.omit(.))))

df_ACM <- inner_join(df_ACM_1_unique, df_ACM_2_unique, by = "idno")%>%
  select(-edition.x, -edition.y)

# Fonction pour renommer les colonnes en utilisant table_cores
rename_columns <- function(df, table_cores) {
  rename_vector <- setNames( table_cores$anciens_noms, table_cores$nouveaux_noms)
  df %>% rename(!!!rename_vector)
}

df_ACM <- rename_columns(df_ACM, table_cores)

#Retraitement des variables occupation pour ne conserver que le niveau agrégé
#Supression des lignes inutilisables

df_ACM <- df_ACM %>%
  filter(
    !csp %in% c(66666, 77777, 88888, 99999),  # toujours supprimer ces valeurs
    !(genre == 2 & csp_père %in% c(66, 77, 88, 99)),  # condition spécifique au père
    !(genre == 1 & csp_mère %in% c(66, 77, 88, 99))   # condition spécifique à la mère
  ) %>%
  mutate(csp = str_extract(as.character(csp), "^\\d"))  # extraire le 1er chiffre


################################################################################
# Regroupement en 6 grandes CSP                                               #
###############################################################################

# Dictionnaire de correspondance
cores_enquete <- c(
  "1"= "c",
  "2"= "C",
  "3"= "PI",
  "4"= "C",
  "5"= "OQ",
  "6"= "A",
  "7"= "OQ",
  "8"= "OQ",
  "9"= "ONQ"
)

cores_parents <- c(
  "1"=  "C",
  "2"= "C",
  "3"= "PI",
  "4"= "C",
  "5"= "OQ ",
  "6"= "OQ" ,
  "7"=  "ONQ",
  "8"= "PI"
)

df_ACM <- df_ACM %>%
  mutate(csp_père = recode(as.character(csp_père), !!!cores_parents))%>%
  mutate(csp_mère = recode(as.character(csp_mère), !!!cores_parents))%>%
  mutate(csp = recode(as.character(csp), !!!cores_enquete))

################################################################################
#                 On filtre par genre et on crée la variable mobilite          #
################################################################################

df_ACM <- df_ACM %>%
  mutate(
    # Nettoyage des espaces dans les variables texte
    csp = str_trim(csp),
    csp_père = str_trim(csp_père),
    csp_mère = str_trim(csp_mère),
    
    # Création de la variable conditionnelle
    mobilite = if_else(
      genre == 2,
      paste(csp, csp_père, "M", sep = "_"),
      paste(csp, csp_mère, "F", sep = "_")
    )
  )


write.csv(df_ACM, "~/work/Modelisation-statistique-Etude-de-la-mobilite-sociale/df_ACM.csv", row.names = FALSE)




