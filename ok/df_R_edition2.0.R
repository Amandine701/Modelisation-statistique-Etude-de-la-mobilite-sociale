library(dplyr)
library(stringr)

####################A suppr pour faciliter select ##############################


# "C"= Professions de cadres et professions libérales
#"PI" = Professions intermédiaires
# "OQ"= "Ouvriers ou employés qualifiés, agriculteurs"
# E = employés
# ONQ "Ouvriers ou employés non qualifiés"
# A Agriculteurs pêcheurs

################################################################################
#                         Choix de l'édition                                   #
################################################################################

df_summary <- ESS_Theme_Socio_demographics %>%
  group_by(edition) %>%
  summarise(
    nb_lignes = n(),
    nb_idno_uniques = n_distinct(idno),
    na_isco08= sum(is.na(isco08)),
    na_occf14b = sum(is.na(occf14b)),
    na_occm14b = sum(is.na(occm14b)),
    education = sum(is.na(eisced)),
    education_pere=sum(is.na(eiscedf)),
    education_mere=sum(is.na(eiscedm)),
  ) %>%
  arrange(edition)

print(df_summary)

################################################################################
#     Création un sous-dataframe avec sélection des colonnes et renommage.     #
################################################################################

# Table de correspondance variables/ noms de variables explicites
table_cores <- tibble(
  anciens_noms = c("idno","gndr", "cntry",  "anweight", "isco08", "occf14b",
                   "occm14b","eisced","eiscedf","eiscedm","agea"),
  nouveaux_noms = c("identifiant","genre", "pays",  "poids", "csp", "csp_père",
                    "csp_mère","education", "education_pere", "education_mere","age")
)

df_ACM_1 <- ESS_Theme_Socio_demographics %>%
  select(idno, cntry, anweight, isco08, occf14b, occm14b, eisced,eiscedf,eiscedm, edition) %>%
  filter(edition==2.0)


df_ACM_2 <- ESS_Theme_Personal_and_household_characteristics%>%
  select(idno,gndr,agea, edition)%>%
  filter(edition==2.0)

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
    !csp %in% c(66666, 77777, 88888, 99999),                          
    !(genre == 1 & (is.na(csp_père) | csp_père %in% c(66, 77, 88, 99))),  # pour les hommes 
    !(genre == 2 & (is.na(csp_mère) | csp_mère %in% c(66, 77, 88, 99)))   # pour les femmes 
  ) %>%
  mutate(csp = str_extract(as.character(csp), "^\\d"))


df_ACM <- df_ACM %>%
  filter(
    age != 999,
    !education       %in% c(55,77,88, 99),
    !education_pere  %in% c(55,77, 88, 99),
    !education_mere  %in% c(55,77, 88, 99)
  )


################################################################################
# Regroupement en 6 grandes CSP                                               #
###############################################################################

# Dictionnaire de correspondance
cores_enquete <- c(
  "1"= "C",
  "2"= "C",
  "3"= "PI",
  "4"= "E",
  "5"= "E",
  "6"= "A",
  "7"= "OQ",
  "8"= "OQ",
  "9"= "ONQ"
)


cores_parents <- c(
  "1"=  "C",
  "2"= "C",
  "3"= "PI",
  "4"= "E",
  "5"= "E",
  "6"= "OQ" ,
  "7"="OQ",
  "8"=  "ONQ",
  "9"= "A"
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
      genre == 1,
      paste(csp, csp_père, "M", sep = "_"),
      paste(csp, csp_mère, "F", sep = "_")
    )
  )


df_ACM <- df_ACM %>%
  mutate(
    age_cat = case_when(
      age >= 14 & age <= 25 ~ "14-25",
      age >= 26 & age <= 40 ~ "26-40",
      age >= 41 & age <= 65 ~ "41-65",
      age > 65             ~ ">65",
      TRUE                 ~ NA_character_
    )
  )


write.csv(df_ACM, "~/work/Modelisation-statistique-Etude-de-la-mobilite-sociale/df_ACM.csv", row.names = FALSE)



