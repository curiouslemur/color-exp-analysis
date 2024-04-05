library(readr)


#### MG demography data #####
mg_dem <- read_csv("data/exp1/csv/p3-mdg-dem.csv")

# uniforming the response in the different columns
mg_dem <- mg_dem %>%
  mutate(profession = replace(profession, 
                              profession %in% c("ETUDIANT", "ETUDIANTE", "ETUDIENT", "etudiant", "Etudiente", "Student", "Etudiant", 
                                                "étudiant", "etudiante", "etudient", "Etudiante", "étudiante", "Etudient"), "student")) %>% 
  mutate(gender = replace(gender, gender %in% c("homme", "Homme", "masculin", "Male", "FEMININ", "MASCULIN", "HOMME", "MALE", "Masculin"), "male")) %>%
  mutate(gender = replace(gender, gender %in% c("femme", "FEMME", "femelle", "Femme", "feminin", "Feminin", "femmme"), "female")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd1"), "mdg1")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd2"), "mdg2")) %>%
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd3"), "mdg3"))

write_csv(mg_dem, "data/exp1/csv/clean/mg-dem.csv")

#### MG response data #####
mgdf <- read_csv("data/exp1/csv/p3-mdg.csv")
mgdf <- mgdf %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd1"), "mdg1")) %>% 
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd2"), "mdg2")) %>%
  mutate(expCountry = replace(expCountry, expCountry %in% c("mgd3"), "mdg3"))

write_csv(mgdf, "data/exp1/csv/clean/mg.csv")
